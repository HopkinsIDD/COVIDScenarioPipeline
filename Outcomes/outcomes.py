import itertools
import time, random
import warnings
from numba import jit

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from SEIR.utils import config
import pyarrow.parquet
import pyarrow as pa
import pandas as pd
from SEIR import file_paths


def run_delayframe_outcomes(config, in_run_id, in_prefix, in_sim_id, out_run_id, out_prefix, out_sim_id, scenario_outcomes, nsim = 1, n_jobs=1, stoch_traj_flag = True):
    start = time.monotonic()
    in_sim_ids = np.arange(in_sim_id, in_sim_id + nsim)
    out_sim_ids = np.arange(out_sim_id, out_sim_id + nsim)

    parameters = read_parameters_from_config(config, in_run_id, in_prefix, in_sim_ids, scenario_outcomes)

    loaded_values = None
    if (n_jobs == 1) or (nsim == 1):  # run single process for debugging/profiling purposes
        for sim_offset in np.arange(nsim):
            onerun_delayframe_outcomes(in_run_id, in_prefix, in_sim_ids[sim_offset], out_run_id, out_prefix, out_sim_ids[sim_offset], parameters, loaded_values, stoch_traj_flag)
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_delayframe_outcomes,
            itertools.repeat(in_run_id),
            itertools.repeat(in_prefix),
            in_sim_ids,
            itertools.repeat(out_run_id),
            itertools.repeat(out_prefix),
            out_sim_ids,
            itertools.repeat(parameters),
            itertools.repeat(loaded_values),
            itertools.repeat(stoch_traj_flag),
            max_workers=n_jobs
        )

    print(f"""
>> {nsim} outcomes simulations completed in {time.monotonic() - start:.1f} seconds
""")
    return 1


def onerun_delayframe_outcomes_load_hpar(config, in_run_id, in_prefix, in_sim_id, out_run_id, out_prefix, out_sim_id, scenario_outcomes, stoch_traj_flag = True):
    parameters = read_parameters_from_config(config, in_run_id, in_prefix, [in_sim_id], scenario_outcomes)

    loaded_values = pyarrow.parquet.read_table(file_paths.create_file_name(
        in_run_id,
        in_prefix,
        in_sim_id,
        'hpar',
        'parquet'
    )).to_pandas()

    onerun_delayframe_outcomes(in_run_id, in_prefix, in_sim_id, out_run_id, out_prefix, out_sim_id, parameters, loaded_values, stoch_traj_flag)
    return 1


def read_parameters_from_config(config, run_id, prefix, sim_ids, scenario_outcomes):
    # Prepare the probability table:
    # Either mean of probabilities given or from the file... This speeds up a bit the process.
    # However needs an ordered dict, here we're abusing a bit the spec.
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    if (config["outcomes"]["param_from_file"].get()):
        # load a file from the seir model, to know how to filter the provided csv file
        diffI = pd.read_parquet(file_paths.create_file_name(
          run_id,
          prefix,
          sim_ids[0],
          'seir',
          'parquet'
        ))
        diffI = diffI[diffI['comp'] == 'diffI']
        dates = diffI.time
        diffI.drop(['comp'], inplace = True, axis = 1)
        places = diffI.drop(['time'], axis=1).columns

        # Load the actual csv file
        # Load the actual csv file
        branching_file = config["outcomes"]["param_place_file"].as_str()
        branching_data = pa.parquet.read_table(branching_file).to_pandas()
        if ('relative_probability' not in list(branching_data['quantity'])):
            raise ValueError(f"No 'relative_probablity' quantity in {branching_file}, therefor making it useless")

        print('Loaded geoids in loaded relative probablity file:', len(branching_data.geoid.unique()), '', end='')
        branching_data = branching_data[branching_data['geoid'].isin(diffI.drop('time', axis=1).columns)]
        print('Intersect with seir simulation: ', len(branching_data.geoid.unique()), 'keeped')

        if (len(branching_data.geoid.unique()) != diffI.drop('time', axis=1).columns.shape[0]):
            raise ValueError(f"Places in seir input files does not correspond to places in outcome probability file {branching_file}")

    subclasses = ['']
    if config["outcomes"]["subclasses"].exists():
        subclasses = config["outcomes"]["subclasses"].get()

    parameters = {}
    for new_comp in config_outcomes:
        if config_outcomes[new_comp]['source'].exists():
            for subclass in subclasses:
                class_name = new_comp + subclass
                parameters[class_name] = {}
                # Read the config for this compartement
                parameters[class_name]['source'] = config_outcomes[new_comp]['source'].as_str()
                if (parameters[class_name]['source'] != 'incidI'):
                    parameters[class_name]['source'] = parameters[class_name]['source'] + subclass

                parameters[class_name]['probability'] = config_outcomes[new_comp]['probability']['value']

                parameters[class_name]['delay'] = config_outcomes[new_comp]['delay']['value']

                if config_outcomes[new_comp]['duration'].exists():
                    parameters[class_name]['duration'] = config_outcomes[new_comp]['duration']['value']
                    if config_outcomes[new_comp]['duration']['name'].exists():
                        parameters[class_name]['duration_name'] = config_outcomes[new_comp]['duration']['name'].as_str() + subclass
                    else:
                        parameters[class_name]['duration_name'] = new_comp + '_curr' + subclass

                if (config["outcomes"]["param_from_file"].get()):
                    rel_probability = branching_data[(branching_data['source']==parameters[class_name]['source']) &
                                                 (branching_data['outcome']==class_name) &
                                                 (branching_data['quantity']=='relative_probability')].copy(deep=True)
                    if len(rel_probability) > 0:
                        print(f"Using 'param_from_file' for relative probability {parameters[class_name]['source']} -->  {class_name}")
                        # Sort it in case the relative probablity file is misecified
                        rel_probability.geoid = rel_probability.geoid.astype("category")
                        rel_probability.geoid.cat.set_categories(diffI.drop('time', axis=1).columns, inplace=True)
                        rel_probability = rel_probability.sort_values(["geoid"])
                        parameters[class_name]['rel_probability'] = rel_probability['value'].to_numpy()
                    else:
                        print(f"*NOT* Using 'param_from_file' for relative probability {parameters[class_name]['source']} -->  {class_name}")

            # We need to compute sum across classes if there is subclasses
            if (subclasses != ['']):
                parameters[new_comp] = {}
                parameters[new_comp]['sum'] = [new_comp + c for c in subclasses]
                if config_outcomes[new_comp]['duration'].exists():
                    duration_name = new_comp + '_curr'
                    if config_outcomes[new_comp]['duration']['name'].exists():
                        duration_name = config_outcomes[new_comp]['duration']['name'].as_str()
                    parameters[duration_name] = {}
                    parameters[duration_name]['sum'] = [duration_name + c for c in subclasses]

        elif config_outcomes[new_comp]['sum'].exists():
            parameters[new_comp] = {}
            parameters[new_comp]['sum'] = config_outcomes[new_comp]['sum']
        else:
            raise ValueError(f"No 'source' or 'sum' specified for comp {new_comp}")

    return parameters


def onerun_delayframe_outcomes(in_run_id, in_prefix, in_sim_id, out_run_id, out_prefix, out_sim_id, parameters, loaded_values=None, stoch_traj_flag = True):
    # Read files
    diffI, places, dates = read_seir_sim(in_run_id, in_prefix, in_sim_id)
    # Compute outcomes
    #outcomes, hpar = compute_all_delayframe_outcomes(parameters, diffI, places, dates, loaded_values, stoch_traj_flag)
    outcomes, hpar = compute_all_multioutcomes(parameters, diffI, places, dates, loaded_values, stoch_traj_flag)

    # Write output
    write_outcome_sim(outcomes, out_run_id, out_prefix, out_sim_id)
    write_outcome_hpar(hpar, out_run_id, out_prefix, out_sim_id)


def read_seir_sim(run_id, prefix, sim_id):
    diffI = pd.read_parquet(file_paths.create_file_name(
        run_id,
        prefix,
        sim_id,
        'seir',
        'parquet'
    ))
    diffI = diffI[diffI['comp'] == 'diffI']
    dates = diffI.time
    diffI.drop(['comp'], inplace=True, axis=1)
    places = diffI.drop(['time'], axis=1).columns
    return diffI, places, dates


def write_outcome_sim(outcomes, run_id, prefix, sim_id):
    out_df = pa.Table.from_pandas(outcomes, preserve_index=False)
    pa.parquet.write_table(
        out_df,
        file_paths.create_file_name(
            run_id,
            prefix,
            sim_id,
            'hosp',
            'parquet'
        )
    )


def write_outcome_hpar(hpar, run_id, prefix, sim_id):
    out_hpar = pa.Table.from_pandas(hpar, preserve_index=False)
    pa.parquet.write_table(out_hpar,
                           file_paths.create_file_name(
                               run_id,
                               prefix,
                               sim_id,
                               'hpar',
                               'parquet'
                           )
                           )


def compute_all_delayframe_outcomes(parameters, diffI, places, dates, loaded_values=None, stoch_traj_flag = True):
    all_data = {}
    # We store them as numpy matrices. Dimensions is dates X places
    all_data['incidI'] = diffI.drop(['time'], axis=1).to_numpy()#.astype(np.int32)

    hpar = pd.DataFrame(columns=['geoid', 'quantity', 'outcome', 'source', 'value'])

    outcomes = pd.melt(diffI, id_vars='time', value_name='incidI', var_name='geoid')
    for new_comp in parameters:
        if 'source' in parameters[new_comp]:
            # Read the config for this compartment: if a source is specified, we
            # 1. compute incidence from binomial draw
            # 2. compute duration if needed
            source = parameters[new_comp]['source']
            if loaded_values is not None:
                probability = \
                    loaded_values[(loaded_values['quantity'] == 'probability') & (loaded_values['outcome'] == new_comp)
                                  & (loaded_values['source'] == source)]['value'].to_numpy()
                delay = int(
                    np.round(np.mean(
                        loaded_values[(loaded_values['quantity'] == 'delay') & (loaded_values['outcome'] == new_comp)
                                      & (loaded_values['source'] == source)]['value'].to_numpy())))
            else:
                probability = parameters[new_comp]['probability'].as_random_distribution()(size=len(places))
                if 'rel_probability' in parameters[new_comp]:
                    probability = probability * parameters[new_comp]['rel_probability']
                    probability[probability > 1] = 1
                    probability[probability < 0] = 0

                delay = int(np.round(parameters[new_comp]['delay'].as_random_distribution()(size=1)))

            # Create new compartment incidence:
            all_data[new_comp] = np.empty_like(all_data['incidI'])
            # Draw with from source compartment
            if stoch_traj_flag:
                all_data[new_comp] = np.random.binomial(all_data[source].astype(np.int32), probability * np.ones_like(all_data[source]))
            else:
                all_data[new_comp] = all_data[source] *  (probability * np.ones_like(all_data[source]))

            # import matplotlib.pyplot as plt
            # plt.imshow(probability * np.ones_like(all_data[source]))
            # plt.title(np.mean(probability))
            # plt.savefig('P'+new_comp + '|' + source)

            # Shift to account for the delay
            all_data[new_comp] = shift(all_data[new_comp], delay, fill_value=0)
            # Produce a dataframe an merge it
            df = dataframe_from_array(all_data[new_comp], places, dates, new_comp)
            outcomes = pd.merge(outcomes, df)

            hpar = pd.concat([hpar,
                              pd.DataFrame.from_dict(
                                  {'geoid': places,
                                   'quantity': ['probability'] * len(places),
                                   'outcome': [new_comp] * len(places),
                                   'source': [source] * len(places),
                                   'value': probability * np.ones(len(places))}),
                              pd.DataFrame.from_dict(
                                  {'geoid': places,
                                   'quantity': ['delay'] * len(places),
                                   'outcome': [new_comp] * len(places),
                                   'source': [source] * len(places),
                                   'value': delay * np.ones(len(places))})
                              ],
                             axis=0)

            # Make duration
            if 'duration' in parameters[new_comp]:
                if loaded_values is not None:
                    duration = int(
                        np.round(np.mean(loaded_values[
                                     (loaded_values['quantity'] == 'duration') & (loaded_values['outcome'] == new_comp)
                                     & (loaded_values['source'] == source)]['value'].to_numpy())))
                else:
                    duration = int(np.round(parameters[new_comp]['duration'].as_random_distribution()(size=1)))
                all_data[parameters[new_comp]['duration_name']] = np.cumsum(all_data[new_comp], axis=0) - \
                                                                  shift(np.cumsum(all_data[new_comp], axis=0), duration)

                df = dataframe_from_array(all_data[parameters[new_comp]['duration_name']], places,
                                          dates, parameters[new_comp]['duration_name'])
                outcomes = pd.merge(outcomes, df)

                hpar = pd.concat([hpar, pd.DataFrame.from_dict(
                    {'geoid': places,
                     'quantity': ['duration'] * len(places),
                     'outcome': [new_comp] * len(places),
                     'source': [source] * len(places),
                     'value': duration * np.ones(len(places))})],
                                 axis=0)

        elif 'sum' in parameters[new_comp]:
            # Sum all concerned compartment.
            outcomes[new_comp] = outcomes[parameters[new_comp]['sum']].sum(axis=1)

    return outcomes, hpar


def dataframe_from_array(data, places, dates, comp_name):
    """ 
        Produce a dataframe in long form from a numpy matrix of 
    dimensions: dates * places. This dataframe are merged together 
    to produce the final output
    """
    df = pd.DataFrame(data.astype(np.double), columns=places, index=dates)
    df.reset_index(inplace=True)
    df = pd.melt(df, id_vars='time', value_name=comp_name, var_name='geoid')
    return df


""" Compute delay frame based on temporally varying input"""
##
# @function
# @brief Compute delay frame based on temporally varying input
# ## Parameters
# @parameters[new_comp] Parameters from a yaml config in the form of a dictionary
# ```yaml
# outcomes:
#   output_var_name:
#     source: input_var_name
#     ... # finish later
# @source_data numpy matrix of dates x places
# @places Index for the places dimension of source_data
# @dates Index for dates dimension of source_data.  dates should be one day apart a closed interval
# @loaded_values A numpy array of dimensions place x time with values containing the probabilities
def compute_all_multioutcomes(parameters, diffI, places, dates, loaded_values=None, stoch_traj_flag = True):

    all_data = {}
    # We store them as numpy matrices. Dimensions is dates X places
    all_data['incidI'] = diffI.drop(['time'], axis=1).to_numpy()#.astype(np.int32)

    hpar = pd.DataFrame(columns=['geoid', 'quantity', 'outcome', 'source', 'value'])

    outcomes = pd.melt(diffI, id_vars='time', value_name='incidI', var_name='geoid')
    for new_comp in parameters:
        if 'source' in parameters[new_comp]:
            # Read the config for this compartment: if a source is specified, we
            # 1. compute incidence from binomial draw
            # 2. compute duration if needed
            source = parameters[new_comp]['source']

            if loaded_values is not None:
                raise ValueError('NOT IMPLEMENTED')
                ## This may be unnecessary
                probabilities = \
                    loaded_values[
                        (loaded_values['quantity'] == 'probability') &
                        (loaded_values['outcome'] == new_comp) &
                        (loaded_values['source'] == source)
                    ]['value'].to_numpy()
                delays = int(np.round(
                    loaded_values[
                        (loaded_values['quantity'] == 'delay') &
                        (loaded_values['outcome'] == new_comp) &
                        (loaded_values['source'] == source)
                    ]['value'].to_numpy()
                ))
            else:
                probabilities = parameters[new_comp]['probability'].as_random_distribution()(size=len(places)) # one draw per geoid
                probabilities = np.repeat(probabilities[:,np.newaxis], len(dates), axis = 1).T  # duplicate in time
                if 'rel_probability' in parameters[new_comp]:
                    raise ValueError(f"relative probability not yet supported")
                    probabilities = probabilities * parameters[new_comp]['rel_probability']
                    probabilities[probabilities > 1] = 1
                    probabilities[probabilities < 0] = 0

                delays = parameters[new_comp]['delay'].as_random_distribution()(size=len(places)) # one draw per geoid
                delays = np.repeat(delays[:,np.newaxis], len(dates), axis = 1).T  # duplicate in time
                delays = np.round(delays).astype(int)

            # Create new compartment incidence:
            all_data[new_comp] = np.empty_like(all_data['incidI'])
            # Draw with from source compartment
            if stoch_traj_flag:
                all_data[new_comp] = np.random.binomial(all_data[source].astype(np.int32), probabilities)
            else:
                all_data[new_comp] = all_data[source] *  (probability * np.ones_like(all_data[source]))

            # Shift to account for the delay
            ## stoch_delay_flag is whether to use stochastic delays or not
            stoch_delay_flag = False
            all_data[new_comp] = multishift(all_data[new_comp], delays, stoch_delay_flag = stoch_delay_flag)
            # Produce a dataframe an merge it
            df = dataframe_from_array(all_data[new_comp], places, dates, new_comp)
            outcomes = pd.merge(outcomes, df)

            hpar = pd.concat(
                [
                    hpar,
                    pd.DataFrame.from_dict(
                        {'geoid': places,
                        'quantity': ['probability'] * len(places),
                        'outcome': [new_comp] * len(places),
                        'source': [source] * len(places),
                        'value': probabilities[0] * np.ones(len(places))}),
                    pd.DataFrame.from_dict(
                        {'geoid': places,
                        'quantity': ['delay'] * len(places),
                        'outcome': [new_comp] * len(places),
                        'source': [source] * len(places),
                        'value': delays[0] * np.ones(len(places))})
                ],
                axis=0)

            # Make duration
            if 'duration' in parameters[new_comp]:
                if loaded_values is not None:
                    raise ValueError("NOT IMPLEMENTED")
                    durations = int(np.round(
                        loaded_values[
                            (loaded_values['quantity'] == 'duration') &
                            (loaded_values['outcome'] == new_comp) &
                            (loaded_values['source'] == source)
                        ]['value'].to_numpy()
                    ))
                else:
                    durations = parameters[new_comp]['duration'].as_random_distribution()(size=len(places)) # one draw per geoid
                    durations = np.repeat(durations[:,np.newaxis], len(dates), axis = 1).T  # duplicate in time
                    durations = np.round(durations).astype(int)
                    all_data[parameters[new_comp]['duration_name']] = np.cumsum(all_data[new_comp], axis=0) - \
                        multishift(np.cumsum(all_data[new_comp], axis=0), durations, stoch_delay_flag=stoch_delay_flag)

                    df = dataframe_from_array(all_data[parameters[new_comp]['duration_name']], places,
                                            dates, parameters[new_comp]['duration_name'])
                    outcomes = pd.merge(outcomes, df)

                    hpar = pd.concat(
                        [
                            hpar,
                            pd.DataFrame.from_dict(
                                {'geoid': places,
                                'quantity': ['duration'] * len(places),
                                'outcome': [new_comp] * len(places),
                                'source': [source] * len(places),
                                'value': durations[0] * np.ones(len(places))
                                }
                            )
                        ],axis=0)
        elif 'sum' in parameters[new_comp]:
            # Sum all concerned compartment.
            outcomes[new_comp] = outcomes[parameters[new_comp]['sum']].sum(axis=1)

    return outcomes, hpar



""" Quite fast shift implementation, along the first axis, 
    which is date. num is an integer not negative nor zero """
@jit(nopython=True)
def shift(arr, num, fill_value=0):
    if (num == 0):
        return arr
    else:
        result = np.empty_like(arr)
        # if num > 0:
        result[:num] = fill_value
        result[num:] = arr[:-num]
    # elif num < 0:
    #    result[num:] = fill_value
    #    result[:num] = arr[-num:]
    # else:
    #    result[:] = arr
    return result


def multishiftee(arr, shifts, stoch_delay_flag = True):
    """ Shift along first (0) axis """
    result = np.zeros_like(arr)

    if (stoch_delay_flag):
        raise ValueError("NOT SUPPORTED YET")
        #for i, row in reversed(enumerate(np.rows(arr))):
        #    for j,elem in reversed(enumerate(row)):
                ## This function takes in :
                ##  - elem (int > 0)
                ##  - delay (single average delay)
                ## and outputs
                ##  - vector of fixed size where the k element stores # of people who are delayed by k
                #percentages = np.random.multinomial(el<fixed based on delays[i][j]>)
        #        cases = diff(round(cumsum(percentages)*elem))
        #        for k,case in enumerate(cases):
        #            results[i+k][j] = cases[k]
    else:
        for i, row in enumerate(arr):
            for j, elem in enumerate(row):
                if(i + shifts[i][j] < arr.shape[0]):
                    result[i+shifts[i][j]][j] += elem
    return result

@jit(nopython=True, cache=True)
def multishift(arr, shifts, stoch_delay_flag = True):
    """ Shift along first (0) axis """
    result = np.zeros_like(arr)

    if (stoch_delay_flag):
        raise ValueError("NOT SUPPORTED YET")
        #for i, row in reversed(enumerate(np.rows(arr))):
        #    for j,elem in reversed(enumerate(row)):
                ## This function takes in :
                ##  - elem (int > 0)
                ##  - delay (single average delay)
                ## and outputs
                ##  - vector of fixed size where the k element stores # of people who are delayed by k
                #percentages = np.random.multinomial(el<fixed based on delays[i][j]>)
        #        cases = diff(round(cumsum(percentages)*elem))
        #        for k,case in enumerate(cases):
        #            results[i+k][j] = cases[k]
    else:
        for i in range(arr.shape[0]):            # numba nopython does not allow iterating over 2D array
            for j in range(arr.shape[1]):
                if(i + shifts[i,j] < arr.shape[0]):
                    result[i+shifts[i,j], j] += arr[i,j]
    return result