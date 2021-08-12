import itertools
import time, random
import warnings
from numba import jit
import confuse

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from SEIR.utils import config, Timer
import SEIR.NPI as NPI
import pyarrow.parquet
import pyarrow as pa
import pandas as pd
from SEIR import file_paths
from SEIR.setup import _parameter_reduce, npi_load

import logging

logger = logging.getLogger(__name__)


def run_delayframe_outcomes(config, in_sim_id, in_run_id, in_prefix, out_sim_id, out_run_id, out_prefix,
                            scenario_outcomes, nsim=1, n_jobs=1, stoch_traj_flag=True):
    start = time.monotonic()
    in_sim_ids = np.arange(in_sim_id, in_sim_id + nsim)
    out_sim_ids = np.arange(out_sim_id, out_sim_id + nsim)

    parameters, npi_config = read_parameters_from_config(config, in_run_id, in_prefix, in_sim_ids, scenario_outcomes)

    loaded_values = None
    if (n_jobs == 1) or (nsim == 1):  # run single process for debugging/profiling purposes
        for sim_offset in np.arange(nsim):
            onerun_delayframe_outcomes(in_sim_ids[sim_offset], in_run_id, in_prefix, out_sim_ids[sim_offset],
                                       out_run_id, out_prefix, parameters, loaded_values, stoch_traj_flag, npi_config)
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_delayframe_outcomes,
            in_sim_ids,
            itertools.repeat(in_run_id),
            itertools.repeat(in_prefix),
            out_sim_ids,
            itertools.repeat(out_run_id),
            itertools.repeat(out_prefix),
            itertools.repeat(parameters),
            itertools.repeat(loaded_values),
            itertools.repeat(stoch_traj_flag),
            itertools.repeat(npi_config),
            max_workers=n_jobs
        )

    print(f"""
>> {nsim} outcomes simulations completed in {time.monotonic() - start:.1f} seconds
""")
    return 1


def onerun_delayframe_outcomes_load_hpar(config, in_sim_id, in_run_id, in_prefix, out_sim_id, out_run_id, out_prefix,
                                         scenario_outcomes, stoch_traj_flag=True):
    parameters, npi_config = read_parameters_from_config(config, in_run_id, in_prefix, [in_sim_id], scenario_outcomes)

    loaded_values = pyarrow.parquet.read_table(file_paths.create_file_name(
        in_run_id,
        in_prefix,
        in_sim_id,
        'hpar',
        'parquet'
    )).to_pandas()

    if npi_config is not None:
        with Timer('onerun_delayframe_outcomes_load_hpar.NPI'):
            diffI, places, dates = read_seir_sim(in_run_id, in_prefix, in_sim_id)
            npi = NPI.NPIBase.execute(
                npi_config=npi_config[0],
                global_config=npi_config[1],
                geoids=places,
                loaded_df=npi_load(
                    file_paths.create_file_name_without_extension(
                        in_run_id,
                        in_prefix,
                        in_sim_id,
                        "hnpi"), 'parquet')
            )
    else:
        npi = None

    onerun_delayframe_outcomes(in_sim_id, in_run_id, in_prefix, out_sim_id, out_run_id, out_prefix, parameters,
                               loaded_values, stoch_traj_flag, npi)
    return 1


def read_parameters_from_config(config, run_id, prefix, sim_ids, scenario_outcomes):
    # Prepare the probability table:
    # Either mean of probabilities given or from the file... This speeds up a bit the process.
    # However needs an ordered dict, here we're abusing a bit the spec.
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    if config["outcomes"]["param_from_file"].get():
        # load a file from the seir model, to know how to filter the provided csv file
        diffI, places, dates = read_seir_sim(run_id=run_id, prefix=prefix, sim_id=sim_ids[0])

        # Load the actual csv file
        # Load the actual csv file
        branching_file = config["outcomes"]["param_place_file"].as_str()
        branching_data = pa.parquet.read_table(branching_file).to_pandas()
        if 'relative_probability' not in list(branching_data['quantity']):
            raise ValueError(f"No 'relative_probablity' quantity in {branching_file}, therefor making it useless")

        print('Loaded geoids in loaded relative probablity file:', len(branching_data.geoid.unique()), '', end='')
        branching_data = branching_data[branching_data['geoid'].isin(places)]
        print('Intersect with seir simulation: ', len(branching_data.geoid.unique()), 'keeped')

        if len(branching_data.geoid.unique()) != places.shape[0]:
            raise ValueError(
                f"Places in seir input files does not correspond to places in outcome probability file {branching_file}")

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
                src_name = config_outcomes[new_comp]['source'].get()
                if isinstance(src_name, str):
                    if src_name != 'incidI':
                        parameters[class_name]['source'] = src_name + subclass
                    else:
                        parameters[class_name]['source'] = src_name
                else:
                    if subclasses != ['']:
                        raise ValueError("Subclasses not compatible with outcomes ")
                    else:
                        parameters[class_name]['source'] = dict(src_name['incidence'])

                parameters[class_name]['probability'] = config_outcomes[new_comp]['probability']['value']

                parameters[class_name]['delay'] = config_outcomes[new_comp]['delay']['value']

                if config_outcomes[new_comp]['duration'].exists():
                    parameters[class_name]['duration'] = config_outcomes[new_comp]['duration']['value']
                    if config_outcomes[new_comp]['duration']['name'].exists():
                        parameters[class_name]['duration_name'] = config_outcomes[new_comp]['duration'][
                                                                      'name'].as_str() + subclass
                    else:
                        parameters[class_name]['duration_name'] = new_comp + '_curr' + subclass

                if config["outcomes"]["param_from_file"].get():
                    rel_probability = branching_data[#(branching_data['source'] == parameters[class_name]['source']) &
                                                     (branching_data['outcome'] == class_name) &
                                                     (branching_data['quantity'] == 'relative_probability')].copy(
                        deep=True)
                    if len(rel_probability) > 0:
                        print(
                            f"Using 'param_from_file' for relative probability {parameters[class_name]['source']} -->  {class_name}")
                        # Sort it in case the relative probablity file is misecified
                        rel_probability.geoid = rel_probability.geoid.astype("category")
                        rel_probability.geoid.cat.set_categories(diffI.drop('date', axis=1).columns, inplace=True)
                        rel_probability = rel_probability.sort_values(["geoid"])
                        parameters[class_name]['rel_probability'] = rel_probability['value'].to_numpy()
                    else:
                        print(
                            f"*NOT* Using 'param_from_file' for relative probability {parameters[class_name]['source']} -->  {class_name}")

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
            parameters[new_comp]['sum'] = config_outcomes[new_comp]['sum'].get()
        else:
            raise ValueError(f"No 'source' or 'sum' specified for comp {new_comp}")

    npi_config = None
    if config["outcomes"]["interventions"]["settings"][scenario_outcomes].exists():
        npi_config = [config["outcomes"]["interventions"]["settings"][scenario_outcomes], config]

    return parameters, npi_config


def onerun_delayframe_outcomes(in_sim_id, in_run_id, in_prefix, out_sim_id, out_run_id, out_prefix, parameters,
                               loaded_values=None, stoch_traj_flag=True, npi_config=None):
    # Read files
    diffI, places, dates = read_seir_sim(in_run_id, in_prefix, in_sim_id)

    # If a list, then it's just the config from run confing, so build the NPI
    # otherwise it's None (no NPI) or an NPI already loaded
    if isinstance(npi_config, list):
        npi = NPI.NPIBase.execute(npi_config=npi_config[0], global_config=npi_config[1], geoids=places)
    elif npi_config is not None:
        npi = npi_config
    else:
        npi = None

    # Compute outcomes
    # outcomes, hpar = compute_all_delayframe_outcomes(parameters, diffI, places, dates, loaded_values, stoch_traj_flag, npi)
    with Timer('onerun_delayframe_outcomes.compute'):
        outcomes, hpar = compute_all_multioutcomes(parameters, diffI, places, dates, loaded_values, stoch_traj_flag,
                                                   npi)

    with Timer('onerun_delayframe_outcomes.postprocess'):
        # Write output
        write_outcome_sim(outcomes, out_run_id, out_prefix, out_sim_id)
        write_outcome_hpar(hpar, out_run_id, out_prefix, out_sim_id)
        # if npi is not None:
        write_outcome_hnpi(npi, out_run_id, out_prefix, out_sim_id)


def read_seir_sim(run_id, prefix, sim_id):
    diffI = pd.read_parquet(file_paths.create_file_name(
        run_id,
        prefix,
        sim_id,
        'seir',
        'parquet'
    ))

    diffI = diffI[diffI['value_type'] == 'incidence']
    diffI.drop(['value_type'], inplace=True, axis=1)
    dates = diffI['date'].unique()
    todrop = [c for c in diffI.columns if c[:3] == 'mc_']
    todrop.append('date')
    places = diffI.drop(todrop, axis=1).columns
    return diffI, places, dates


def write_outcome_sim(outcomes, run_id, prefix, sim_id):
    outcomes['time'] = outcomes['date']
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


def write_outcome_hnpi(npi, run_id, prefix, sim_id):
    if npi is not None:
        npi.writeReductions(
            file_paths.create_file_name_without_extension(run_id, prefix, sim_id, "hnpi"), "parquet"
        )
    else:
        hnpi = pd.DataFrame(columns=['geoid', 'npi_name', 'start_date', 'end_date', 'parameter', 'reduction'])
        out_hnpi = pa.Table.from_pandas(hnpi, preserve_index=False)
        pa.parquet.write_table(out_hnpi,
                               file_paths.create_file_name(
                                   run_id,
                                   prefix,
                                   sim_id,
                                   'hnpi',
                                   'parquet'
                               )
                               )


def dataframe_from_array(data, places, dates, comp_name):
    """ 
        Produce a dataframe in long form from a numpy matrix of 
    dimensions: dates * places. This dataframe are merged together 
    to produce the final output
    """
    df = pd.DataFrame(data.astype(np.double), columns=places, index=dates)
    df.index.name = 'date'
    df.reset_index(inplace=True)
    df = pd.melt(df, id_vars='date', value_name=comp_name, var_name='geoid')
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
def compute_all_multioutcomes(parameters, diffI, places, dates, loaded_values=None, stoch_traj_flag=True, npi=None):
    hpar = pd.DataFrame(columns=['geoid', 'quantity', 'outcome', 'value'])
    all_data = {}

    outcomes = dataframe_from_array(np.zeros((len(dates), len(places)), dtype=np.int),
                                    places,
                                    dates, 'zeros').drop('zeros',axis=1)

    for new_comp in parameters:
        if 'source' in parameters[new_comp]:
            # Read the config for this compartment: if a source is specified, we
            # 1. compute incidence from binomial draw
            # 2. compute duration if needed
            source_name = parameters[new_comp]['source']
            print(f'doing {new_comp}')
            if source_name == 'incidI' and 'incidI' not in all_data:  # create incidI
                source_array = get_filtered_incidI(diffI, dates, places, {'infection_stage':'I1'})
                all_data['incidI'] = source_array
                outcomes =  pd.merge(outcomes, dataframe_from_array(source_array, places, dates, 'incidI'))
            elif isinstance(source_name, dict):
                source_array = get_filtered_incidI(diffI, dates, places, source_name)
                # we don't keep source in this cases
            else:  # already defined outcomes
                source_array = all_data[source_name]

            if loaded_values is not None:
                ## This may be unnecessary
                probabilities = \
                    loaded_values[
                        (loaded_values['quantity'] == 'probability') &
                        (loaded_values['outcome'] == new_comp) #&
                        # (loaded_values['mc_vaccination_stage'] == 'unvaccinated') &
                        #(loaded_values['source'] == source_name)
                        ]['value'].to_numpy()
                delays = loaded_values[
                    (loaded_values['quantity'] == 'delay') &
                    (loaded_values['outcome'] == new_comp) #&
                    # (loaded_values['mc_vaccination_stage'] == 'unvaccinated') &
                   # (loaded_values['source'] == source_name)
                    ]['value'].to_numpy()
            else:
                probabilities = parameters[new_comp]['probability'].as_random_distribution()(
                    size=len(places))  # one draw per geoid
                if 'rel_probability' in parameters[new_comp]:
                    probabilities = probabilities * parameters[new_comp]['rel_probability']

                delays = parameters[new_comp]['delay'].as_random_distribution()(size=len(places))  # one draw per geoid

            probabilities[probabilities > 1] = 1
            probabilities[probabilities < 0] = 0
            probabilities = np.repeat(probabilities[:, np.newaxis], len(dates), axis=1).T  # duplicate in time
            delays = np.repeat(delays[:, np.newaxis], len(dates), axis=1).T  # duplicate in time
            delays = np.round(delays).astype(int)
            # write hpar before NPI
            hpar = pd.concat(
                [
                    hpar,
                    pd.DataFrame.from_dict(
                        {'geoid': places,
                         # 'mc_vaccination_stage': [p_comp] * len(places),
                         'quantity': ['probability'] * len(places),
                         'outcome': [new_comp] * len(places),
                         #'source': [source_name] * len(places),
                         'value': probabilities[0] * np.ones(len(places))}),
                    pd.DataFrame.from_dict(
                        {'geoid': places,
                         # 'mc_vaccination_stage': [p_comp] * len(places),
                         'quantity': ['delay'] * len(places),
                         'outcome': [new_comp] * len(places),
                         #'source': [source_name] * len(places),
                         'value': delays[0] * np.ones(len(places))})
                ],
                axis=0)
            if npi is not None:
                delays = _parameter_reduce(delays, npi.getReduction(f"{new_comp}::delay".lower()))
                delays = np.round(delays).astype(int)
                probabilities = _parameter_reduce(probabilities, npi.getReduction(f"{new_comp}::probability".lower()))

            # Create new compartment incidence:
            all_data[new_comp] = np.empty_like(source_array)
            # Draw with from source compartment
            if stoch_traj_flag:
                all_data[new_comp] = np.random.binomial(source_array.astype(np.int32),
                                                        probabilities)
            else:
                all_data[new_comp] = source_array * (
                        probabilities * np.ones_like(source_array))

            # Shift to account for the delay
            ## stoch_delay_flag is whether to use stochastic delays or not
            stoch_delay_flag = False
            all_data[new_comp] = multishift(all_data[new_comp], delays,
                                            stoch_delay_flag=stoch_delay_flag)
            # Produce a dataframe an merge it
            df_p = dataframe_from_array(all_data[new_comp], places, dates, new_comp)
            outcomes = pd.merge(outcomes, df_p)

            # Make duration
            if 'duration' in parameters[new_comp]:
                if loaded_values is not None:
                    durations = loaded_values[
                        (loaded_values['quantity'] == 'duration') &
                        (loaded_values['outcome'] == new_comp) #&
                        # (loaded_values['mc_vaccination_stage'] == p_comps[0]) &
                        # (loaded_values['source'] == source_name)
                        ]['value'].to_numpy()
                else:
                    durations = parameters[new_comp]['duration'].as_random_distribution()(
                        size=len(places))  # one draw per geoid
                durations = np.repeat(durations[:, np.newaxis], len(dates), axis=1).T  # duplicate in time
                durations = np.round(durations).astype(int)

                hpar = pd.concat(
                    [
                        hpar,
                        pd.DataFrame.from_dict(
                            {'geoid': places,
                             # 'mc_vaccination_stage': [p_comp] * len(places),
                             'quantity': ['duration'] * len(places),
                             'outcome': [new_comp] * len(places),
                             #'source': [source_name] * len(places),
                             'value': durations[0] * np.ones(len(places))
                             }
                        )
                    ], axis=0)

                if npi is not None:
                    # import matplotlib.pyplot as plt
                    # plt.imshow(durations)
                    # plt.title(durations.mean())
                    # plt.colorbar()
                    # plt.savefig('Dbef'+new_comp + '-' + source)
                    # plt.close()
                    # print(f"{new_comp}-duration".lower(), npi.getReduction(f"{new_comp}-duration".lower()))
                    durations = _parameter_reduce(durations, npi.getReduction(f"{new_comp}::duration".lower()))
                    durations = np.round(durations).astype(int)
                    # plt.imshow(durations)
                    # plt.title(durations.mean())
                    # plt.colorbar()
                    # plt.savefig('Daft'+new_comp + '-' + source)
                    # plt.close()

                # df = pd.DataFrame()
                all_data[parameters[new_comp]['duration_name']] = np.cumsum(all_data[new_comp],
                                                                            axis=0) - \
                                                                  multishift(
                                                                      np.cumsum(all_data[new_comp],
                                                                                axis=0), durations,
                                                                      stoch_delay_flag=stoch_delay_flag)

                df_p = dataframe_from_array(all_data[parameters[new_comp]['duration_name']], places,
                                            dates, parameters[new_comp]['duration_name'])
                # df_p['mc_vaccination_stage'] = p_comp
                # df = pd.concat([df, df_p])
                outcomes = pd.merge(outcomes, df_p)

        elif 'sum' in parameters[new_comp]:
            # Sum all concerned compartment.
            outcomes[new_comp] = outcomes[parameters[new_comp]['sum']].sum(axis=1)

    return outcomes, hpar


def backward_compatibility_incidI(diffI, dates, places):
    # We store them as numpy matrices. Dimensions is dates X places
    incidI_arr = np.zeros((len(dates), len(places)), dtype=np.int)
    mc_vaccination_stages = diffI['mc_vaccination_stage'].unique()
    for p_comp in mc_vaccination_stages:
        incidI = diffI[(diffI['mc_vaccination_stage'] == p_comp) & (diffI['mc_infection_stage'] == 'I1')]

        # additional_mcs = [c for c in incidI.drop(['date', 'mc_vaccination_stage', 'mc_infection_stage', 'mc_name'], axis=1).columns if 'mc_' in c]
        # if not additional_mcs:
        #    incidI = incidI.drop(['date', 'mc_vaccination_stage', 'mc_infection_stage', 'mc_name'], axis=1)
        #    all_data[p_comp]['incidI'] = incidI.to_numpy()
        # else:

        for mcn in incidI['mc_name'].unique():
            new_df = incidI[incidI['mc_name'] == mcn]
            new_df = new_df.drop([c for c in new_df.columns if 'mc_' in c], axis=1)
            new_df = new_df.drop('date', axis=1)
            incidI_arr = incidI_arr + new_df.to_numpy()
    return incidI_arr


def get_filtered_incidI(diffI, dates, places, filters):
    incidI_arr = np.zeros((len(dates), len(places)), dtype=np.int)
    df = diffI.copy()
    for mc_type, mc_value in filters.items():
        if isinstance(mc_value, str):
            mc_value = [mc_value]
        df = df[df[f'mc_{mc_type}'].isin(mc_value)]

    for mcn in df['mc_name'].unique():
        new_df = df[df['mc_name'] == mcn]
        new_df = new_df.drop([c for c in new_df.columns if 'mc_' in c], axis=1)
        new_df = new_df.drop('date', axis=1)
        incidI_arr = incidI_arr + new_df.to_numpy()
    return incidI_arr

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


def multishiftee(arr, shifts, stoch_delay_flag=True):
    """ Shift along first (0) axis """
    result = np.zeros_like(arr)

    if (stoch_delay_flag):
        raise ValueError("NOT SUPPORTED YET")
        # for i, row in reversed(enumerate(np.rows(arr))):
        #    for j,elem in reversed(enumerate(row)):
        ## This function takes in :
        ##  - elem (int > 0)
        ##  - delay (single average delay)
        ## and outputs
        ##  - vector of fixed size where the k element stores # of people who are delayed by k
        # percentages = np.random.multinomial(el<fixed based on delays[i][j]>)
        #        cases = diff(round(cumsum(percentages)*elem))
        #        for k,case in enumerate(cases):
        #            results[i+k][j] = cases[k]
    else:
        for i, row in enumerate(arr):
            for j, elem in enumerate(row):
                if (i + shifts[i][j] < arr.shape[0]):
                    result[i + shifts[i][j]][j] += elem
    return result


@jit(nopython=True)
def multishift(arr, shifts, stoch_delay_flag=True):
    """ Shift along first (0) axis """
    result = np.zeros_like(arr)

    if (stoch_delay_flag):
        raise ValueError("NOT SUPPORTED YET")
        # for i, row in reversed(enumerate(np.rows(arr))):
        #    for j,elem in reversed(enumerate(row)):
        ## This function takes in :
        ##  - elem (int > 0)
        ##  - delay (single average delay)
        ## and outputs
        ##  - vector of fixed size where the k element stores # of people who are delayed by k
        # percentages = np.random.multinomial(el<fixed based on delays[i][j]>)
        #        cases = diff(round(cumsum(percentages)*elem))
        #        for k,case in enumerate(cases):
        #            results[i+k][j] = cases[k]
    else:
        for i in range(arr.shape[0]):  # numba nopython does not allow iterating over 2D array
            for j in range(arr.shape[1]):
                if (i + shifts[i, j] < arr.shape[0]):
                    result[i + shifts[i, j], j] += arr[i, j]
    return result
