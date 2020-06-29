import itertools
import time, random
import warnings

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from SEIR.utils import config
import pyarrow.parquet as pq
import pyarrow as pa
import pandas as pd
from SEIR import file_paths


def run_delayframe_outcomes(config, run_id, prefix, setup_name, scenario_outcomes, nsim=1, index=1, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(index, index + nsim)

    parameters = read_parameters_from_config(config, run_id, prefix, setup_name, scenario_outcomes, sim_ids)

    if n_jobs == 1:  # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_delayframe_outcomes(sim_id, sim_id, run_id, prefix, setup_name, parameters)
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_delayframe_outcomes,
            sim_ids,
            sim_ids,
            itertools.repeat(run_id),
            itertools.repeat(prefix),
            itertools.repeat(setup_name),
            itertools.repeat(parameters),
            max_workers=n_jobs
        )

    print(f"""
>> {nsim} outcomes simulations completed in {time.monotonic() - start:.1f} seconds
""")


def onerun_delayframe_outcomes_load_hpar(config, run_id, prefix, setup_name, scenario_outcomes, sim_id2write, sim_id2load):
    parameters = read_parameters_from_config(config, run_id, prefix, setup_name, scenario_outcomes, [sim_id2load])

    loaded_values = pq.read_table(file_paths.create_file_name(
        run_id,
        prefix,
        sim_id2load,
        'hpar',
        'parquet'
    )).to_pandas()

    onerun_delayframe_outcomes(sim_id2write, sim_id2load, run_id, prefix, setup_name, parameters, loaded_values)


def read_parameters_from_config(config, run_id, prefix, setup_name, scenario_outcomes, sim_ids):
    # Prepare the probability table:
    # Either mean of probabilities given or from the file... This speeds up a bit the process.
    # However needs an ordered dict, here we're abusing a bit the spec.
    config_outcomes = config["outcomes"]["settings"][scenario_outcomes]
    if (config["outcomes"]["param_from_file"].get()):
        # load a file from the seir model, to know how to filter the provided csv file
        diffI = pd.read_parquet(file_paths.create_file_name(
            run_id,
            setup_name,
            sim_ids[0],
            'seir',
            'parquet'
        ))
        diffI = diffI[diffI['comp'] == 'diffI']
        dates = diffI.time
        diffI.drop(['comp'], inplace=True, axis=1)

        # Load the actual csv file
        branching_file = config["outcomes"]["param_place_file"].as_str()
        branching_data = pa.parquet.read_table(branching_file, ).to_pandas()
        branching_data = branching_data[branching_data['geoid'].isin(diffI.drop('time', axis=1).columns)]
        branching_data["colname"] = "R" + branching_data["outcome"] + "|" + branching_data["source"]
        branching_data = branching_data[["geoid", "colname", "value"]]
        branching_data = pd.pivot(branching_data, index="geoid", columns="colname", values="value")
        if branching_data.shape[0] != diffI.drop('time', axis=1).columns.shape[0]:
            raise ValueError(
                f"Places in seir input files does not correspond to places in outcome probability file {branching_file}")

    parameters = {}
    for new_comp in config_outcomes:
        parameters[new_comp] = {}
        if config_outcomes[new_comp]['source'].exists():
            # Read the config for this compartment
            parameters[new_comp]['source'] = config_outcomes[new_comp]['source'].as_str()
            parameters[new_comp]['probability'] = config_outcomes[new_comp]['probability'][
                'value']

            parameters[new_comp]['delay'] = config_outcomes[new_comp]['delay']['value']

            if config_outcomes[new_comp]['duration'].exists():
                parameters[new_comp]['duration'] = config_outcomes[new_comp]['duration']['value']
                if config_outcomes[new_comp]['duration']['name'].exists():
                    parameters[new_comp]['duration_name'] = config_outcomes[new_comp]['duration']['name'].as_str()
                else:
                    parameters[new_comp]['duration_name'] = new_comp + '_curr'

            if (config["outcomes"]["param_from_file"].get()):
                colname = 'R' + new_comp + '|' + parameters[new_comp]['source']
                if colname in branching_data.columns:
                    print(f"Using 'param_from_file' for probability {colname}")
                    parameters[new_comp]['rel_probability'] = branching_data[colname].to_numpy()
                else:
                    print(f"NOT using 'param_from_file' for probability {colname}")

        elif config_outcomes[new_comp]['sum'].exists():
            parameters[new_comp]['sum'] = config_outcomes[new_comp]['sum']
        else:
            raise ValueError(f"No 'source' or 'sum' specified for comp {new_comp}")

    return parameters


def onerun_delayframe_outcomes(sim_id2write, sim_id2load, run_id, prefix, setup_name, parameters, loaded_values=None):

    # Read files
    diffI, places, dates = read_seir_sim(run_id, setup_name, sim_id2load)

    # Compute outcomes
    outcomes, hpar = compute_all_delayframe_outcomes(parameters, diffI, places, dates, loaded_values)

    # Write output
    write_outcome_sim(outcomes, run_id, prefix, sim_id2write)
    write_outcome_hpar(hpar, run_id, prefix, sim_id2write)


def read_seir_sim(run_id, setup_name, sim_id):
    diffI = pd.read_parquet(file_paths.create_file_name(
        run_id,
        setup_name,
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


def compute_all_delayframe_outcomes(parameters, diffI, places, dates, loaded_values=None):
    scipy.random.seed()

    all_data = {}
    # We store them as numpy matrices. Dimensions is dates X places
    all_data['incidI'] = diffI.drop(['time'], axis=1).to_numpy().astype(np.int32)

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
                probability = parameters[new_comp]['probability'].as_random_distribution()(size=1)
                if 'rel_probability' in parameters[new_comp]:
                    probability = probability * parameters[new_comp]['rel_probability']

                delay = int(np.round(parameters[new_comp]['delay'].as_random_distribution()(size=1)))

            # Create new compartment incidence:
            all_data[new_comp] = np.empty_like(all_data['incidI'])
            # Draw with from source compartment
            all_data[new_comp] = np.random.binomial(all_data[source], probability * np.ones_like(all_data[source]))

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


""" Quite fast shift implementation, along the first axis, 
    which is date. num is an integer not negative nor zero """


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
