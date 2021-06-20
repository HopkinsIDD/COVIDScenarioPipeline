import itertools
import time
import warnings

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from SEIR import NPI, setup, file_paths
from SEIR.utils import config, Timer
import pyarrow.parquet as pq
import pyarrow as pa
import logging

logger = logging.getLogger(__name__)

# The compiled module may be known as steps or SEIR.steps depending on virtual_env vs conda
try:
    with warnings.catch_warnings() as w:  # ignore DeprecationWarning inside numba
        warnings.simplefilter("ignore")
        from steps import steps_SEIR_nb
except ModuleNotFoundError as e:
    try:
        with warnings.catch_warnings() as w:  # ignore DeprecationWarning inside numba
            warnings.simplefilter("ignore")
            from SEIR.steps import steps_SEIR_nb
    except ModuleNotFoundError as e:
        raise RuntimeError("Missing compiled module, please run `python setup.py install`") from e


def onerun_SEIR(sim_id: int, s: setup.Setup, stoch_traj_flag: bool = True):
    scipy.random.seed()

    with Timer('onerun_SEIR.NPI'):
        npi = NPI.NPIBase.execute(npi_config=s.npi_config,
                                  global_config=config,
                                  geoids=s.spatset.nodenames,
                                  pnames_overlap_operation_sum=s.parameters.intervention_overlap_operation['sum'])

    with Timer('onerun_SEIR.seeding'):
        initial_conditions = s.seedingAndIC.draw_ic(sim_id, setup=s)
        seeding_data = s.seedingAndIC.draw_seeding(sim_id, setup=s)

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    with Timer('onerun_SEIR.pdraw'):
        p_draw = s.parameters.parameters_quick_draw(s.n_days, s.nnodes)

    with Timer('onerun_SEIR.reduce'):
        parameters = s.parameters.parameters_reduce(p_draw, npi)
        log_debug_parameters(p_draw, "Parameters without interventions")
        log_debug_parameters(parameters, "Parameters with interventions")

    with Timer('onerun_SEIR.compartments'):
        parsed_parameters, unique_strings, transition_array, proportion_array, proportion_info = \
            s.compartments.get_transition_array(parameters, s.parameters.pnames)

    with Timer('onerun_SEIR.compute'):
        states = steps_SEIR_nb(
            s.compartments.compartments.shape[0],
            s.nnodes,
            s.n_days,
            parsed_parameters,
            s.dt,
            transition_array,
            proportion_array,
            proportion_info,
            initial_conditions,
            seeding_data,
            mobility_data,
            mobility_geoid_indices,
            mobility_data_indices,
            s.popnodes,
            stoch_traj_flag)

    with Timer('onerun_SEIR.postprocess'):
        if s.write_csv or s.write_parquet:
            postprocess_and_write(sim_id, s, states, p_draw, npi, seeding_data)

    return 1


def log_debug_parameters(params, prefix):
    if logging.getLogger().isEnabledFor(logging.DEBUG):
        logging.debug(prefix)
        for parameter in params:
            try:
                logging.debug(
                    f"""    shape {parameter.shape}, type {parameter.dtype}, range [{parameter.min()}, {parameter.mean()}, {parameter.max()}]""")
            except:
                logging.debug(f"""    value {parameter}""")


def onerun_SEIR_loadID(sim_id2write, s, sim_id2load, stoch_traj_flag=True):
    if (s.write_parquet and s.write_csv):
        print("Confused between reading .csv or parquet. Assuming input file is .parquet")
    if s.write_parquet:
        extension = 'parquet'
    elif s.write_csv:
        extension = 'csv'

    sim_id_str = str(sim_id2load + s.first_sim_index - 1).zfill(9)

    scipy.random.seed()

    with Timer('onerun_SEIR_loadID.NPI'):
        npi = NPI.NPIBase.execute(
            npi_config=s.npi_config,
            global_config=config,
            geoids=s.spatset.nodenames,
            loaded_df=setup.npi_load(
                file_paths.create_file_name_without_extension(
                    s.in_run_id,  # Not sure about this one
                    s.in_prefix,  # Not sure about this one
                    sim_id2load + s.first_sim_index - 1,
                    "snpi"
                ),
                extension
            )
        )
    with Timer('onerun_SEIR_loadID.seeding'):
        initial_conditions = s.seedingAndIC.load_ic(sim_id2load, setup=s)
        seeding_data = s.seedingAndIC.load_seeding(sim_id2load, setup=s)

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data
    with Timer('onerun_SEIR_loadID.pdraw'):
        p_draw = s.parameters.parameters_load(
            file_paths.create_file_name_without_extension(
                s.in_run_id,  # Not sure about this one
                s.in_prefix,  # Not sure about this one
                sim_id2load + s.first_sim_index - 1,
                "spar"
            ),
            s.n_days,
            s.nnodes,
            extension
        )
    with Timer('onerun_SEIR_loadID.reduce'):
        parameters = s.parameters.parameters_reduce(p_draw, npi)
        log_debug_parameters(p_draw, "Parameters without interventions")
        log_debug_parameters(parameters, "Parameters with interventions")

    with Timer('onerun_SEIR.compartments'):
        parsed_parameters, unique_strings, transition_array, proportion_array, proportion_info = \
            s.compartments.get_transition_array(parameters, s.parameters.pnames)


    with Timer('onerun_SEIR.compute'):
        states = steps_SEIR_nb(
            s.compartments.compartments.shape[0],
            s.nnodes,
            s.n_days,
            parsed_parameters,
            s.dt,
            transition_array,
            proportion_array,
            proportion_info,
            initial_conditions,
            seeding_data,
            mobility_data,
            mobility_geoid_indices,
            mobility_data_indices,
            s.popnodes,
            stoch_traj_flag)

    with Timer('onerun_SEIR_loadID.postprocess'):
        if s.write_csv or s.write_parquet:
            out_df = postprocess_and_write(sim_id2write, s, states, p_draw, npi, seeding_data)

    return 1


def run_parallel(s, *, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(1, s.nsim + 1)

    if n_jobs == 1:  # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_SEIR(sim_id, s)
    else:
        tqdm.contrib.concurrent.process_map(onerun_SEIR, sim_ids, itertools.repeat(s),
                                            max_workers=n_jobs)

    logging.info(f""">> {s.nsim} simulations completed in {time.monotonic() - start:.1f} seconds""")


def states2Df(s, states):
    # Tidyup data for  R, to save it:
    #
    # Write output to .snpi.*, .spar.*, and .seir.* files

    states_prev, states_incid = states  # both are [ndays x ncompartments x nspatial_nodes ]

    # add line of zero to diff, so we get the real cumulative.
    #states_diff = np.zeros((states_cumu.shape[0] + 1, *states_cumu.shape[1:]))
    #states_diff[1:, :, :] = states_cumu
    #states_diff = np.diff(states_diff, axis=0)

    ts_index = pd.MultiIndex.from_product(
        [pd.date_range(s.ti, s.tf, freq='D'), s.compartments.compartments['name']],
        names=['date', 'mc_name'])
    # prevalence data, we use multi.index dataframe, sparring us the array manipulation we use to do
    prev_df = pd.DataFrame(
        data=states_prev.reshape(s.n_days * s.compartments.get_ncomp(), s.nnodes),
        index=ts_index, columns=s.spatset.nodenames).reset_index()
    prev_df = pd.merge(left=s.compartments.get_compartments_explicitDF(), right=prev_df,
                       how='right', on='mc_name')
    prev_df.insert(loc=0, column='value_type', value='prevalence')

    ts_index = pd.MultiIndex.from_product(
        [pd.date_range(s.ti, s.tf, freq='D'), s.compartments.compartments['name']],
        names=['date', 'mc_name'])

    incid_df = pd.DataFrame(
        data=states_incid.reshape(s.n_days * s.compartments.get_ncomp(), s.nnodes),
        index=ts_index, columns=s.spatset.nodenames).reset_index()
    incid_df = pd.merge(left=s.compartments.get_compartments_explicitDF(), right=incid_df,
                        how='right', on='mc_name')
    incid_df.insert(loc=0, column='value_type', value='incidence')

    out_df = pd.concat((incid_df, prev_df), axis=0).set_index('date')

    return out_df


def postprocess_and_write(sim_id, s, states, p_draw, npi, seeding):
    out_df = states2Df(s, states)
    if s.write_csv:
        npi.writeReductions(
            file_paths.create_file_name_without_extension(s.out_run_id, s.out_prefix,
                                                          sim_id + s.first_sim_index - 1, "snpi"),
            "csv"
        )
        s.parameters.parameters_write(
            p_draw,
            file_paths.create_file_name_without_extension(s.out_run_id, s.out_prefix,
                                                          sim_id + s.first_sim_index - 1, "spar"),
            "csv"
        )

        out_df.to_csv(
            file_paths.create_file_name(s.out_run_id, s.prefix, sim_id + s.out_first_sim_index - 1, "seir", "csv"),
            index='date',
            index_label='date')

    if s.write_parquet:
        npi.writeReductions(
            file_paths.create_file_name_without_extension(s.out_run_id, s.out_prefix,
                                                          sim_id + s.first_sim_index - 1, "snpi"),
            "parquet"
        )

        s.parameters.parameters_write(
            p_draw,
            file_paths.create_file_name_without_extension(s.out_run_id, s.out_prefix,
                                                          sim_id + s.first_sim_index - 1, "spar"),
            "parquet"
        )

        out_df['date'] = out_df.index
        pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
        pa.parquet.write_table(
            pa_df,
            file_paths.create_file_name(s.out_run_id, s.out_prefix, sim_id + s.first_sim_index - 1, "seir",
                                        "parquet")
        )

    return out_df
