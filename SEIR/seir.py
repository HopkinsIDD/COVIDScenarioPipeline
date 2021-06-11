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

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


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
        p_draw = s.parameters.parameters_quick_draw(len(s.t_inter), s.nnodes)

    with Timer('onerun_SEIR.reduce'):
        parameters = s.parameters.parameters_reduce(p_draw, npi, s.dt)
        log_debug_parameters(p_draw, "Parameters without interventions")
        log_debug_parameters(parameters, "Parameters with interventions")

    with Timer('onerun_SEIR.compartments'):
        parsed_parameters, unique_strings, transition_array, proportion_array, proportion_info = \
            s.compartments.get_transition_array(parameters, s.parameters.pnames)

    with Timer('onerun_SEIR.compute'):
        states = steps_SEIR_nb(
            s.compartments.compartments.shape[0],
            s.nnodes,
            s.t_inter,
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


def postprocess_and_write(sim_id, s, states, p_draw, npi, seeding):
    # Tidyup data for  R, to save it:
    if (s.write_csv or s.write_parquet):
        # Write output to .snpi.*, .spar.*, and .seir.* files
        a = states.copy()[:, :, :, ::int(1 / s.dt)]
        a = np.moveaxis(a, 2, 3)
        a = np.moveaxis(a, 1, 2)
        a = np.moveaxis(a, 0, 1)
        b = np.diff(a, axis=0)
        difI = np.zeros((s.n_days, p_draw[4], s.nnodes))
        difI[1:, :] = b[:, cumI, :]
        na = np.zeros((s.n_days, ncomp + 1, p_draw[4], s.nnodes))
        na[:, :-1, :] = a
        na[:, -1, :] = difI
        m, n, i, r = na.shape
        # r : number of nodes
        # i : number of vaccination states
        # n : number of compartments
        # m : number of times

        # FIX ME: No clue if this is right or not
        # c_index = np.tile(np.arange(n),m*i)
        # pc_index = np.moveaxis(np.tile(np.arange(i),(m,n,1)),1,2).reshape(m*n*i)
        c_index = np.moveaxis(np.tile(np.arange(n), (m, i, 1)), 1, 2).reshape(m * n * i)
        pc_index = np.tile(np.arange(i), (m, n, 1)).reshape(m * n * i)
        out_arr = np.column_stack((c_index, pc_index, na.reshape(m * n * i, -1)))
        out_df = pd.DataFrame(
            out_arr,
            columns=['comp', 'p_comp'] + s.spatset.nodenames,
            index=pd.date_range(s.ti, s.tf, freq='D').repeat((ncomp + 1) * p_draw[4]))
        out_df['comp'].replace(S, 'S', inplace=True)
        out_df['comp'].replace(E, 'E', inplace=True)
        out_df['comp'].replace(I1, 'I1', inplace=True)
        out_df['comp'].replace(I2, 'I2', inplace=True)
        out_df['comp'].replace(I3, 'I3', inplace=True)
        out_df['comp'].replace(R, 'R', inplace=True)
        out_df['comp'].replace(cumI, 'cumI', inplace=True)
        out_df['comp'].replace(ncomp, 'diffI', inplace=True)
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
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
                index='time',
                index_label='time')

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

            out_df['time'] = out_df.index
            pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
            pa.parquet.write_table(
                pa_df,
                file_paths.create_file_name(s.out_run_id, s.out_prefix, sim_id + s.first_sim_index - 1, "seir",
                                            "parquet")
            )

    return out_df


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
        initial_conditions = setup.seedingAndIC.y0_load(s, sim_id2load)
        seeding_data = setup.seedingAndIC.seeding_load(s, sim_id2load)

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
            len(s.t_inter),
            s.nnodes,
            extension
        )
    with Timer('onerun_SEIR_loadID.reduce'):
        parameters = s.parameters.parameters_reduce(p_draw, npi, s.dt)
        log_debug_parameters(p_draw, "Parameters without interventions")
        log_debug_parameters(parameters, "Parameters with interventions")

    with Timer('onerun_SEIR.compartments'):
        parsed_parameters, unique_strings, transition_array, proportion_array, proportion_info = \
            s.compartments.get_transition_array(parameters, s.parameters.pnames)

    with Timer('onerun_SEIR.compute'):
        states = steps_SEIR_nb(
            s.compartments.compartments.shape[0],
            s.nnodes,
            s.t_inter,
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
