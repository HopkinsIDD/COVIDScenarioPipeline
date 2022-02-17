import itertools
import time
from matplotlib.pyplot import step

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from . import NPI, setup, file_paths, steps_rk4
from .utils import config, Timer, aws_disk_diagnosis, read_df
import pyarrow as pa
import logging
from .dev import steps as steps_experimental

logger = logging.getLogger(__name__)


def steps_SEIR(
    s,
    parsed_parameters,
    transition_array,
    proportion_array,
    proportion_info,
    initial_conditions,
    seeding_data,
    seeding_amounts,
    stoch_traj_flag,
):

    mobility_data = s.mobility.data
    mobility_data = mobility_data.astype("float64")
    assert type(s.compartments.compartments.shape[0]) == int
    assert type(s.nnodes) == int
    assert s.n_days > 1
    assert parsed_parameters.shape[1:3] == (s.n_days, s.nnodes)
    assert type(s.dt) == float
    assert type(transition_array[0][0]) == np.int64
    assert type(proportion_array[0]) == np.int64
    assert type(proportion_info[0][0]) == np.int64
    assert initial_conditions.shape == (s.compartments.compartments.shape[0], s.nnodes)
    assert type(initial_conditions[0][0]) == np.float64
    # Test of empty seeding:
    assert len(seeding_data.keys()) == 4

    keys_ref = [
        "seeding_sources",
        "seeding_destinations",
        "seeding_places",
        "day_start_idx",
    ]
    for key, item in seeding_data.items():
        assert key in keys_ref
        if key == "day_start_idx":
            assert len(item) == s.n_days + 1
            # assert (item == np.zeros(s.n_days + 1, dtype=np.int64)).all()
        # else:
        #     assert item.size == np.array([], dtype=np.int64)
        assert item.dtype == np.int64

    assert len(mobility_data) > 0

    assert type(mobility_data[0]) == np.float64
    assert len(mobility_data) == len(s.mobility.indices)
    assert type(s.mobility.indices[0]) == np.int32
    assert len(s.mobility.indptr) == s.nnodes + 1
    assert type(s.mobility.indptr[0]) == np.int32
    assert len(s.popnodes) == s.nnodes
    assert type(s.popnodes[0]) == np.int64

    fnct_args = {
        "ncompartments": s.compartments.compartments.shape[0],
        "nspatial_nodes": s.nnodes,
        "ndays": s.n_days,
        "parameters": parsed_parameters,
        "dt": s.dt,
        "transitions": transition_array,
        "proportion_info": proportion_info,
        "transition_sum_compartments": proportion_array,
        "initial_conditions": initial_conditions,
        "seeding_data": seeding_data,
        "seeding_amounts": seeding_amounts,
        "mobility_data": mobility_data,
        "mobility_row_indices": s.mobility.indices,
        "mobility_data_indices": s.mobility.indptr,
        "population": s.popnodes,
        "stochastic_p": stoch_traj_flag,
    }

    logging.info(f"Integrating with method {s.integration_method}")

    if s.integration_method == "legacy":
        raise ValueError("AOT legacy method not available on this version")
        # seir_sim = steps_SEIR_nb(**fnct_args)
    elif s.integration_method == "rk4.jit":
        seir_sim = steps_rk4.rk4_integration(**fnct_args)
    else:
        logging.critical(
            "Experimental !!! These methods are not ready for production ! "
        )
        if s.integration_method in [
            "scipy.solve_ivp",
            "scipy.odeint",
            "scipy.solve_ivp2",
            "scipy.odeint2",
        ]:
            if stoch_traj_flag == True:
                raise ValueError(
                    f"with method {s.integration_method}, only deterministic"
                    f"integration is possible (got stoch_straj_flag={stoch_traj_flag}"
                )
            seir_sim = steps_experimental.ode_integration(
                **fnct_args, integration_method=s.integration_method
            )
        elif s.integration_method == "rk4.jit1":
            seir_sim = steps_experimental.rk4_integration1(**fnct_args)
        elif s.integration_method == "rk4.jit2":
            seir_sim = steps_experimental.rk4_integration2(**fnct_args)
        elif s.integration_method == "rk4.jit3":
            seir_sim = steps_experimental.rk4_integration3(**fnct_args)
        elif s.integration_method == "rk4.jit4":
            seir_sim = steps_experimental.rk4_integration4(**fnct_args)
        elif s.integration_method == "rk4.jit5":
            seir_sim = steps_experimental.rk4_integration5(**fnct_args)
        elif s.integration_method == "rk4.jit6":
            seir_sim = steps_experimental.rk4_integration6(**fnct_args)
        elif s.integration_method == "rk4.jit.smart":
            seir_sim = steps_experimental.rk4_integration2_smart(**fnct_args)
        elif s.integration_method == "rk4_aot":
            seir_sim = steps_experimental.rk4_aot(**fnct_args)
        else:
            raise ValueError(f"Unknow integration scheme, got {s.integration_method}")
    return seir_sim


def build_npi_SEIR(s, load_ID, sim_id2load):
    if load_ID:
        npi = NPI.NPIBase.execute(
            npi_config=s.npi_config_seir,
            global_config=config,
            geoids=s.spatset.nodenames,
            loaded_df=s.read_simID(ftype="snpi", sim_id=sim_id2load),
        )
    else:
        npi = NPI.NPIBase.execute(
            npi_config=s.npi_config_seir,
            global_config=config,
            geoids=s.spatset.nodenames,
            pnames_overlap_operation_sum=s.parameters.intervention_overlap_operation[
                "sum"
            ],
        )
    return npi


def onerun_SEIR(
    sim_id2write: int,
    s: setup.Setup,
    load_ID: bool = False,
    sim_id2load: int = None,
    stoch_traj_flag: bool = True,
):
    scipy.random.seed()

    with Timer("onerun_SEIR.NPI"):
        npi = build_npi_SEIR(s=s, load_ID=load_ID, sim_id2load=sim_id2load)

    with Timer("onerun_SEIR.compartments"):
        (
            unique_strings,
            transition_array,
            proportion_array,
            proportion_info,
        ) = s.compartments.get_transition_array()

    with Timer("onerun_SEIR.seeding"):
        if load_ID:
            initial_conditions = s.seedingAndIC.load_ic(sim_id2load, setup=s)
            seeding_data, seeding_amounts = s.seedingAndIC.load_seeding(
                sim_id2load, setup=s
            )
        else:
            initial_conditions = s.seedingAndIC.draw_ic(sim_id2write, setup=s)
            seeding_data, seeding_amounts = s.seedingAndIC.draw_seeding(
                sim_id2write, setup=s
            )

    with Timer("onerun_SEIR.parameters"):
        # Draw or load parameters
        if load_ID:
            p_draw = s.parameters.parameters_load(
                param_df=s.read_simID(ftype="spar", sim_id=sim_id2load),
                nt_inter=s.n_days,
                nnodes=s.nnodes,
            )
        else:
            p_draw = s.parameters.parameters_quick_draw(
                nt_inter=s.n_days, nnodes=s.nnodes
            )
        # reduce them
        parameters = s.parameters.parameters_reduce(p_draw, npi)
        log_debug_parameters(p_draw, "Parameters without interventions")
        log_debug_parameters(parameters, "Parameters with interventions")

        # Parse them
        parsed_parameters = s.compartments.parse_parameters(
            parameters, s.parameters.pnames, unique_strings
        )
        log_debug_parameters(parsed_parameters, "Unique Parameters used by transitions")

    with Timer("onerun_SEIR.compute"):
        states = steps_SEIR(
            s,
            parsed_parameters,
            transition_array,
            proportion_array,
            proportion_info,
            initial_conditions,
            seeding_data,
            seeding_amounts,
            stoch_traj_flag,
        )

    with Timer("onerun_SEIR.postprocess"):
        if s.write_csv or s.write_parquet:
            out_df = postprocess_and_write(
                sim_id2write, s, states, p_draw, npi, seeding_data
            )
    return out_df


def run_parallel(s, *, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(1, s.nsim + 1)

    if n_jobs == 1:  # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_SEIR(sim_id, s)
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_SEIR, sim_ids, itertools.repeat(s), max_workers=n_jobs
        )

    logging.info(
        f""">> {s.nsim} simulations completed in {time.monotonic() - start:.1f} seconds"""
    )


def states2Df(s, states):
    # Tidyup data for  R, to save it:
    #
    # Write output to .snpi.*, .spar.*, and .seir.* files

    (
        states_prev,
        states_incid,
    ) = states  # both are [ndays x ncompartments x nspatial_nodes ]

    # add line of zero to diff, so we get the real cumulative.
    # states_diff = np.zeros((states_cumu.shape[0] + 1, *states_cumu.shape[1:]))
    # states_diff[1:, :, :] = states_cumu
    # states_diff = np.diff(states_diff, axis=0)

    ts_index = pd.MultiIndex.from_product(
        [pd.date_range(s.ti, s.tf, freq="D"), s.compartments.compartments["name"]],
        names=["date", "mc_name"],
    )
    # prevalence data, we use multi.index dataframe, sparring us the array manipulation we use to do
    prev_df = pd.DataFrame(
        data=states_prev.reshape(s.n_days * s.compartments.get_ncomp(), s.nnodes),
        index=ts_index,
        columns=s.spatset.nodenames,
    ).reset_index()
    prev_df = pd.merge(
        left=s.compartments.get_compartments_explicitDF(),
        right=prev_df,
        how="right",
        on="mc_name",
    )
    prev_df.insert(loc=0, column="mc_value_type", value="prevalence")

    ts_index = pd.MultiIndex.from_product(
        [pd.date_range(s.ti, s.tf, freq="D"), s.compartments.compartments["name"]],
        names=["date", "mc_name"],
    )

    incid_df = pd.DataFrame(
        data=states_incid.reshape(s.n_days * s.compartments.get_ncomp(), s.nnodes),
        index=ts_index,
        columns=s.spatset.nodenames,
    ).reset_index()
    incid_df = pd.merge(
        left=s.compartments.get_compartments_explicitDF(),
        right=incid_df,
        how="right",
        on="mc_name",
    )
    incid_df.insert(loc=0, column="mc_value_type", value="incidence")

    out_df = pd.concat((incid_df, prev_df), axis=0).set_index("date")

    out_df["date"] = out_df.index

    return out_df


def postprocess_and_write(sim_id, s, states, p_draw, npi, seeding):

    # print(f"before postprocess_and_write for id {s.out_run_id}, {s.out_prefix}, {sim_id + s.first_sim_index - 1}")
    # aws_disk_diagnosis()

    # NPIs
    s.write_simID(ftype="snpi", sim_id=sim_id, df=npi.getReductionDF())
    # Parameters
    s.write_simID(
        ftype="spar", sim_id=sim_id, df=s.parameters.getParameterDF(p_draw=p_draw)
    )
    out_df = states2Df(s, states)
    s.write_simID(ftype="seir", sim_id=sim_id, df=out_df)

    return out_df


def log_debug_parameters(params, prefix):
    if logging.getLogger().isEnabledFor(logging.DEBUG):
        logging.debug(prefix)
        for parameter in params:
            try:
                logging.debug(
                    f"""    shape {parameter.shape}, type {parameter.dtype}, range [{parameter.min()}, {parameter.mean()}, {parameter.max()}]"""
                )
            except:
                logging.debug(f"""    value {parameter}""")
