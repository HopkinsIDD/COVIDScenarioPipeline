#!/usr/bin/env python

##
# interface.py defines handlers to the gempyor epidemic module
# (both (SEIR) and the pipeline outcomes module (Outcomes))
# so they can be used from R for inference.
# R folks needs to define start a python, and set some variable as follow
# ```R`
#   reticulate::use_python(Sys.which(opt$python),require=TRUE)
#   gempyor <- reticulate::import("gempyor")
#   gempyor_inference_runner <- gempyor$InferenceSimulator(
#                                                 config_path=config_file_out_generation,
#                                                 run_id=test$runid,
#                                                 prefix=global_block_prefix,
#                                                 first_sim_index=1,
#                                                 scenario="test",
#                                                 deathrate="med",
#                                                 stoch_traj_flag=1,
#                                                 initialize=TRUE  # Shall we pre-compute now things that are not pertubed by inference
# )
#
# err <- gempyor_inference_runner$one_simulation(0)
# err <- gempyor_inference_runner$one_simulation_loadID(sim_id2write=0, sim_id2load=0)
# ```
# This populate the namespace with four functions, with return value 0 if the
# function terminated successfully


import pathlib
from . import seir, setup, file_paths
from . import outcomes
from .utils import config, Timer
import numpy as np
from concurrent.futures import ProcessPoolExecutor

### Logger configuration
import logging
import os
import functools
import multiprocessing as mp


logging.basicConfig(level=os.environ.get("COVID_LOGLEVEL", "INFO").upper())
logger = logging.getLogger()
handler = logging.StreamHandler()
# '%(asctime)s %(name)-12s %(levelname)-8s %(message)s'
formatter = logging.Formatter(
    " %(name)s :: %(levelname)-8s :: %(message)s"
    # "%(asctime)s [%(filename)s:%(lineno)s - %(funcName)20s() ] %(message)s"
)

handler.setFormatter(formatter)
# logger.addHandler(handler)


class InferenceSimulator:
    def __init__(
        self,
        config_path,
        run_id="test_run_id",
        prefix="test_prefix",
        first_sim_index=1,
        scenario="inference",
        deathrate="med",
        stoch_traj_flag=False,
        rng_seed=None,
        nsim=1,
        initialize=True,
        out_run_id=None,  # if out_run_id is different from in_run_id, fill this
        out_prefix=None,  # if out_prefix is different from in_prefix, fill this
    ):
        self.scenario = scenario
        self.deathrate = deathrate

        in_run_id = run_id
        if out_run_id is None:
            out_run_id = in_run_id
        in_prefix = prefix
        if out_prefix is None:
            out_prefix = in_prefix

        # Config prep
        config.clear()
        config.read(user=False)
        config.set_file(config_path)
        spatial_config = config["spatial_setup"]
        spatial_base_path = pathlib.Path(spatial_config["base_path"].get())

        np.random.seed(rng_seed)

        interactive = False
        write_csv = False
        write_parquet = True
        self.s = setup.Setup(
            setup_name=config["name"].get() + "_" + str(scenario),
            spatial_setup=setup.SpatialSetup(
                setup_name=spatial_config["setup_name"].get(),
                geodata_file=spatial_base_path / spatial_config["geodata"].get(),
                mobility_file=spatial_base_path / spatial_config["mobility"].get(),
                popnodes_key=spatial_config["popnodes"].get(),
                nodenames_key=spatial_config["nodenames"].get(),
            ),
            nsim=nsim,
            npi_scenario=scenario,
            npi_config_seir=config["interventions"]["settings"][scenario],
            seeding_config=config["seeding"],
            initial_conditions_config=config["initial_conditions"],
            parameters_config=config["seir"]["parameters"],
            seir_config=config["seir"],
            outcomes_config=config["outcomes"],
            outcomes_scenario=deathrate,
            ti=config["start_date"].as_date(),
            tf=config["end_date"].as_date(),
            interactive=interactive,
            write_csv=write_csv,
            write_parquet=write_parquet,
            dt=config["dt"].as_number(),
            first_sim_index=first_sim_index,
            in_run_id=in_run_id,
            in_prefix=in_prefix,
            out_run_id=out_run_id,
            out_prefix=out_prefix,
            stoch_traj_flag=stoch_traj_flag,
        )

        print(
            f"""  gempyor >> Running ***{'STOCHASTIC' if stoch_traj_flag else 'DETERMINISTIC'}*** simulation;\n"""
            f"""  gempyor >> Setup {self.s.setup_name}; index: {self.s.first_sim_index}; run_id: {in_run_id},\n"""
            f"""  gempyor >> prefix: {in_prefix};"""  # ti: {s.ti}; tf: {s.tf};
        )

        self.already_built = (
            False  # whether we have already build the costly object we just build once.
        )

    def update_prefix(self, new_prefix, new_out_prefix=None):
        self.s.in_prefix = new_prefix
        if new_out_prefix is None:
            self.s.out_prefix = new_prefix
        else:
            self.s.out_prefix = new_out_prefix

    # profile()
    def one_simulation_legacy(
        self, sim_id2write: int, load_ID: bool = False, sim_id2load: int = None
    ):
        sim_id2write = int(sim_id2write)
        if load_ID:
            sim_id2load = int(sim_id2load)
        with Timer(
            f">>> GEMPYOR onesim {'(loading file)' if load_ID else '(from config)'}"
        ):
            with Timer("onerun_SEIR"):
                seir.onerun_SEIR(
                    sim_id2write=sim_id2write,
                    s=self.s,
                    load_ID=load_ID,
                    sim_id2load=sim_id2load,
                )

            with Timer("onerun_OUTCOMES"):
                outcomes.onerun_delayframe_outcomes(
                    sim_id2write=sim_id2write,
                    s=self.s,
                    load_ID=load_ID,
                    sim_id2load=sim_id2load,
                )
        return 0

    def one_simulation(
        self,
        sim_id2write: int,
        load_ID: bool = False,
        sim_id2load: int = None,
        parallel=True,
    ):
        sim_id2write = int(sim_id2write)
        if load_ID:
            sim_id2load = int(sim_id2load)

        with Timer(
            f">>> GEMPYOR onesim {'(loading file)' if load_ID else '(from config)'}"
        ):
            if not self.already_built:
                self.outcomes_parameters = outcomes.read_parameters_from_config(self.s)

            npi_outcomes = None
            if parallel:
                with Timer("//things"):
                    with ProcessPoolExecutor(
                        max_workers=max(mp.cpu_count(), 3)
                    ) as executor:
                        ret_seir = executor.submit(
                            seir.build_npi_SEIR, self.s, load_ID, sim_id2load, config
                        )
                        if self.s.npi_config_outcomes:
                            ret_outcomes = executor.submit(
                                outcomes.build_npi_Outcomes,
                                self.s,
                                load_ID,
                                sim_id2load,
                                config,
                            )
                        if not self.already_built:
                            ret_comparments = executor.submit(
                                self.s.compartments.get_transition_array
                            )

                # print("expections:", ret_seir.exception(), ret_outcomes.exception(), ret_comparments.exception())

                if not self.already_built:
                    (
                        self.unique_strings,
                        self.transition_array,
                        self.proportion_array,
                        self.proportion_info,
                    ) = ret_comparments.result()
                    self.already_built = True
                npi_seir = ret_seir.result()
                if self.s.npi_config_outcomes:
                    npi_outcomes = ret_outcomes.result()
            else:
                (
                    self.unique_strings,
                    self.transition_array,
                    self.proportion_array,
                    self.proportion_info,
                ) = self.s.compartments.get_transition_array()
                npi_seir = seir.build_npi_SEIR(
                    s=self.s, load_ID=load_ID, sim_id2load=sim_id2load, config=config
                )
                if self.s.npi_config_outcomes:
                    npi_outcomes = outcomes.build_npi_Outcomes(
                        s=self.s,
                        load_ID=load_ID,
                        sim_id2load=sim_id2load,
                        config=config,
                    )

            ### Run every time:
            with Timer("onerun_SEIR.seeding"):
                if load_ID:
                    initial_conditions = self.s.seedingAndIC.load_ic(
                        sim_id2load, setup=self.s
                    )
                    seeding_data, seeding_amounts = self.s.seedingAndIC.load_seeding(
                        sim_id2load, setup=self.s
                    )
                else:
                    initial_conditions = self.s.seedingAndIC.draw_ic(
                        sim_id2write, setup=self.s
                    )
                    seeding_data, seeding_amounts = self.s.seedingAndIC.draw_seeding(
                        sim_id2write, setup=self.s
                    )

            with Timer("SEIR.parameters"):
                # Draw or load parameters
                if load_ID:
                    p_draw = self.s.parameters.parameters_load(
                        param_df=self.s.read_simID(ftype="spar", sim_id=sim_id2load),
                        nt_inter=self.s.n_days,
                        nnodes=self.s.nnodes,
                    )
                else:
                    p_draw = self.s.parameters.parameters_quick_draw(
                        nt_inter=self.s.n_days, nnodes=self.s.nnodes
                    )
                # reduce them
                parameters = self.s.parameters.parameters_reduce(p_draw, npi_seir)

                # Parse them
                parsed_parameters = self.s.compartments.parse_parameters(
                    parameters, self.s.parameters.pnames, self.unique_strings
                )

            with Timer("SEIR.compute"):
                states = seir.steps_SEIR(
                    self.s,
                    parsed_parameters,
                    self.transition_array,
                    self.proportion_array,
                    self.proportion_info,
                    initial_conditions,
                    seeding_data,
                    seeding_amounts,
                )

            with Timer("SEIR.postprocess"):
                if self.s.write_csv or self.s.write_parquet:
                    out_df = seir.postprocess_and_write(
                        sim_id2write, self.s, states, p_draw, npi_seir, seeding_data
                    )

            loaded_values = None
            if load_ID:
                loaded_values = self.s.read_simID(ftype="hpar", sim_id=sim_id2load)

            # Compute outcomes
            with Timer("onerun_delayframe_outcomes.compute"):
                outcomes_df, hpar_df = outcomes.compute_all_multioutcomes(
                    s=self.s,
                    sim_id2write=sim_id2write,
                    parameters=self.outcomes_parameters,
                    loaded_values=loaded_values,
                    npi=npi_outcomes,
                )

            with Timer("onerun_delayframe_outcomes.postprocess"):
                outcomes.postprocess_and_write(
                    sim_id=sim_id2write,
                    s=self.s,
                    outcomes=outcomes_df,
                    hpar=hpar_df,
                    npi=npi_outcomes,
                )

        return 0
