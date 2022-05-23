import itertools
import time, random
from numba import jit
import numpy as np
import pandas as pd
import tqdm.contrib.concurrent
from .utils import config, Timer, read_df
import pyarrow as pa
import pandas as pd
from . import NPI, setup
from .outcomes_compute import compute_all_multioutcomes

import logging

logger = logging.getLogger(__name__)


def run_parallel_outcomes(s, *, sim_id2write, nsim=1, n_jobs=1):
    start = time.monotonic()

    # sim_id2loads = np.arange(sim_id2load, sim_id2load + s.nsim)
    sim_id2writes = np.arange(sim_id2write, sim_id2write + s.nsim)

    loaded_values = None
    if (n_jobs == 1) or (
        s.nsim == 1
    ):  # run single process for debugging/profiling purposes
        for sim_offset in np.arange(nsim):
            onerun_delayframe_outcomes(
                sim_id2write=sim_id2writes[sim_offset],
                s=s,
                load_ID=False,
                sim_id2load=None,
            )
            # onerun_delayframe_outcomes(
            #    sim_id2loads[sim_offset],
            #    s,
            #    sim_id2writes[sim_offset],
            #    parameters,
            # )
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_delayframe_outcomes,
            sim_id2writes,
            s,
            max_workers=n_jobs,
        )

    print(
        f"""
>> {nsim} outcomes simulations completed in {time.monotonic() - start:.1f} seconds
"""
    )
    return 1

def build_npi_Outcomes(
    s: setup.Setup,
    load_ID: bool,
    sim_id2load: int,
    config,
    bypass_DF=None,
    bypass_FN=None,
):
    with Timer("Outcomes.NPI"):
        loaded_df = None
        if bypass_DF is not None:
            loaded_df = bypass_DF
        elif bypass_FN is not None:
            loaded_df = read_df(fname=bypass_FN)
        elif load_ID == True:
            loaded_df = s.read_simID(ftype="hnpi", sim_id=sim_id2load)

        if loaded_df is not None:
            npi = NPI.NPIBase.execute(
                npi_config=s.npi_config_outcomes,
                global_config=config,
                geoids=s.spatset.nodenames,
                loaded_df=loaded_df,
            )
        else:
            npi = NPI.NPIBase.execute(
                npi_config=s.npi_config_outcomes,
                global_config=config,
                geoids=s.spatset.nodenames,
            )
    return npi

def onerun_delayframe_outcomes(
    *,
    sim_id2write: int,
    s: setup.Setup,
    load_ID: bool = False,
    sim_id2load: int = None,
):
    with Timer("buildOutcome.structure"):
        parameters = read_parameters_from_config(s)

    npi_outcomes = None
    if s.npi_config_outcomes:
        npi_outcomes = build_npi_Outcomes(
            s=s, load_ID=load_ID, sim_id2load=sim_id2load, config=config
        )

    loaded_values = None
    if load_ID:
        loaded_values = s.read_simID(ftype="hpar", sim_id=sim_id2load)

    # Compute outcomes
    with Timer("onerun_delayframe_outcomes.compute"):
        outcomes, hpar = compute_all_multioutcomes(
            s=s,
            sim_id2write=sim_id2write,
            parameters=parameters,
            loaded_values=loaded_values,
            npi=npi_outcomes,
        )

    with Timer("onerun_delayframe_outcomes.postprocess"):
        postprocess_and_write(
            sim_id=sim_id2write, s=s, outcomes=outcomes, hpar=hpar, npi=npi_outcomes
        )


def read_parameters_from_config(s: setup.Setup):
    with Timer("Outcome.structure"):
        # Prepare the probability table:
        # Either mean of probabilities given or from the file... This speeds up a bit the process.
        # However needs an ordered dict, here we're abusing a bit the spec.
        outcomes_config = s.outcomes_config["settings"][s.outcomes_scenario]
        if s.outcomes_config["param_from_file"].get():
            # Load the actual csv file
            branching_file = s.outcomes_config["param_place_file"].as_str()
            branching_data = pa.parquet.read_table(branching_file).to_pandas()
            if "relative_probability" not in list(branching_data["quantity"]):
                raise ValueError(
                    f"No 'relative_probability' quantity in {branching_file}, therefor making it useless"
                )

            print(
                "Loaded geoids in loaded relative probablity file:",
                len(branching_data.geoid.unique()),
                "",
                end="",
            )
            branching_data = branching_data[
                branching_data["geoid"].isin(s.spatset.nodenames)
            ]
            print(
                "Intersect with seir simulation: ",
                len(branching_data.geoid.unique()),
                "kept",
            )

            if len(branching_data.geoid.unique()) != len(s.spatset.nodenames):
                raise ValueError(
                    f"Places in seir input files does not correspond to places in outcome probability file {branching_file}"
                )

        subclasses = [""]
        if s.outcomes_config["subclasses"].exists():
            subclasses = s.outcomes_config["subclasses"].get()

        parameters = {}
        for new_comp in outcomes_config:
            if outcomes_config[new_comp]["source"].exists():
                for subclass in subclasses:
                    class_name = new_comp + subclass
                    parameters[class_name] = {}
                    # Read the config for this compartement
                    src_name = outcomes_config[new_comp]["source"].get()
                    if isinstance(src_name, str):
                        if src_name != "incidI":
                            parameters[class_name]["source"] = src_name + subclass
                        else:
                            parameters[class_name]["source"] = src_name
                    else:
                        if subclasses != [""]:
                            raise ValueError(
                                "Subclasses not compatible with outcomes from compartments "
                            )
                        elif ("incidence" in src_name.keys()) or (
                            "prevalence" in src_name.keys()
                        ):
                            parameters[class_name]["source"] = dict(src_name)
                        else:
                            raise ValueError(
                                f"unsure how to read outcome {class_name}: not a str, nor an incidence or prevalence: {src_name}"
                            )

                    parameters[class_name]["probability"] = outcomes_config[new_comp][
                        "probability"
                    ]["value"]
                    if outcomes_config[new_comp]["probability"][
                        "intervention_param_name"
                    ].exists():
                        parameters[class_name]["probability::npi_param_name"] = (
                            outcomes_config[new_comp]["probability"][
                                "intervention_param_name"
                            ]
                            .as_str()
                            .lower()
                        )
                        logging.debug(
                            f"probability of outcome {new_comp} is affected by intervention "
                            f"named {parameters[class_name]['probability::npi_param_name']} "
                            f"instead of {new_comp}::probability"
                        )
                    else:
                        parameters[class_name][
                            "probability::npi_param_name"
                        ] = f"{new_comp}::probability".lower()

                    parameters[class_name]["delay"] = outcomes_config[new_comp][
                        "delay"
                    ]["value"]
                    if outcomes_config[new_comp]["delay"][
                        "intervention_param_name"
                    ].exists():
                        parameters[class_name]["delay::npi_param_name"] = (
                            outcomes_config[new_comp]["delay"][
                                "intervention_param_name"
                            ]
                            .as_str()
                            .lower()
                        )
                        logging.debug(
                            f"delay of outcome {new_comp} is affected by intervention "
                            f"named {parameters[class_name]['delay::npi_param_name']} "
                            f"instead of {new_comp}::delay"
                        )
                    else:
                        parameters[class_name][
                            "delay::npi_param_name"
                        ] = f"{new_comp}::delay".lower()

                    if outcomes_config[new_comp]["duration"].exists():
                        parameters[class_name]["duration"] = outcomes_config[new_comp][
                            "duration"
                        ]["value"]
                        if outcomes_config[new_comp]["duration"][
                            "intervention_param_name"
                        ].exists():
                            parameters[class_name]["duration::npi_param_name"] = (
                                outcomes_config[new_comp]["duration"][
                                    "intervention_param_name"
                                ]
                                .as_str()
                                .lower()
                            )
                            logging.debug(
                                f"duration of outcome {new_comp} is affected by intervention "
                                f"named {parameters[class_name]['duration::npi_param_name']} "
                                f"instead of {new_comp}::duration"
                            )
                        else:
                            parameters[class_name][
                                "duration::npi_param_name"
                            ] = f"{new_comp}::duration".lower()

                        if outcomes_config[new_comp]["duration"]["name"].exists():
                            parameters[class_name]["duration_name"] = (
                                outcomes_config[new_comp]["duration"]["name"].as_str()
                                + subclass
                            )
                        else:
                            parameters[class_name]["duration_name"] = (
                                new_comp + "_curr" + subclass
                            )

                    if s.outcomes_config["param_from_file"].get():
                        rel_probability = branching_data[
                            (branching_data["outcome"] == class_name)
                            & (branching_data["quantity"] == "relative_probability")
                        ].copy(deep=True)
                        if len(rel_probability) > 0:
                            logging.debug(
                                f"Using 'param_from_file' for relative probability in outcome {class_name}"
                            )
                            # Sort it in case the relative probablity file is mispecified
                            rel_probability.geoid = rel_probability.geoid.astype(
                                "category"
                            )
                            rel_probability.geoid.cat.set_categories(
                                s.spatset.nodenames, inplace=True
                            )
                            rel_probability = rel_probability.sort_values(["geoid"])
                            parameters[class_name]["rel_probability"] = rel_probability[
                                "value"
                            ].to_numpy()
                        else:
                            logging.debug(
                                f"*NOT* Using 'param_from_file' for relative probability in outcome  {class_name}"
                            )

                # We need to compute sum across classes if there is subclasses
                if subclasses != [""]:
                    parameters[new_comp] = {}
                    parameters[new_comp]["sum"] = [new_comp + c for c in subclasses]
                    if outcomes_config[new_comp]["duration"].exists():
                        duration_name = new_comp + "_curr"
                        if outcomes_config[new_comp]["duration"]["name"].exists():
                            duration_name = outcomes_config[new_comp]["duration"][
                                "name"
                            ].as_str()
                        parameters[duration_name] = {}
                        parameters[duration_name]["sum"] = [
                            duration_name + c for c in subclasses
                        ]

            elif outcomes_config[new_comp]["sum"].exists():
                parameters[new_comp] = {}
                parameters[new_comp]["sum"] = outcomes_config[new_comp]["sum"].get()
            else:
                raise ValueError(f"No 'source' or 'sum' specified for comp {new_comp}")

    return parameters


def postprocess_and_write(sim_id, s, outcomes, hpar, npi):
    outcomes["time"] = outcomes["date"]
    s.write_simID(ftype="hosp", sim_id=sim_id, df=outcomes)
    s.write_simID(ftype="hpar", sim_id=sim_id, df=hpar)

    if npi is None:
        hnpi = pd.DataFrame(
            columns=[
                "geoid",
                "npi_name",
                "start_date",
                "end_date",
                "parameter",
                "reduction",
            ]
        )
    else:
        hnpi = npi.getReductionDF()
    s.write_simID(ftype="hnpi", sim_id=sim_id, df=hnpi)
