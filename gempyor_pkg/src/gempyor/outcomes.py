import itertools
import time, random
from numba import jit
import numpy as np
import pandas as pd
import tqdm.contrib.concurrent
from .utils import config, Timer
import pyarrow as pa
import pandas as pd
from . import NPI, setup

import logging

logger = logging.getLogger(__name__)


def run_parallel_Outcomes(
    s,
    *,
    sim_id2load,
    sim_id2write,
    nsim=1,
    n_jobs=1
):
    raise NotImplementedError(
        "This method to run many simulation needs to be updated, to preload the slow setup"
    )
    start = time.monotonic()

    sim_id2loads = np.arange(sim_id2load, sim_id2load + s.nsim)
    sim_id2writes = np.arange(sim_id2write, sim_id2write + s.nsim)

    loaded_values = None
    if (n_jobs == 1) or (
        s.nsim == 1
    ):  # run single process for debugging/profiling purposes
        for sim_offset in np.arange(nsim):
            onerun_delayframe_outcomes(
                sim_id2loads[sim_offset],
                s,
                sim_id2writes[sim_offset],
                parameters,
            )
    else:
        tqdm.contrib.concurrent.process_map(
            onerun_delayframe_outcomes,
            sim_id2loads,
            s,
            sim_id2writes,
            itertools.repeat(parameters),
            itertools.repeat(loaded_values),
            max_workers=n_jobs,
        )

    print(
        f"""
>> {nsim} outcomes simulations completed in {time.monotonic() - start:.1f} seconds
"""
    )
    return 1


def build_npi_Outcomes(s: setup.Setup, load_ID: bool, sim_id2load: int):
    if load_ID:
        npi = NPI.NPIBase.execute(
            npi_config=s.npi_config_outcomes,
            global_config=config,
            geoids=s.spatset.nodenames,
            loaded_df=s.read_simID(ftype="hnpi", sim_id=sim_id2load),
        )
    else:
        npi = NPI.NPIBase.execute(
            npi_config=s.npi_config_outcomes,
            global_config=config,
            geoids=s.spatset.nodenames,
            pnames_overlap_operation_sum=s.parameters.intervention_overlap_operation[
                "sum"
            ],
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

    npi = None
    if s.npi_config_outcomes:
        npi = build_npi_Outcomes(s=s, load_ID=load_ID, sim_id2load=sim_id2load)

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
            npi=npi,
        )
        print(outcomes)
        print(parameters)

    with Timer("onerun_delayframe_outcomes.postprocess"):
        postprocess_and_write(sim_id=sim_id2write, s=s, outcomes=outcomes, hpar=hpar, npi=npi)


def read_parameters_from_config(s: setup.Setup):
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
            "keeped",
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
                        raise ValueError("Subclasses not compatible with outcomes ")
                    else:
                        parameters[class_name]["source"] = dict(src_name["incidence"])

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
                    print(
                        f"probability of outcome {new_comp} is affected by intervention "
                        f"named {parameters[class_name]['probability::npi_param_name']} "
                        f"instead of {new_comp}::probability"
                    )
                else:
                    parameters[class_name][
                        "probability::npi_param_name"
                    ] = f"{new_comp}::probability".lower()

                parameters[class_name]["delay"] = outcomes_config[new_comp]["delay"][
                    "value"
                ]
                if outcomes_config[new_comp]["delay"][
                    "intervention_param_name"
                ].exists():
                    parameters[class_name]["delay::npi_param_name"] = (
                        outcomes_config[new_comp]["delay"]["intervention_param_name"]
                        .as_str()
                        .lower()
                    )
                    print(
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
                        print(
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
                        print(
                            f"Using 'param_from_file' for relative probability in outcome {class_name}"
                        )
                        # Sort it in case the relative probablity file is mispecified
                        rel_probability.geoid = rel_probability.geoid.astype("category")
                        rel_probability.geoid.cat.set_categories(
                            s.spatset.nodenames, inplace=True
                        )
                        rel_probability = rel_probability.sort_values(["geoid"])
                        parameters[class_name]["rel_probability"] = rel_probability[
                            "value"
                        ].to_numpy()
                    else:
                        print(
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
    s.write_simID(ftype="snpi", sim_id=sim_id, df=hnpi)


def dataframe_from_array(data, places, dates, comp_name):
    """
        Produce a dataframe in long form from a numpy matrix of
    dimensions: dates * places. This dataframe are merged together
    to produce the final output
    """
    df = pd.DataFrame(data.astype(np.double), columns=places, index=dates)
    df.index.name = "date"
    df.reset_index(inplace=True)
    df = pd.melt(df, id_vars="date", value_name=comp_name, var_name="geoid")
    return df


def read_incidences_sim(s, sim_id):
    seir_df = s.read_simID(ftype="seir", sim_id=sim_id)

    incidences = seir_df[seir_df["mc_value_type"] == "incidence"]
    incidences.drop(["mc_value_type"], inplace=True, axis=1)
    return incidences


def compute_all_multioutcomes(
    *, s, sim_id2write, parameters, loaded_values=None, npi=None
):
    """Compute delay frame based on temporally varying input. We load the seir sim corresponding to sim_id to write"""
    hpar = pd.DataFrame(columns=["geoid", "quantity", "outcome", "value"])
    all_data = {}
    dates = pd.date_range(s.ti, s.tf, freq="D")

    outcomes = dataframe_from_array(
        np.zeros((len(dates), len(s.spatset.nodenames)), dtype=int),
        s.spatset.nodenames,
        dates,
        "zeros",
    ).drop("zeros", axis=1)

    diffI = read_incidences_sim(s, sim_id=sim_id2write)

    for new_comp in parameters:
        if "source" in parameters[new_comp]:
            # Read the config for this compartment: if a source is specified, we
            # 1. compute incidence from binomial draw
            # 2. compute duration if needed
            source_name = parameters[new_comp]["source"]
            print(f"doing {new_comp}")
            if source_name == "incidI" and "incidI" not in all_data:  # create incidI
                source_array = get_filtered_incidI(
                    diffI, dates, s.spatset.nodenames, {"infection_stage": "I1"}
                )
                print(source_array.max(), "sdsdasda")
                all_data["incidI"] = source_array
                outcomes = pd.merge(
                    outcomes,
                    dataframe_from_array(
                        source_array, s.spatset.nodenames, dates, "incidI"
                    ),
                )
            elif isinstance(source_name, dict):
                source_array = get_filtered_incidI(
                    diffI, dates, s.spatset.nodenames, source_name
                )
                # we don't keep source in this cases
            else:  # already defined outcomes
                source_array = all_data[source_name]

            if (loaded_values is not None) and (
                new_comp in loaded_values["outcome"].values
            ):
                ## This may be unnecessary
                probabilities = loaded_values[
                    (loaded_values["quantity"] == "probability")
                    & (loaded_values["outcome"] == new_comp)
                ]["value"].to_numpy()
                delays = loaded_values[
                    (loaded_values["quantity"] == "delay")
                    & (loaded_values["outcome"] == new_comp)
                ]["value"].to_numpy()
            else:
                probabilities = parameters[new_comp][
                    "probability"
                ].as_random_distribution()(
                    size=len(s.spatset.nodenames)
                )  # one draw per geoid
                if "rel_probability" in parameters[new_comp]:
                    probabilities = (
                        probabilities * parameters[new_comp]["rel_probability"]
                    )

                delays = parameters[new_comp]["delay"].as_random_distribution()(
                    size=len(s.spatset.nodenames)
                )  # one draw per geoid
            print(probabilities)
            probabilities[probabilities > 1] = 1
            probabilities[probabilities < 0] = 0
            probabilities = np.repeat(
                probabilities[:, np.newaxis], len(dates), axis=1
            ).T  # duplicate in time
            delays = np.repeat(
                delays[:, np.newaxis], len(dates), axis=1
            ).T  # duplicate in time
            delays = np.round(delays).astype(int)
            # write hpar before NPI
            hpar = pd.concat(
                [
                    hpar,
                    pd.DataFrame.from_dict(
                        {
                            "geoid": s.spatset.nodenames,
                            "quantity": ["probability"] * len(s.spatset.nodenames),
                            "outcome": [new_comp] * len(s.spatset.nodenames),
                            "value": probabilities[0]
                            * np.ones(len(s.spatset.nodenames)),
                        }
                    ),
                    pd.DataFrame.from_dict(
                        {
                            "geoid": s.spatset.nodenames,
                            "quantity": ["delay"] * len(s.spatset.nodenames),
                            "outcome": [new_comp] * len(s.spatset.nodenames),
                            "value": delays[0] * np.ones(len(s.spatset.nodenames)),
                        }
                    ),
                ],
                axis=0,
            )
            if npi is not None:
                delays = NPI.reduce_parameter(
                    parameter=delays,
                    modification=npi.getReduction(
                        parameters[new_comp]["delay::npi_param_name"].lower()
                    ),
                )
                delays = np.round(delays).astype(int)
                probabilities = NPI.reduce_parameter(
                    parameter=probabilities,
                    modification=npi.getReduction(
                        parameters[new_comp]["probability::npi_param_name"].lower()
                    ),
                )

            # Create new compartment incidence:
            all_data[new_comp] = np.empty_like(source_array)
            # Draw with from source compartment
            if s.stoch_traj_flag:
                all_data[new_comp] = np.random.binomial(
                    source_array.astype(np.int32), probabilities
                )
            else:
                all_data[new_comp] = source_array * (
                    probabilities * np.ones_like(source_array)
                )

            # Shift to account for the delay
            ## stoch_delay_flag is whether to use stochastic delays or not
            stoch_delay_flag = False
            all_data[new_comp] = multishift(
                all_data[new_comp], delays, stoch_delay_flag=stoch_delay_flag
            )
            # Produce a dataframe an merge it
            df_p = dataframe_from_array(
                all_data[new_comp], s.spatset.nodenames, dates, new_comp
            )
            outcomes = pd.merge(outcomes, df_p)

            # Make duration
            if "duration" in parameters[new_comp]:
                if (loaded_values is not None) and (
                    new_comp in loaded_values["outcome"].values
                ):
                    durations = loaded_values[
                        (loaded_values["quantity"] == "duration")
                        & (loaded_values["outcome"] == new_comp)
                    ]["value"].to_numpy()
                else:
                    durations = parameters[new_comp][
                        "duration"
                    ].as_random_distribution()(
                        size=len(s.spatset.nodenames)
                    )  # one draw per geoid
                durations = np.repeat(
                    durations[:, np.newaxis], len(dates), axis=1
                ).T  # duplicate in time
                durations = np.round(durations).astype(int)

                hpar = pd.concat(
                    [
                        hpar,
                        pd.DataFrame.from_dict(
                            {
                                "geoid": s.spatset.nodenames,
                                "quantity": ["duration"] * len(s.spatset.nodenames),
                                "outcome": [new_comp] * len(s.spatset.nodenames),
                                "value": durations[0]
                                * np.ones(len(s.spatset.nodenames)),
                            }
                        ),
                    ],
                    axis=0,
                )

                if npi is not None:
                    # import matplotlib.pyplot as plt
                    # plt.imshow(durations)
                    # plt.title(durations.mean())
                    # plt.colorbar()
                    # plt.savefig('Dbef'+new_comp + '-' + source)
                    # plt.close()
                    # print(f"{new_comp}-duration".lower(), npi.getReduction(f"{new_comp}-duration".lower()))
                    durations = NPI.reduce_parameter(
                        parameter=durations,
                        modification=npi.getReduction(
                            parameters[new_comp]["duration::npi_param_name"].lower()
                        ),
                    )  # npi.getReduction(f"{new_comp}::duration".lower()))
                    durations = np.round(durations).astype(int)
                    # plt.imshow(durations)
                    # plt.title(durations.mean())
                    # plt.colorbar()
                    # plt.savefig('Daft'+new_comp + '-' + source)
                    # plt.close()

                all_data[parameters[new_comp]["duration_name"]] = np.cumsum(
                    all_data[new_comp], axis=0
                ) - multishift(
                    np.cumsum(all_data[new_comp], axis=0),
                    durations,
                    stoch_delay_flag=stoch_delay_flag,
                )

                df_p = dataframe_from_array(
                    all_data[parameters[new_comp]["duration_name"]],
                    s.spatset.nodenames,
                    dates,
                    parameters[new_comp]["duration_name"],
                )
                outcomes = pd.merge(outcomes, df_p)

        elif "sum" in parameters[new_comp]:
            sum_outcome = np.zeros(
                (len(dates), len(s.spatset.nodenames)),
                dtype=all_data[parameters[new_comp]["sum"][0]].dtype,
            )
            # Sum all concerned compartment.
            for cmp in parameters[new_comp]["sum"]:
                sum_outcome += all_data[cmp]
            all_data[new_comp] = sum_outcome
            df_p = dataframe_from_array(
                sum_outcome, s.spatset.nodenames, dates, new_comp
            )
            outcomes = pd.merge(outcomes, df_p)
    print(outcomes)
    print(outcomes['incidI'].values)
    print(outcomes['incidH'].values)
    print(hpar)
    print('fds')

    return outcomes, hpar

def get_filtered_incidI(diffI, dates, places, filters):
    incidI_arr = np.zeros((len(dates), len(places)), dtype=int)
    df = diffI.copy()
    for mc_type, mc_value in filters.items():
        if isinstance(mc_value, str):
            mc_value = [mc_value]
        df = df[df[f"mc_{mc_type}"].isin(mc_value)]

    for mcn in df["mc_name"].unique():
        new_df = df[df["mc_name"] == mcn]
        new_df = new_df.drop([c for c in new_df.columns if "mc_" in c], axis=1)
        new_df = new_df.drop("date", axis=1)
        incidI_arr = incidI_arr + new_df.to_numpy()
    return incidI_arr


@jit(nopython=True)
def shift(arr, num, fill_value=0):
    """
    Quite fast shift implementation, along the first axis,
    which is date. num is an integer not negative nor zero
    """
    if num == 0:
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
    """Shift along first (0) axis"""
    result = np.zeros_like(arr)

    if stoch_delay_flag:
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
                if i + shifts[i][j] < arr.shape[0]:
                    result[i + shifts[i][j]][j] += elem
    return result


@jit(nopython=True)
def multishift(arr, shifts, stoch_delay_flag=True):
    """Shift along first (0) axis"""
    result = np.zeros_like(arr)

    if stoch_delay_flag:
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
        for i in range(
            arr.shape[0]
        ):  # numba nopython does not allow iterating over 2D array
            for j in range(arr.shape[1]):
                if i + shifts[i, j] < arr.shape[0]:
                    result[i + shifts[i, j], j] += arr[i, j]
    return result
