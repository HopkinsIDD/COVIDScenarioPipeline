from jinja2 import pass_environment
from numba import jit
import numpy as np
import pandas as pd
import pandas as pd
from . import NPI

import logging

logger = logging.getLogger(__name__)


def get_source_array(source_name: str, all_data, seir_sim, dates, s) -> np.ndarray:
    if source_name == "incidI":  # create incidI
        source_array = get_filtered_incidI(
            seir_sim,
            dates,
            s.spatset.nodenames,
            {"incidence": {"infection_stage": "I1"}},
        )

    elif isinstance(source_name, dict):
        source_array = get_filtered_incidI(
            seir_sim, dates, s.spatset.nodenames, source_name
        )
        # we don't write source to the hosp file in this case
    else:  # it's an already defined outcomes
        source_array = all_data[source_name]

    return source_array


def compute_all_multioutcomes(
    *, s, sim_id2write, parameters, loaded_values=None, npi=None
):
    """Compute delay frame based on temporally varying input. We load the seir sim corresponding to sim_id to write"""
    hpar = pd.DataFrame(columns=["geoid", "quantity", "outcome", "value"])
    all_data = {}  # stores the outcomes data, with shape (dates, places)
    dates = pd.date_range(s.ti, s.tf, freq="D")

    outcomes = dataframe_from_array(
        np.zeros((len(dates), len(s.spatset.nodenames)), dtype=int),
        s.spatset.nodenames,
        dates,
        "zeros",
    ).drop("zeros", axis=1)

    seir_sim = read_seir_sim(s, sim_id=sim_id2write)

    ## stoch_delay_flag is whether to use stochastic delays or not. True is not supported.
    stoch_delay_flag = False

    for new_comp in parameters:
        # there is two way of building outcomes: from a source with probability, delay (and duration if needed)
        # or as a sum of existing outcomes.
        if "source" in parameters[new_comp]:

            # 1. compute the source compartment array incidence --------------------------------------
            source_name: str = parameters[new_comp]["source"]
            source_array = get_source_array(
                source_name=source_name,
                all_data=all_data,
                seir_sim=seir_sim,
                dates=dates,
                s=s,
            )
            if source_name == "incidI" and "incidI" not in all_data:
                # add incidI to the written frame, for consistence with past behaviour
                all_data["incidI"] = source_array
                outcomes = pd.merge(
                    outcomes,
                    dataframe_from_array(
                        source_array, s.spatset.nodenames, dates, "incidI"
                    ),
                )

            # do we have some loaded values for this specific new outcome?
            loaded_values_for_this_comp: bool = (loaded_values is not None) and (
                new_comp in loaded_values["outcome"].values
            )
            # 2. compute the probabilities ----------------------------------------------------------------------------
            if loaded_values_for_this_comp:
                probabilities = loaded_values[
                    (loaded_values["quantity"] == "probability")
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

            probabilities[probabilities > 1] = 1
            probabilities[probabilities < 0] = 0
            probabilities = np.repeat(
                probabilities[:, np.newaxis], len(dates), axis=1
            ).T  # duplicate in time

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
                ],
                axis=0,
            )

            if npi is not None:
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

            # 3. compute the delays --------------------------------------
            if parameters[new_comp]["delay::definition"] == "value":
                if loaded_values_for_this_comp:
                    delays = loaded_values[
                        (loaded_values["quantity"].str.startswith("delay"))
                        & (loaded_values["outcome"] == new_comp)
                    ]["value"].to_numpy()
                else:
                    delays = parameters[new_comp]["delay"].as_random_distribution()(
                        size=len(s.spatset.nodenames)
                    )  # one draw per geoid

                delays = np.repeat(
                    delays[:, np.newaxis], len(dates), axis=1
                ).T  # duplicate in time
                delays = np.round(delays).astype(int)
                # write hpar before NP
                hpar = pd.concat(
                    [
                        hpar,
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

                # Shift to account for the delay
                all_data[new_comp] = multishift(
                    all_data[new_comp], delays, stoch_delay_flag=stoch_delay_flag
                )
            elif parameters[new_comp]["delay::definition"] == "shape":
                # we don't use loaded value when there is a shape
                delay_kernel = parameters[new_comp]["delay"].as_convolution_kernel()
                all_data[new_comp] = convolve_along_time_dim(
                    outcome_array=all_data[new_comp], kernel=delay_kernel
                )
                # delays = delays(
            else:
                raise ValueError("delay::definition must be either 'value' or 'shape'")

            # 5. Produce a dataframe with the incidence in the new compartment and merge it --------------------------------------
            df_p = dataframe_from_array(
                all_data[new_comp], s.spatset.nodenames, dates, new_comp
            )
            outcomes = pd.merge(outcomes, df_p)

            # 4. compute the duration --------------------------------------
            if "duration" in parameters[new_comp]:
                if parameters[new_comp]["duration::definition"] == "value":
                    if (loaded_values is not None) and (
                        new_comp in loaded_values["outcome"].values
                    ):
                        durations = loaded_values[
                            (loaded_values["quantity"].str.startswith("duration"))
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
                elif parameters[new_comp]["duration::definition"] == "shape":
                    duration_kernel = parameters[new_comp][
                        "duration"
                    ].as_convolution_kernel()
                    # breakpoint()
                    # careful duration must be written to the name duration_name
                    all_data[parameters[new_comp]["duration_name"]] = np.cumsum(
                        all_data[new_comp], axis=0
                    ) - convolve_along_time_dim(
                            outcome_array=np.cumsum(all_data[new_comp],axis=0,
                    ),
                            kernel=duration_kernel,
                        )
                        
                    # cumulative of indivdidual entering the outcome compartment
                    # minus the people going out
                else:
                    raise ValueError(
                        "duration::definition must be either 'value' or 'shape'"
                    )

                # add the duration of the ourcome to the final dataframe.
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

    return outcomes, hpar


def get_filtered_incidI(diffI, dates, places, filters):

    if list(filters.keys()) == ["incidence"]:
        vtype = "incidence"
    elif list(filters.keys()) == ["prevalence"]:
        vtype = "prevalence"

    diffI = diffI[diffI["mc_value_type"] == vtype]
    diffI.drop(["mc_value_type"], inplace=True, axis=1)
    filters = filters[vtype]

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


def read_seir_sim(s, sim_id):
    seir_df = s.read_simID(ftype="seir", sim_id=sim_id)

    return seir_df


def convolve_along_time_dim(outcome_array: np.ndarray, kernel: np.ndarray):
    """
    outcomes_array has shape (dates, places)
    kernel is 1d
    """
    result = np.zeros_like(outcome_array)
    for place in range(outcome_array.shape[1]):
        result[:, place] = np.convolve(
            outcome_array[:, place], kernel, mode="same"
        )  # same ensure that the result has the same length as the input.
    return result


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
