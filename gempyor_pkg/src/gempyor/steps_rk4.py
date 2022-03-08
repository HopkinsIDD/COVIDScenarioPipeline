import numpy as np
from numba import jit
import tqdm, scipy
from .utils import Timer

(
    transition_source_col,
    transition_destination_col,
    transition_rate_col,
    transition_proportion_start_col,
    transition_proportion_stop_col,
) = np.arange(5)

proportion_sum_starts_col = 0
proportion_sum_stops_col = 1
proportion_exponent_col = 2


def rk4_integration(
    *,
    ncompartments,  # 1
    nspatial_nodes,  # 2
    ndays,  # 3
    parameters,  # 4
    dt,  # 5
    transitions,  # 6
    proportion_info,  # 7
    transition_sum_compartments,  # 8
    initial_conditions,  # 9
    seeding_data,  # 10
    seeding_amounts,  # 11
    mobility_data,  # 12
    mobility_row_indices,  # 13
    mobility_data_indices,  # 14
    population,  # 15
    stochastic_p,  # 16
    method="rk4",
):
    states = np.zeros((ndays, ncompartments, nspatial_nodes))
    states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
    states_current = np.zeros((ncompartments, nspatial_nodes))
    states_next = np.zeros((ncompartments, nspatial_nodes))

    ntransitions = transitions.shape[1]

    ## Setting values
    states_current = np.copy(initial_conditions)
    states_next = states_current.copy()

    percent_who_move = np.zeros((nspatial_nodes))
    percent_day_away = 0.5
    for spatial_node in range(nspatial_nodes):
        percent_who_move[spatial_node] = min(
            mobility_data[
                mobility_data_indices[spatial_node] : mobility_data_indices[
                    spatial_node + 1
                ]
            ].sum()
            / population[spatial_node],
            1,
        )

    @jit(nopython=True, fastmath=True)
    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        st_next = (
            states_current.copy()
        )  # this is used to make sure stochastic integration never goes below zero
        states_diff = np.zeros(
            (2, ncompartments, nspatial_nodes)
        )  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
            total_rate = np.ones((nspatial_nodes))
            first_proportion = True
            for proportion_index in range(
                transitions[transition_proportion_start_col][transition_index],
                transitions[transition_proportion_stop_col][transition_index],
            ):
                relevant_number_in_comp = np.zeros((nspatial_nodes))
                relevant_exponent = np.ones((nspatial_nodes))
                for proportion_sum_index in range(
                    proportion_info[proportion_sum_starts_col][proportion_index],
                    proportion_info[proportion_sum_stops_col][proportion_index],
                ):
                    relevant_number_in_comp += states_current[
                        transition_sum_compartments[proportion_sum_index]
                    ]
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if (
                    first_proportion
                ):  # TODO: ask why there is nothing with n_spatial node here.
                    only_one_proportion = (
                        transitions[transition_proportion_start_col][transition_index]
                        + 1
                    ) == transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= (
                            source_number[source_number > 0]
                            ** relevant_exponent[source_number > 0]
                            / source_number[source_number > 0]
                        )
                    if only_one_proportion:
                        total_rate *= parameters[
                            transitions[transition_rate_col][transition_index]
                        ][today]
                else:
                    for spatial_node in range(nspatial_nodes):
                        proportion_keep_compartment = (
                            1 - percent_day_away * percent_who_move[spatial_node]
                        )
                        proportion_change_compartment = (
                            percent_day_away
                            * mobility_data[
                                mobility_data_indices[
                                    spatial_node
                                ] : mobility_data_indices[spatial_node + 1]
                            ]
                            / population[spatial_node]
                        )
                        rate_keep_compartment = (
                            proportion_keep_compartment
                            * relevant_number_in_comp[spatial_node]
                            ** relevant_exponent[spatial_node]
                            / population[spatial_node]
                            * parameters[
                                transitions[transition_rate_col][transition_index]
                            ][today][spatial_node]
                        )

                        visiting_compartment = mobility_row_indices[
                            mobility_data_indices[spatial_node] : mobility_data_indices[
                                spatial_node + 1
                            ]
                        ]

                        rate_change_compartment = proportion_change_compartment
                        rate_change_compartment *= (
                            relevant_number_in_comp[visiting_compartment]
                            ** relevant_exponent[visiting_compartment]
                        )
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[
                            transitions[transition_rate_col][transition_index]
                        ][today][visiting_compartment]
                        total_rate[spatial_node] *= (
                            rate_keep_compartment + rate_change_compartment.sum()
                        )

            if method == "rk4":
                number_move = source_number * total_rate  # * compound_adjusted_rate
            elif method == "legacy":
                compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)
                if stochastic_p:
                    number_move = (
                        source_number * compound_adjusted_rate
                    )  ## to initialize typ
                    for spatial_node in range(nspatial_nodes):
                        number_move[spatial_node] = np.random.binomial(
                            source_number[spatial_node],
                            compound_adjusted_rate[spatial_node],
                        )
                        if (
                            number_move[spatial_node]
                            > st_next[
                                transitions[transition_source_col][transition_index]
                            ][spatial_node]
                        ):
                            number_move[spatial_node] = st_next[
                                transitions[transition_source_col][transition_index]
                            ][spatial_node]
                    st_next[
                        transitions[transition_source_col][transition_index]
                    ] -= number_move
                    st_next[
                        transitions[transition_destination_col][transition_index]
                    ] += number_move
                else:
                    number_move = source_number * compound_adjusted_rate

            states_diff[
                0, transitions[transition_source_col][transition_index]
            ] -= number_move
            states_diff[
                0, transitions[transition_destination_col][transition_index]
            ] += number_move
            states_diff[
                1, transitions[transition_destination_col][transition_index], :
            ] += number_move  # Cumumlative

        return np.reshape(states_diff, states_diff.size)  # return a 1D vector

    @jit(nopython=True, fastmath=True)
    def rk4_integrate(t, x, today):
        k1 = rhs(t, x, today)
        k2 = rhs(t + dt / 2, x + dt / 2 * k1, today)
        k3 = rhs(t + dt / 2, x + dt / 2 * k2, today)
        k4 = rhs(t + dt, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)

    for time_index, time in tqdm.tqdm(enumerate(times)):  # , total=len(times)
        today = int(np.floor(time))
        is_a_new_day = today != yesterday
        yesterday = today

        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                seeding_data["day_start_idx"][today],
                seeding_data["day_start_idx"][
                    min(
                        today + int(np.ceil(dt)), len(seeding_data["day_start_idx"]) - 1
                    )
                ],
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data["seeding_places"][seeding_instance_idx]
                seeding_sources = seeding_data["seeding_sources"][seeding_instance_idx]
                seeding_destinations = seeding_data["seeding_destinations"][
                    seeding_instance_idx
                ]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[
                    seeding_sources
                ][seeding_places] * (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][
                    seeding_places
                ] += this_seeding_amounts

                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][
                    seeding_places
                ] += this_seeding_amounts

        x_ = np.zeros((2, ncompartments, nspatial_nodes))
        x_[0] = states_next
        x_ = np.reshape(x_, x_.size)
        if method == "rk4":
            sol = rk4_integrate(time, x_, today)
        elif method == "legacy":
            sol = x_ + rhs(time, x_, today)
        x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
        states_daily_incid[today] += x_[1]
        states_next = x_[0]

    if dt == 2.0:  # smooth prevalence:
        states_f = scipy.interpolate.interp1d(
            np.arange(ndays, step=2),
            states[::2, :, :],
            axis=0,
            kind="linear",
            bounds_error=False,  # necessary in some case, for some ti and tf
            fill_value="extrapolate",  # necessary in some case, for some ti and tf
        )
        states = states_f(np.arange(ndays))

        # states_i_f = scipy.interpolate.interp1d(np.arange(ndays, step=2), states_daily_incid[::2, :, :], axis=0, kind="linear")
        # states_daily_incid = states_i_f(np.arange(ndays)) / 2
        ## error is smaller with this bellow, but there should be even smarter ways of doing this TODO
        states_daily_incid = states_daily_incid / 2
        states_daily_incid[1::2, :, :] = states_daily_incid[:-1:2, :, :]

    return states, states_daily_incid
