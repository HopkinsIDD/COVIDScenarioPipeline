import numpy as np
from numba import jit
import numba
import tqdm
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

    @jit(nopython=True)
    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
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
                if first_proportion:
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
            number_move = source_number * total_rate  # * compound_adjusted_rate

            states_diff[
                0, transitions[transition_source_col][transition_index]
            ] -= number_move
            states_diff[
                0, transitions[transition_destination_col][transition_index]
            ] += number_move
            states_diff[
                1, transitions[transition_destination_col][transition_index], :
            ] += number_move  # Cumumlative

        # states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size)  # return a 1D vector

    # @jit(nopython=True, fastmath=True, cache=True)
    def rk4_integrate(t, x, today):
        dt = 1  # day by day rk integration. TODO: Should support smaller dt !
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        # ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        # return x_next, ell_next

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in tqdm.tqdm(enumerate(times)):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = today != yesterday
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                seeding_data["day_start_idx"][today],
                seeding_data["day_start_idx"][today + 1],
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

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][
                    seeding_places
                ] += this_seeding_amounts

            x_ = np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            sol = rk4_integrate(today, x_, today)
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid
