from numba.pycc import CC
import numpy as np
import numba as nb
from numba import types

cc = CC("steps")
cc.verbose = True

debug_mode = False
debug_print = False


transition_source_col, \
transition_destination_col, \
transition_rate_col, \
transition_proportion_start_col, \
transition_proportion_stop_col = \
    np.arange(5)

proportion_sum_starts_col = 0
proportion_sum_stops_col = 1
proportion_exponent_col = 2


# noinspection PyRedundantParentheses
@cc.export(
    ## name
    "steps_SEIR_nb",
    ## Return
    "UniTuple(float64[:, :, :], 2) ("  ## return states and cumlative states, both [ ndays x ncompartments x nspatial_nodes ]
    ## Dimensions
    "int32,"  ## ncompartments
    "int32,"  ## nspatial_nodes
    "int32,"  ## Number of days
    ## Parameters
    "float64[:, :, :],"  ## Parameters [ nparameters x ndays x nspatial_nodes]
    "float64,"  ## dt
    ## Transitions
    "int64[:, :],"  ## transitions [ [source, destination, proportion_start, proportion_stop, rate] x ntransitions ]
    "int64[:, :],"  ## proportions_info [ [sum_starts, sum_stops, exponent] x ntransition_proportions ]
    "int64[:],"  ## transition_sum_compartments [ ntransition_proportion_sums ] (aka proportion_array)
    ## Initial Conditions
    "float64[:,:],"  ## initial_conditions [ ncompartments x nspatial_nodes ]
    ## Seeding
    "DictType(unicode_type, int64[:]),"  # seeding keys: 'seeding_amounts', 'seeding_places', 'seeding_destinations', 'seeding_sources'
    ## Mobility
    "float64[:],"  # mobility_data [ nmobility_instances ]
    "int32[:],"  # mobility_row_indices [ nmobility_instances ]
    "int32[:],"  # mobility_data_indices [ nspatial_nodes + 1]
    "int64[:],"  # population [ nspatial_nodes ]
    "boolean"  # stochastic_p
    ")"
)
def steps_SEIR_nb(
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
        mobility_data,  # 11
        mobility_row_indices,  # 12
        mobility_data_indices,  # 13
        population,  # 14
        stochastic_p  # 15
):
    ## Declarations
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    compound_adjusted_rate = np.zeros((nspatial_nodes))
    number_move = np.zeros((nspatial_nodes))

    # print("SEIR.beforeLoop")

    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):
        today = int(np.floor(time))
        is_a_new_day = False
        if time % 1 == 0: is_a_new_day = True
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):

                #print("some-seeding", seeding_data['seeding_amounts'][seeding_instance_idx])
                #print(seeding_instance_idx)
                # seeding_instance_data[seeding_value_col][seeding_instance_idx] = min(
                #     seeding_instance_data[seeding_value_col][seeding_instance_idx],
                #     states_next[
                #         seeding_instance_data[seeding_source_col][seeding_instance_idx]][
                #             seeding_instance_data[seeding_spatial_node_col][seeding_instance_idx]]
                # )

                states_next[
                    seeding_data['seeding_sources'][seeding_instance_idx]][
                    seeding_data['seeding_places'][seeding_instance_idx]] -= \
                    seeding_data['seeding_amounts'][seeding_instance_idx]

                states_next[
                    seeding_data['seeding_sources'][seeding_instance_idx]][
                    seeding_data['seeding_places'][seeding_instance_idx]] = states_next[
                    seeding_data['seeding_sources'][seeding_instance_idx]][
                    seeding_data['seeding_places'][seeding_instance_idx]] * (states_next[
                    seeding_data['seeding_sources'][seeding_instance_idx]][
                    seeding_data['seeding_places'][seeding_instance_idx]] > 0)

                states_next[
                    seeding_data['seeding_destinations'][seeding_instance_idx]][
                    seeding_data['seeding_places'][seeding_instance_idx]] += \
                    seeding_data['seeding_amounts'][seeding_instance_idx]

                # ADD TO cumulative, this is debatable,
                states_daily_incid[today,
                                   seeding_data['seeding_destinations'][seeding_instance_idx],
                                   seeding_data['seeding_places'][seeding_instance_idx]] += \
                    seeding_data['seeding_amounts'][seeding_instance_idx]

        for transition_index in range(ntransitions):
            #print("processing tranision", transition_index)
            total_rate = np.ones((nspatial_nodes))
            first_proportion = True
            for proportion_index in range(
                    transitions[transition_proportion_start_col][transition_index],
                    transitions[transition_proportion_stop_col][transition_index]
            ):
                relevant_number_in_comp = np.zeros((nspatial_nodes))
                relevant_exponent = np.ones((nspatial_nodes))
                for proportion_sum_index in range(
                        proportion_info[proportion_sum_starts_col][proportion_index],
                        proportion_info[proportion_sum_stops_col][proportion_index]
                ):
                    relevant_number_in_comp += states_current[
                        transition_sum_compartments[proportion_sum_index]
                    ]
                    relevant_exponent *= parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
                    #print(total_rate)
                else:
                    for spatial_node in range(nspatial_nodes):
                        proportion_keep_compartment = 1 - percent_day_away * percent_who_move[spatial_node]
                        proportion_change_compartment = percent_day_away * mobility_data[
                                                                           mobility_data_indices[spatial_node]:
                                                                           mobility_data_indices[spatial_node + 1]] / \
                                                        population[spatial_node]
                        rate_keep_compartment = proportion_keep_compartment * \
                                                relevant_number_in_comp[spatial_node] ** \
                                                relevant_exponent[spatial_node] / \
                                                population[spatial_node] * \
                                                parameters[transitions[transition_rate_col][transition_index]][today][spatial_node]

                        visiting_compartment = mobility_row_indices[mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]]
                        rate_change_compartment = proportion_change_compartment
                        rate_change_compartment = rate_change_compartment * relevant_number_in_comp[visiting_compartment]
                        rate_change_compartment = rate_change_compartment ** relevant_exponent[visiting_compartment]
                        rate_change_compartment = rate_change_compartment / population[visiting_compartment]
                        rate_change_compartment = rate_change_compartment * parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            if stochastic_p:
                for spatial_node in range(nspatial_nodes):
                    number_move[spatial_node] = np.random.binomial(
                        source_number[spatial_node],
                        compound_adjusted_rate[spatial_node]
                    )
            else:
                number_move = source_number * compound_adjusted_rate

            #            # number_move = min(
            #            #     number_move,
            #            #     states_next[transitions[transition_source_index][transition_index]]
            #            # )
            #
            states_next[transitions[transition_source_col][transition_index]] -= number_move
            states_next[transitions[transition_destination_col][transition_index]] += number_move
            states_daily_incid[today, transitions[transition_destination_col][transition_index], :] += number_move

        states_current = states_next


        # if debug_print:
        #     print("State Movements:")
        #     movements = states_next - states_current
        #     print("Movement extremes")
        #     print(movements.min())
        #     print(movements.max())
        if debug_print:
            print("Current States extremes:")
            print(states_current.min())
            print(states_current.max())
            print("  states_current compartment extremes:")
            for comp in range(states_current.shape[0]):
                print("  ", states_current[comp].min())
                print("  ", states_current[comp].max())

        if ((states_current.min() < 0) or (states_current.max() > 10 ** 10)):
            print((states_current.min() < 0), (states_current.max() > 10 ** 10))
            raise ValueError(f"Overflow error. Too small ?. Too large ?")

    # print(states)
    # print(states_daily_incid)
    return states, states_daily_incid


if __name__ == "__main__":
    cc.compile()
