from numba.pycc import CC
import numpy as np

cc = CC("steps")
cc.verbose = True

debug_mode = False
debug_print = False

seeding_source_col, \
    seeding_destination_col, \
    seeding_spatial_node_col, \
    seeding_value_col = \
    np.arange(4)

transition_source_col, \
    transition_destination_col, \
    transition_rate_col,\
    transition_proportion_start_col, \
    transition_proportion_stop_col = \
    np.arange(5)

proportion_compartment_col = 0
proportion_sum_starts_col = 0
proportion_exponent_col = 1
@cc.export(
    ## name
    "steps_SEIR_nb",
    ## Return
    "float64[:, :, :](" ## states [ ntimes x ncompartments x nspatial_nodes ]
    ## Dimensions
    "int32," ## ncompartments
    "int32," ## nspatial_nodes
    "float64[:]," ## times [ ntimes ]
    ## Parameters
    "float64[:, :, :]," ## Parameters [ nparameters x ntimes x nspatial_nodes]
    "float64," ## dt
    ## Transitions
    "int64[:, :]," ## Transitions [ ntransitions x [source, destination, proportion_start, proportion_stop, rate] ]
    "int64[:, :]," ## proportions_info [ [sum_start, exponent] x ntransition_proportions ]
    "int64[:]," ## transition_sum_compartments [ ntransition_proportion_sums ]
    ## Initial Conditions
    "float64[:,:]," ## initial_conditions [ ncompartments x nspatial_nodes ]
    ## Seeding
    "int64[:]," ## seeding_instance_time_start_indices [ ntimes + 1 ]
    "int64[:,:]," ## seeding_instance_source_compartments [ [source_compartment, destination_compartment, spatial_node, value] x nseeding_instances ]
    ## Mobility
    "float64[:]," # mobility_data [ nmobility_instances ]
    "int32[:]," # mobility_row_indices [ nspatial_nodes ]
    "int32[:]," # mobility_data_indices [ nspatial_nodes + 1]
    "int64[:]," # population [ nspatial_nodes ]
    "boolean" # stochastic_p
    ")"
)
def steps_SEIR_nb(
        ncompartments, #1
        nspatial_nodes, #2
        times, #3
        parameters, #4
        dt, #5
        transitions, #6
        proportion_info, #7
        transition_sum_compartments, #8
        initial_conditions, #9
        seeding_instance_time_start_indices, #10
        seeding_instance_data, #11
        mobility_data, #12
        mobility_row_indices, #13
        mobility_data_indices, #14
        population, #15
        stochastic_p #16
):
    ## Declarations
    states = np.zeros((len(times), ncompartments, nspatial_nodes))
    states_current = np.zeros((ncompartments, nspatial_nodes))
    states_next = np.zeros((ncompartments, nspatial_nodes))

    ntransitions = transitions.shape[0]

    ## Setting values
    states_current = np.copy(initial_conditions)
    states_next = states_current.copy()

    percent_who_move = np.zeros((nspatial_nodes))
    percent_day_away = 0.5
    for spatial_node in range(nspatial_nodes):
        percent_who_move[spatial_node] = min(
            mobility_data[
                mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node+1]
            ].sum() / population[spatial_node],
            1
        )

    compound_adjusted_rate = np.zeros((nspatial_nodes))
    number_move = np.zeros((nspatial_nodes))

    for time_index, time in enumerate(times):
        for seeding_instance_idx in range(
                seeding_instance_time_start_indices[time_index],
                seeding_instance_time_start_indices[time_index+1]
        ):
            # seeding_instance_data[seeding_value_col][seeding_instance_idx] = min(
            #     seeding_instance_data[seeding_value_col][seeding_instance_idx],
            #     states_next[
            #         seeding_instance_data[seeding_source_col][seeding_instance_idx]][
            #             seeding_instance_data[seeding_spatial_node_col][seeding_instance_idx]]
            # )
            states_next[
                seeding_instance_data[seeding_source_col][seeding_instance_idx]][
                    seeding_instance_data[seeding_spatial_node_col][seeding_instance_idx]] -= \
                        seeding_instance_data[seeding_value_col][seeding_instance_idx]
            states_next[
                seeding_instance_data[seeding_destination_col][seeding_instance_idx]][
                    seeding_instance_data[seeding_spatial_node_col][seeding_instance_idx]] += \
                        seeding_instance_data[seeding_value_col][seeding_instance_idx]

        for transition_index in range(ntransitions):
            total_rate = 1
            first_proportion = True
            for proportion_index in range(
                    transitions[transition_proportion_start_col][transition_index],
                    transitions[transition_proportion_stop_col][transition_index]
            ):
                relevant_number_in_comp = np.zeros((nspatial_nodes))
                relevant_exponent = np.ones((nspatial_nodes))
                for proportion_sum_index in range(
                        proportion_info[proportion_sum_starts_col][proportion_index],
                        proportion_info[proportion_sum_starts_col][proportion_index+1]
                ):
                    relevant_number_in_comp += states_current[
                        transition_sum_compartments[proportion_info[proportion_sum_starts_col][proportion_index]]
                    ]
                    relevant_exponent *= parameters[
                        proportion_info[proportion_exponent_col][proportion_sum_index]
                    ][time_index]
                if first_proportion:
                    first_proportion = False
                    source_number = relevant_number_in_comp ** relevant_exponent
                else:
                    for spatial_node in range(nspatial_nodes):
                        proportion_keep_compartment = 1 - percent_day_away * percent_who_move[spatial_node]
                        proportion_change_compartment = percent_day_away * mobility_data[mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]] / population[spatial_node]
                        rate_keep_compartment =   proportion_keep_compartment * \
                            relevant_number_in_comp[spatial_node] ** relevant_exponent[spatial_node] / \
                            population[spatial_node] * \
                            parameters[transitions[transition_rate_col][transition_index]][time_index][spatial_node]
                        rate_change_compartment = proportion_keep_compartment * \
                            relevant_number_in_comp[spatial_node] ** \
                            relevant_exponent[mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]] / \
                            population[mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]] * \
                            relevant_exponent[mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]]
                        total_rate *= (rate_keep_compartment + rate_change_compartment.sum())

            compound_adjusted_rate[spatial_node] = 1.0 - np.exp(-dt * total_rate)

            if stochastic_p:
                for spatial_node in range(nspatial_nodes):
                     number_move[spatial_node] = np.random.binomial(
                         source_number[spatial_node],
                         compound_adjusted_rate[spatial_node]
                     )
            else:
                number_move =  source_number * compound_adjusted_rate

#            # number_move = min(
#            #     number_move,
#            #     states_next[transitions[transition_source_index][transition_index]]
#            # )
#
            states_next[transitions[transition_source_col][transition_index]] -= number_move
            states_next[transitions[transition_destination_col][transition_index]] += number_move

        states[time_index, :,:] = states_next
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
                print("  " , states_current[comp].min())
                print("  " , states_current[comp].max())

        if((states_current.min() < 0) or (states_current.max() > 10 ** 10)):
           raise ValueError("Overflow error")

    return states


if __name__ == "__main__":
    cc.compile()
