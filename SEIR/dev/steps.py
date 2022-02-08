import numpy as np
from scipy.integrate import solve_ivp, odeint, ode
from numba import jit
import numba
import tqdm
from SEIR.utils import Timer

transition_source_col, \
transition_destination_col, \
transition_rate_col, \
transition_proportion_start_col, \
transition_proportion_stop_col = \
    np.arange(5)

proportion_sum_starts_col = 0
proportion_sum_stops_col = 1
proportion_exponent_col = 2

def ode_integration(
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
        integration_method):

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

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    @jit(nopython=True)
    def rhs(t, x, today):
        print('rhs.t', t)
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in tqdm.tqdm(enumerate(times)):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

            rhs_wrapper = lambda t, x: rhs(t, x, today)
            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            if integration_method == 'scipy.solve_ivp':
                sol = solve_ivp(rhs_wrapper, [today, today+1], x_,  t_eval=[today+1])
                sol = sol.y
            elif integration_method == 'scipy.odeint': 
                sol = odeint(rhs, x_, [today, today+1], (today,), tfirst=True)
                sol = sol[1]
            if integration_method == 'scipy.solve_ivp2':
                sol = solve_ivp(rhs_wrapper, [today, today+1], x_,  t_eval=[today+1])
                sol = sol.y
            elif integration_method == 'scipy.odeint2': 
                sol = odeint(rhs, x_, [today, today+1], (today,), tfirst=True, 
                            rtol=1e-3, atol=1e-3)
                sol = sol[1]
            
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid



@jit(nopython=True)
def rk4_integration1(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    def rk4_integrate(t, x, today):
        dt = 1 # day by day rk integration
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next
        

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            sol = rk4_integrate(today, x_, today)  
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid

def rk4_integration2(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    @jit(nopython=True)
    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    @jit(nopython=True)
    def rk4_integrate(t, x, today):
        dt = 1 # day by day rk integration
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next
        

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            sol = rk4_integrate(today, x_, today)  
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid

def rk4_integration3(
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
        stochastic_p  # 16
        ):

    seeding_places_dict = seeding_data['seeding_places']
    seeding_sources_dict = seeding_data['seeding_sources']
    seeding_destinations_dict = seeding_data['seeding_destinations']
    day_start_idx_dict = seeding_data['day_start_idx']

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

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    @jit(nopython=True)
    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    @jit(nopython=True)
    def rk4_integrate(t, x, today):
        dt = 1 # day by day rk integration
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next

    @jit(nopython=True)
    def day_wrapper_rk4(today, states_next):
        x_ =  np.zeros((2, ncompartments, nspatial_nodes))
        for seeding_instance_idx in range(
                    day_start_idx_dict[today],
                    day_start_idx_dict[today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_places_dict[seeding_instance_idx]
                seeding_sources = seeding_sources_dict[seeding_instance_idx]
                seeding_destinations = seeding_destinations_dict[seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts


                # ADD TO cumulative, this is debatable,
                # WARNING this here.
                x_[1][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

        
        x_[0] = states_next
        x_ = np.reshape(x_, x_.size)
        sol = rk4_integrate(today, x_, today)  
        x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))

        return x_
        

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next

            x_ = day_wrapper_rk4(today, states_next)
            
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid

def rk4_integration4(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    @jit(nopython=True)#, fastmath=True, parallel=True)
    def rhs(t, x, today):
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    #@jit(nopython=True, fastmath=True, cache=True)
    def rk4_integrate(t, x, today):
        dt = 1 # day by day rk integration
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next
        

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            sol = rk4_integrate(today, x_, today)  
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid



@jit(nopython=True)#, fastmath=True, parallel=True)
def rk4_integration5(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    
    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...


            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            x_ = np.reshape(x_, x_.size)
            dt = 1 # day by day rk integration
            
            rk_coefs = [0,  dt / 2,  dt / 2, dt]
            kx = np.zeros((4, x_.size))
            for i in range(4):
                t = today
                x = x_ + kx[i-1]*rk_coefs[i]

                states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
                states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

                for transition_index in range(ntransitions):
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
                            # exponents should not be a proportion, since we don't sum them over sum compartments
                            relevant_exponent = parameters[
                                proportion_info[proportion_exponent_col][proportion_index]
                            ][today]
                        if first_proportion:
                            only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                                transitions[transition_proportion_stop_col][transition_index]
                            first_proportion = False
                            source_number = relevant_number_in_comp
                            if source_number.max() > 0:
                                total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                            if only_one_proportion:
                                total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                                rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                                rate_change_compartment /= population[visiting_compartment]
                                rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                                total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

                    #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

                    #if stochastic_p:
                    #    for spatial_node in range(nspatial_nodes):
                    #        number_move[spatial_node] = np.random.binomial(
                    #            source_number[spatial_node],
                    #            compound_adjusted_rate[spatial_node]
                    #        )
                    #else:
                    if True:
                        number_move = source_number * total_rate # * compound_adjusted_rate
                    #for spatial_node in range(nspatial_nodes):
                        #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                        #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                        # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
                    states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
                    states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
                    states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

                #states_current = states_next.copy()
                kx[i] = np.reshape(states_diff, states_diff.size) #return a 1D vector

            sol = x_ + dt / 6 * (kx[0] + 2 * kx[1] + 2 * kx[2] + kx[3])

        #def rk4_integrate(t, x, today):
        #    k1 = rhs(t, x, today)
        #    k2 = rhs(t, x + dt / 2 * k1, today)
        #    k3 = rhs(t, x + dt / 2 * k2, today)
        #    k4 = rhs(t, x + dt * k3, today)
        #    return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)

        #    sol = rk4_integrate(today, x_, today)  
            x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = x_[1]
            states_next = x_[0]

    return states, states_daily_incid


def rk4_integration2_smart(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    @jit(nopython=True, fastmath=True)
    def rhs(t, x):    
        today=int(np.floor(t))
        if (today) > ndays:
            today = ndays-1
        states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return np.reshape(states_diff, states_diff.size) #return a 1D vector

    #@jit(nopython=True)
    def rk4_integrate(today, x):
        dt = 1 # day by day rk integration
        k1 = rhs(today, x)
        k2 = rhs(today, x + dt / 2 * k1)
        k3 = rhs(today, x + dt / 2 * k2)
        k4 = rhs(today, x + dt * k3)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next
        
    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    #for time_index, time in enumerate(times):  # TODO: get rid of dt
    today = 0
    while today < ndays - 1 + 1e-7:
        is_a_new_day = (today != yesterday)
        yesterday = today
        time_jump = 1

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            time_jump = max(
                np.searchsorted(seeding_data['day_start_idx'][today+1:], seeding_data['day_start_idx'][today]+1),
                1)
            if time_jump < 50: time_jump = 1
            if time_jump == 1:
                for seeding_instance_idx in range(
                        seeding_data['day_start_idx'][today],
                        seeding_data['day_start_idx'][today + 1]
                ):
                    this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                    seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                    seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                    seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                    # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                    states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                    states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                        (states_next[seeding_sources][seeding_places] > 0)
                    states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                    total_seeded += this_seeding_amounts
                    times_seeded += 1
                    # ADD TO cumulative, this is debatable,
                    states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                    ### Shape

                    # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                    # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                    # states_current = np.zeros((ncompartments, nspatial_nodes))
                    # states_next = np.zeros((ncompartments, nspatial_nodes))
    
                    # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...
            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            
            x_ = np.reshape(x_, x_.size)
            
            if time_jump == 1:
                sol = rk4_integrate(today, x_)
                x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
                states_daily_incid[today] = x_[1]
                states_next = x_[0]
            else:
                #I = ode(rhs)
                #solver_ode = "dopri5"
                #rtol = 1e-2
                #atol = 1e-2
                #I.set_integrator(solver_ode,rtol=rtol,atol=atol,nsteps=10**8)
                #I.set_initial_value(x_,today)
                #sol =  np.vstack([I.integrate(time) for time in np.arange(today, today+time_jump+1)])
                #print(sol.shape)
                #with Timer('solver_int'):
                #    solver = nbkode.RungeKutta45(rhs, today, x_)
                #ts = np.arange(today, today+time_jump+1)
                #with Timer(f'solver_solve{time_jump}'):
                #    ts, sol = solver.run(ts)
                with Timer(f'solver_solve{time_jump}'):

                    sol = odeint(rhs, 
                                    y0=x_, 
                                    t=np.arange(today, today+time_jump+1), 
                                    tfirst=True)
                
                x_ = np.reshape(sol, (time_jump+1, 2, ncompartments, nspatial_nodes))
                states_daily_incid[today:today+time_jump] = x_[1:,1]
                states_next = x_[1:,0]
                states[today:today+time_jump-1, :, :] = states_next[:-1]
                states_next = states_next[-1]
            today += time_jump

    return states, states_daily_incid


from numba.pycc import CC
from numba import types

cc = CC("integrators")
cc.verbose = True

@cc.export(
    ## name
    "rk4_aot",
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
    "DictType(unicode_type, int64[:]),"  # seeding keys: 'seeding_places', 'seeding_destinations', 'seeding_sources'
    "float64[:],"  # seeding_amounts
    ## Mobility
    "float64[:],"  # mobility_data [ nmobility_instances ]
    "int32[:],"  # mobility_row_indices [ nmobility_instances ]
    "int32[:],"  # mobility_data_indices [ nspatial_nodes + 1]
    "int64[:],"  # population [ nspatial_nodes ]
    "boolean"  # stochastic_p
    ")"
)
def rk4_integration_aot(
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
        stochastic_p  # 16
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
            mobility_data_indices[spatial_node]:mobility_data_indices[spatial_node + 1]
            ].sum() / population[spatial_node],
            1
        )

    #compound_adjusted_rate = np.zeros((nspatial_nodes))
    #number_move = np.zeros((nspatial_nodes))

    def rhs(t, x, today):
        #states_current = np.reshape(x, (2, ncompartments, nspatial_nodes))[0]
        states_current = x[0]
        states_diff =  np.zeros((2, ncompartments, nspatial_nodes))  # first dim: 0 -> states_diff, 1: states_cum

        for transition_index in range(ntransitions):
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
                    # exponents should not be a proportion, since we don't sum them over sum compartments
                    relevant_exponent = parameters[
                        proportion_info[proportion_exponent_col][proportion_index]
                    ][today]
                if first_proportion:
                    only_one_proportion = (transitions[transition_proportion_start_col][transition_index] + 1) == \
                        transitions[transition_proportion_stop_col][transition_index]
                    first_proportion = False
                    source_number = relevant_number_in_comp
                    if source_number.max() > 0:
                        total_rate[source_number > 0] *= source_number[source_number > 0] ** relevant_exponent[source_number > 0] / source_number[source_number > 0]
                    if only_one_proportion:
                        total_rate *= parameters[transitions[transition_rate_col][transition_index]][today]
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
                        rate_change_compartment *= relevant_number_in_comp[visiting_compartment] ** relevant_exponent[visiting_compartment]
                        rate_change_compartment /= population[visiting_compartment]
                        rate_change_compartment *= parameters[transitions[transition_rate_col][transition_index]][today][visiting_compartment]
                        total_rate[spatial_node] *= (rate_keep_compartment + rate_change_compartment.sum())

            #compound_adjusted_rate = 1.0 - np.exp(-dt * total_rate)

            #if stochastic_p:
            #    for spatial_node in range(nspatial_nodes):
            #        number_move[spatial_node] = np.random.binomial(
            #            source_number[spatial_node],
            #            compound_adjusted_rate[spatial_node]
            #        )
            #else:
            if True:
                number_move = source_number * total_rate # * compound_adjusted_rate
            #for spatial_node in range(nspatial_nodes):
                #if number_move[spatial_node] > states_current[transitions[transition_source_col][transition_index]][spatial_node]:
                #    number_move[spatial_node] = states_current[transitions[transition_source_col][transition_index]][spatial_node]
                # Not possible to enforce this anymore, but it shouldn't be a problem or maybe ? # TODO
            states_diff[0,transitions[transition_source_col][transition_index]] -= number_move
            states_diff[0,transitions[transition_destination_col][transition_index]] += number_move
            states_diff[1, transitions[transition_destination_col][transition_index], :] += number_move  # Cumumlative

        #states_current = states_next.copy()
        return states_diff #return a 1D vector

    def rk4_integrate(t, x, today):
        dt = 1 # day by day rk integration
        k1 = rhs(t, x, today)
        k2 = rhs(t, x + dt / 2 * k1, today)
        k3 = rhs(t, x + dt / 2 * k2, today)
        k4 = rhs(t, x + dt * k3, today)
        return x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        #ell_next = dt / 6 * (k1ell + 2 * k2ell + 2 * k3ell + k4ell)
        #return x_next, ell_next
        

    yesterday = -1
    times = np.arange(0, (ndays - 1) + 1e-7, dt)
    for time_index, time in enumerate(times):  # TODO: get rid of dt
        today = int(np.floor(time))
        is_a_new_day = (today != yesterday)
        yesterday = today

        total_seeded = 0
        times_seeded = 0
        if is_a_new_day:
            # Prevalence is saved at the begining of the day, while incidence is during the day
            states[today, :, :] = states_next
            for seeding_instance_idx in range(
                    seeding_data['day_start_idx'][today],
                    seeding_data['day_start_idx'][today + 1]
            ):
                this_seeding_amounts = seeding_amounts[seeding_instance_idx]
                seeding_places = seeding_data['seeding_places'][seeding_instance_idx]
                seeding_sources = seeding_data['seeding_sources'][seeding_instance_idx]
                seeding_destinations = seeding_data['seeding_destinations'][seeding_instance_idx]
                # this_seeding_amounts = this_seeding_amounts < states_next[seeding_sources] ?  this_seeding_amounts : states_next[seeding_instance_idx]
                states_next[seeding_sources][seeding_places] -= this_seeding_amounts
                states_next[seeding_sources][seeding_places] = states_next[seeding_sources][seeding_places] * \
                    (states_next[seeding_sources][seeding_places] > 0)
                states_next[seeding_destinations][seeding_places] += this_seeding_amounts

                total_seeded += this_seeding_amounts
                times_seeded += 1
                # ADD TO cumulative, this is debatable,
                states_daily_incid[today][seeding_destinations][seeding_places] += this_seeding_amounts

                ### Shape

                # states = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_daily_incid = np.zeros((ndays, ncompartments, nspatial_nodes))
                # states_current = np.zeros((ncompartments, nspatial_nodes))
                # states_next = np.zeros((ncompartments, nspatial_nodes))
 
                # the rhs needs states_next, states_daily_incid, states_curent and today. One of them is not needed anymore...

            x_ =  np.zeros((2, ncompartments, nspatial_nodes))
            x_[0] = states_next
            #x_ = np.reshape(x_, x_.size)
            sol = rk4_integrate(today, x_, today)  
            #x_ = np.reshape(sol, (2, ncompartments, nspatial_nodes))
            states_daily_incid[today] = sol[1]
            states_next = sol[0]

    return states, states_daily_incid

if __name__ == "__main__":
    cc.compile()
