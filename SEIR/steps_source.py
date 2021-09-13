from numba.pycc import CC
import numpy as np

cc = CC("steps")
cc.verbose = True

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)

debug_mode = False
debug_print = False

@cc.export(
    ## name
    "steps_SEIR_nb",
    ## Return
    "float64[:,:,:,:](" # times x compartments x parallel_compartments x nodes
    ## Parameters
    "float64[:,:]," # times x nodes
    "float64[:,:]," # times x nodes
    "float64[:,:]," # times x nodes
    "float64[:,:]," # times x nodes
    "int32,"
    "float64[:,:,:]," # times x parallel_compartments x nodes
    "float64[:,:,:]," # times x parallel_compartments x nodes
    "int32,"
    "float64[:,:,:]," # times x parallel_transitions x nodes
    "int32[:]," # parallel_compartments
    "int32[:]," # parallel_compartments
    ## Non-parameters
    "float64[:,:,:]," # compartments x nodes
    "float64[:,:]," # times x nodes
    "float64,"
    "float64[:]," # times
    "int64,"
    "int64[:]," # sparse nodes x nodes
    "int32[:]," # sparse nodes x nodes
    "int32[:]," # sparse nodes x nodes
    "int64[:]," # compartments x parallel_compartments x nnodes
    "boolean"
    ")"
)
def steps_SEIR_nb(
        alpha,
        beta,
        sigma,
        gamma,
        n_parallel_compartments,
        susceptibility_ratio, # actually a reduction
        transmissibility_ratio, # actually a reduction
        n_parallel_transitions,
        transition_rate,
        transition_from,
        transition_to,
        y0,
        seeding,
        dt,
        t_inter,
        nnodes,
        popnodes,
        mobility_row_indices,
        mobility_data_indices,
        mobility_data,
        stoch_traj_flag
):
    y = np.zeros((ncomp, n_parallel_compartments, nnodes))
    y = np.copy(y0)
    states = np.zeros((ncomp, n_parallel_compartments, nnodes, len(t_inter)))
    susceptibility_ratio = 1 - susceptibility_ratio

    transmissibility_ratio = 1 - transmissibility_ratio

    exposeCases = np.zeros((n_parallel_compartments, nnodes)) - 1
    incidentCases = np.zeros((n_parallel_compartments, nnodes)) - 1
    incident2Cases = np.zeros((n_parallel_compartments, nnodes)) - 1
    incident3Cases = np.zeros((n_parallel_compartments, nnodes)) - 1
    recoveredCases = np.zeros((n_parallel_compartments, nnodes)) - 1
    vaccinatedCases = np.zeros((ncomp,n_parallel_transitions,nnodes)) - 1
    p_expose = 0

    percent_who_move = np.zeros((nnodes))
    percent_day_away = 0.5
    for node in range(nnodes):
        percent_who_move[node] = mobility_data[
            mobility_data_indices[node]:mobility_data_indices[node+1]
          ].sum() / popnodes[node]

    for it, t in enumerate(t_inter):
        states[:, :, :, it] = y
        if (it % int(1 / dt) == 0):
            y[E][0] = y[E][0] + seeding[int(t)]
            y[S][0] = y[S][0] - seeding[int(t)]
            y[S][0] = y[S][0] * (y[S][0] > 0)

        for i in range(nnodes):
            index = np.arange(mobility_data_indices[i], mobility_data_indices[i+1])
            row_index = mobility_row_indices[index]
            n_infected_local = 0
            n_infected_away = np.zeros(len(row_index,))
            for p_compartment in range(n_parallel_compartments):
                n_infected_local = n_infected_local + \
                    (y[I1][p_compartment][i] + y[I2][p_compartment][i] + y[I3][p_compartment][i]) *\
                    transmissibility_ratio[it][p_compartment][i]
                n_infected_away = n_infected_away + \
                    (y[I1][p_compartment][row_index] + y[I2][p_compartment][row_index] + y[I3][p_compartment][row_index]) * \
                    transmissibility_ratio[it][p_compartment][row_index]
            p_expose = 1.0 - np.exp(-dt * (
              (
                  (1 - percent_day_away * percent_who_move[i]) * beta[it][i] *
                  n_infected_local ** alpha[it][i] / popnodes[i]
              ) +  # Staying at home FoI
                (
                    percent_day_away * mobility_data[index] / popnodes[i] *  # Probability of going there
                    beta[it][row_index] *  # The beta for there
                    ( n_infected_away ) ** # The number exposed for there
                    alpha[it][i] / popnodes[row_index]  # population there
                ).sum()
            ))

            p_infect = 1 - np.exp(-dt * sigma[it][i])
            p_recover = 1 - np.exp(-dt * gamma[it][i])


            ## Fix this:
            for p_compartment in range(n_parallel_compartments):
                exposure_probability = susceptibility_ratio[it][p_compartment][i] * p_expose
                if(debug_mode):
                    if (np.isnan(exposure_probability)) or (exposure_probability > 1) or (exposure_probability < 0) :
                        raise ValueError("SUSCEPTIBILITY OUT OF BOUNDS")
                    if (np.isnan(p_infect)) or (p_infect > 1) or (p_infect < 0) :
                        raise ValueError("SYMPTOMATIC RATE OUT OF BOUNDS")
                    if (np.isnan(p_recover)) or (p_recover > 1) or (p_recover < 0) :
                        raise ValueError("RECOVERY RATE OUT OF BOUNDS")
                if stoch_traj_flag:
                    exposeCases[p_compartment][i] = np.random.binomial(y[S][p_compartment][i], exposure_probability)
                    incidentCases[p_compartment][i] = np.random.binomial(y[E][p_compartment][i], p_infect)
                    incident2Cases[p_compartment][i] = np.random.binomial(y[I1][p_compartment][i], p_recover)
                    incident3Cases[p_compartment][i] = np.random.binomial(y[I2][p_compartment][i], p_recover)
                    recoveredCases[p_compartment][i] = np.random.binomial(y[I3][p_compartment][i], p_recover)
                else:
                    exposeCases[p_compartment][i] = y[S][p_compartment][i] * exposure_probability
                    incidentCases[p_compartment][i] =  y[E][p_compartment][i] * p_infect
                    incident2Cases[p_compartment][i] = y[I1][p_compartment][i] * p_recover
                    incident3Cases[p_compartment][i] = y[I2][p_compartment][i] * p_recover
                    recoveredCases[p_compartment][i] = y[I3][p_compartment][i] * p_recover

        y[S] += -exposeCases
        y[E] += exposeCases - incidentCases
        y[I1] += incidentCases - incident2Cases
        y[I2] += incident2Cases - incident3Cases
        y[I3] += incident3Cases - recoveredCases
        y[R] += recoveredCases
        y[cumI] += incidentCases

        ## Vaccination
        for i in range(nnodes):
            for comp in range(ncomp-1):
                for transition in range(n_parallel_transitions):
                    from_compartment = transition_from[transition]
                    n = y[comp][from_compartment][i]
                    p = 1 - np.exp(-dt * transition_rate[it][transition][i])
                    if debug_mode:
                        if (np.isnan(p)) or (p > 1) or (p < 0):
                            raise ValueError("TRANSITION RATE OUT OF BOUNDS")
                    if stoch_traj_flag:
                        vaccinatedCases[comp][transition][i] = \
                            np.random.binomial(n, p)
                    else:
                        vaccinatedCases[comp][transition][i] = \
                            n * p
        if debug_print:
            print("Movement")
            print("  exposed [", exposeCases.min(), ", ", exposeCases.max(), "]")
            print("  incident [", incidentCases.min(), ", ", incidentCases.max(), "]")
            print("  incident2 [", incident2Cases.min(), ", ", incident2Cases.max(), "]")
            print("  incident3 [", incident3Cases.min(), ", ", incident3Cases.max(), "]")
            print("  recovered [", recoveredCases.min(), ", ", recoveredCases.max(), "]")
            print("  vaccinated [",vaccinatedCases.min(),", ",vaccinatedCases.max())

        for transition in range(n_parallel_transitions):
            from_compartment = transition_from[transition]
            to_compartment = transition_to[transition]
            y[:-1,from_compartment,:] -= vaccinatedCases[:-1,from_compartment,:]
            y[:-1,to_compartment,:] += vaccinatedCases[:-1,from_compartment,:]


        if debug_print:
            print("Y extremes:")
            print(y.min())
            print(y.max())
            print("  by compartment extremes:")
            for comp in range(ncomp):
                print("  " , y[comp].min())
                print("  " , y[comp].max())

        if((y.min() < 0) or (y.max() > 10 ** 10)):
           raise ValueError("Overflow error")
    return states


if __name__ == "__main__":
    cc.compile()
