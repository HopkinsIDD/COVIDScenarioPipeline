from numba.pycc import CC
import numpy as np

cc = CC("steps")
cc.verbose = True

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


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
    "float64[:,:]," # compartments x nodes
    "float64[:,:]," # times x nodes
    "float64,"
    "float64[:]," # times
    "int64,"
    "int64[:]," # sparse nodes x nodes
    "int32[:]," # sparse nodes x nodes
    "int32[:]," # sparse nodes x nodes
    "float64[:]," # compartments x parallel_compartments x nnodes
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
    print("Starting simulation")
    y = np.zeros((ncomp, n_parallel_compartments, nnodes))
    y[:,0,:] = y0
    states = np.zeros((ncomp, n_parallel_compartments, nnodes, len(t_inter)))
    susceptibility_ratio = 1 - susceptibility_ratio
    transmissibility_ratio = 1 - transmissibility_ratio

    exposeCases = np.empty((n_parallel_compartments, nnodes))
    incidentCases = np.empty((n_parallel_compartments, nnodes))
    incident2Cases = np.empty((n_parallel_compartments, nnodes))
    incident3Cases = np.empty((n_parallel_compartments, nnodes))
    recoveredCases = np.empty((n_parallel_compartments, nnodes))
    vaccinatedCases = np.zeros((ncomp,n_parallel_compartments,nnodes))
    p_expose = 0

    percent_who_move = np.zeros((nnodes))
    percent_day_away = 0.5
    for node in range(nnodes):
        percent_who_move[node] = mobility_data[
            mobility_data_indices[node]:mobility_data_indices[node+1]
          ].sum() / popnodes[node]

    for it, t in enumerate(t_inter):
        if (it % int(1 / dt) == 0):
            y[E][0] = y[E][0] + seeding[int(t)]
            y[S][0] = y[S][0] - seeding[int(t)]
            y[S][0] = y[S][0] * (y[S][0] > 0)

        for i in range(nnodes):
            index = np.arange(mobility_data_indices[i], mobility_data_indices[i+1])
            row_index = mobility_row_indices[index]
            n_infected_local = 0
            n_infected_away = np.zeros(len(row_index,))
            for dose in range(n_parallel_compartments):
                n_infected_local = n_infected_local + \
                    (y[I1][dose][i] + y[I2][dose][i] + y[I3][dose][i]) *\
                    transmissibility_ratio[it][dose][i]
                n_infected_away = n_infected_away + \
                    (y[I1][dose][row_index] + y[I2][dose][row_index] + y[I3][dose][row_index]) * \
                    transmissibility_ratio[it][dose][row_index]
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

            if stoch_traj_flag:
                ## Fix this:
                for compartment in range(n_parallel_compartments):
                    exposure_probability = susceptibility_ratio[it][compartment][i] * p_expose
                    if exposure_probability > 1 :
                        exposure_probability = 1
                    exposeCases[compartment][i] = np.random.binomial(y[S][compartment][i], exposure_probability)
                    incidentCases[compartment][i] = np.random.binomial(y[E][compartment][i], p_infect)
                    incident2Cases[compartment][i] = np.random.binomial(y[I1][compartment][i], p_recover)
                    incident3Cases[compartment][i] = np.random.binomial(y[I2][compartment][i], p_recover)
                    recoveredCases[compartment][i] = np.random.binomial(y[I3][compartment][i], p_recover)
            else:
                for compartment in range(n_parallel_compartments):
                    exposeCases[compartment][i] = y[S,compartment][i] * p_expose * susceptibility_ratio[it][compartment][i]
                    incidentCases[compartment][i] =  y[E][compartment][i] * p_infect
                    incident2Cases[compartment][i] = y[I1][compartment][i] * p_recover
                    incident3Cases[compartment][i] = y[I2][compartment][i] * p_recover
                    recoveredCases[compartment][i] = y[I3][compartment][i] * p_recover

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
                for compartment in range(n_parallel_compartments):
                    if stoch_traj_flag:
                        n = y[comp][compartment][i]
                        p = transition_rate[it][compartment][i]
                        vaccinatedCases[comp][compartment][i] = \
                            np.random.binomial(y[comp][compartment][i], transition_rate[it][compartment][i])
                    else:
                        vaccinatedCases[comp][compartment][i] = \
                            y[comp][compartment][i] * transition_rate[it][compartment][i]
        for dose in range(n_parallel_compartments):
            if dose < (n_parallel_compartments - 1):
                y[:,dose,:] -= vaccinatedCases[:,dose,:]
            if dose > 0:
                y[:,dose,:] += vaccinatedCases[:,dose-1,:]

        states[:, :, :, it] = y

    return states


if __name__ == "__main__":
    cc.compile()
