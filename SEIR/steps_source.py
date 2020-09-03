from numba.pycc import CC
import numpy as np

cc = CC("steps")
cc.verbose = True

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


@cc.export(
    "steps_SEIR_nb",
    "float64[:,:,:](float64[:,:], float64[:,:], float64[:,:], float64[:,:], float64[:,:], float64[:,:],"
    "float64, float64[:], int64, int64[:], int32[:], int32[:], float64[:], float64[:,:], boolean)"
)
def steps_SEIR_nb(alpha, beta, sigma, gamma, y0, seeding, dt, t_inter, nnodes, popnodes,
                  mobility_row_indices, mobility_data_indices, mobility_data, dynfilter, stoch_traj_flag):
    y = np.copy(y0)
    states = np.zeros((ncomp, nnodes, len(t_inter)))
    if stoch_traj_flag:
      return(states)

    exposeCases = np.empty(nnodes)
    incidentCases = np.empty(nnodes)
    incident2Cases = np.empty(nnodes)
    incident3Cases = np.empty(nnodes)
    recoveredCases = np.empty(nnodes)

    percent_who_move = np.zeros(nnodes)
    percent_day_away = 0.5
    for j in range(nnodes):
        percent_who_move[j] = mobility_data[mobility_data_indices[j]:mobility_data_indices[j+1]].sum() / popnodes[j]

    for it, t in enumerate(t_inter):
        if (it % int(1 / dt) == 0):
            y[E] = y[E] + seeding[int(t)]
            y[S] = y[S] - seeding[int(t)]
            y[S] = y[S] * (y[S] > 0)

        for i in range(nnodes):
            index = np.arange(mobility_data_indices[i], mobility_data_indices[i+1])
            row_index = mobility_row_indices[index]
            p_expose = 1.0 - np.exp(-dt * (
              ((1 - percent_day_away * percent_who_move[i]) * beta[it][i] *
                  (y[I1][i] + y[I2][i] + y[I3][i]) ** alpha[it][i] / popnodes[i]) +  # Staying at home FoI
              (
                percent_day_away * mobility_data[index] / popnodes[i] *  # Probability of going there
                beta[it][row_index] *  # The beta for there
                (  # num infected there
                  y[I1][row_index] +
                  y[I2][row_index] +
                  y[I3][row_index]
                ) ** alpha[it][i] / popnodes[row_index]  # population there
              ).sum()
            ))

            p_infect = 1 - np.exp(-dt * sigma[it][i])
            p_recover = 1 - np.exp(-dt * gamma[it][i])

            if stoch_traj_flag:
                exposeCases[i] = np.random.binomial(y[S][i], p_expose)
                incidentCases[i] = np.random.binomial(y[E][i], p_infect)
                incident2Cases[i] = np.random.binomial(y[I1][i], p_recover)
                incident3Cases[i] = np.random.binomial(y[I2][i], p_recover)
                recoveredCases[i] = np.random.binomial(y[I3][i], p_recover)
            else:
                exposeCases[i] =    y[S][i] * p_expose
                incidentCases[i] =  y[E][i] * p_infect
                incident2Cases[i] = y[I1][i] * p_recover
                incident3Cases[i] = y[I2][i] * p_recover
                recoveredCases[i] = y[I3][i] * p_recover

        y[S] += -exposeCases
        y[E] += exposeCases - incidentCases
        y[I1] += incidentCases - incident2Cases
        y[I2] += incident2Cases - incident3Cases
        y[I3] += incident3Cases - recoveredCases
        y[R] += recoveredCases
        y[cumI] += incidentCases
        states[:, :, it] = y
        if (it % (1/dt) == 0 and (y[cumI] < dynfilter[int(it % (1/dt))]).any()):
            return -np.ones((ncomp, nnodes, len(t_inter)))

    return states


if __name__ == "__main__":
    cc.compile()
