import itertools
import time
import uuid

from numba import jit
import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from . import NPI, setup
from .utils import config

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


def onerun_SEIR(uid, s):
    scipy.random.seed()

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)
    npi = npi.get().T

    seeding = setup.seeding_draw(s, uid)

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data
    # mobility_ori, mobility_dest = s.mobility.row, s.mobility.col
    # mobility_prob = 1.0 - np.exp(-s.dt * s.mobility.data / s.popnodes[mobility_ori])
    # states = steps_SEIR_nb(setup.parameters_quick_draw(s, npi),
    #                        seeding, uid, s.dt, s.t_inter, s.nnodes, s.popnodes,
    #                        mobility_ori, mobility_dest, mobility_prob, s.dynfilter)
    states = steps_SEIR_nb(setup.parameters_quick_draw(s, npi),
                           seeding, uid, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)

    # Tidyup data for  R, to save it:
    if s.write_csv:
        a = states.copy()[:, :, ::int(1 / s.dt)]
        a = np.moveaxis(a, 1, 2)
        a = np.moveaxis(a, 0, 1)
        b = np.diff(a, axis=0)
        difI = np.zeros((s.t_span + 1, s.nnodes))
        difI[1:, :] = b[:, cumI, :]
        na = np.zeros((s.t_span + 1, ncomp + 1, s.nnodes))
        na[:, :-1, :] = a
        na[:, -1, :] = difI
        m, n, r = na.shape
        out_arr = np.column_stack((np.tile(np.arange(n),
                                           m), na.reshape(n * m, -1)))
        out_df = pd.DataFrame(
            out_arr,
            columns=['comp'] + s.spatset.nodenames,
            index=pd.date_range(s.ti, s.tf, freq='D').repeat(ncomp + 1))
        out_df['comp'].replace(S, 'S', inplace=True)
        out_df['comp'].replace(E, 'E', inplace=True)
        out_df['comp'].replace(I1, 'I1', inplace=True)
        out_df['comp'].replace(I2, 'I2', inplace=True)
        out_df['comp'].replace(I3, 'I3', inplace=True)
        out_df['comp'].replace(R, 'R', inplace=True)
        out_df['comp'].replace(cumI, 'cumI', inplace=True)
        out_df['comp'].replace(ncomp, 'diffI', inplace=True)
        str(uuid.uuid4())[:2]
        out_df.to_csv(
            f"{s.datadir}{s.timestamp}_{s.setup_name}_{str(uuid.uuid4())}.csv",
            index='time',
            index_label='time')

    return 1


def run_parallel(s, *, n_jobs=1):
    start = time.monotonic()
    uids = np.arange(s.nsim)

    if n_jobs == 1:          # run single process for debugging/profiling purposes
        for uid in tqdm.tqdm(uids):
            onerun_SEIR(uid, s)
    else:
        tqdm.contrib.concurrent.process_map(onerun_SEIR, uids, itertools.repeat(s),
                                            max_workers=n_jobs)

    print(f"""
>> {s.nsim} simulations completed in {time.monotonic()-start:.1f} seconds
""")


@jit(nopython=True)
def steps_SEIR_nb(p_vec, seeding, uid, dt, t_inter, nnodes, popnodes,
                  mobility_row_indices, mobility_data_indices, mobility_data, dynfilter):
    """
        Made to run just-in-time-compiled by numba, hence very descriptive and using loop,
        because loops are expanded by the compiler hence not a problem.
        as there is very few authorized function. Needs the nopython option to be fast.
    """
    #np.random.seed(uid)
    t = 0

    y = np.zeros((ncomp, nnodes))
    y[S, :] = popnodes
    states = np.zeros((ncomp, nnodes, len(t_inter)))

    p_infect = 1 - np.exp(-dt * p_vec[1][0][0])
    p_recover = 1 - np.exp(-dt * p_vec[2][0][0])

    percent_who_move = np.zeros(nnodes)
    for j in range(nnodes):
      percent_who_move[j] = mobility_data[mobility_data_indices[j]:mobility_data_indices[j+1] ].sum() / popnodes[j]

    for it, t in enumerate(t_inter):
        is_check_loop = (it % int(1 / dt) == 0)
        if is_check_loop:
            y[I1] += seeding[int(t)]
            y[cumI] += seeding[int(t)]

        for i in range(nnodes):

            p_expose = 1.0 - np.exp(-dt * (
                (1 - percent_who_move[i] ) * p_vec[0][it][i] * (y[I1][i] + y[I2][i] + y[I3][i]) / popnodes[i] + 
                (    percent_who_move[i] ) * (
                  mobility_data[mobility_data_indices[i]:mobility_data_indices[i+1] ] *
                  p_vec[0][it][mobility_row_indices[i] ] *
                  (
                    y[I1][mobility_row_indices[i] ] +
                    y[I2][mobility_row_indices[i] ] +
                    y[I3][mobility_row_indices[i] ]
                  ) / popnodes[mobility_row_indices[i] ]
                ).sum()
              )
            )

            exposeCases = np.random.binomial(y[S][i], p_expose)
            incidentCases = np.random.binomial(y[E][i], p_infect)
            incident2Cases = np.random.binomial(y[I1][i], p_recover)
            incident3Cases = np.random.binomial(y[I2][i], p_recover)
            recoveredCases = np.random.binomial(y[I3][i], p_recover)

            y[S][i] += -exposeCases
            y[E][i] += exposeCases - incidentCases
            y[I1][i] += incidentCases - incident2Cases
            y[I2][i] += incident2Cases - incident3Cases
            y[I3][i] += incident3Cases - recoveredCases
            y[R][i] += recoveredCases
            y[cumI][i] += incidentCases

            if is_check_loop and y[cumI][i] < dynfilter[int(it % (1 / dt))][i]:
                return -np.ones((ncomp, nnodes, len(t_inter)))

            states[S, i, it] = y[S][i]
            states[E, i, it] = y[E][i]
            states[I1, i, it] = y[I1][i]
            states[I2, i, it] = y[I2][i]
            states[I3, i, it] = y[I3][i]
            states[R, i, it] = y[R][i]
            states[cumI, i, it] = y[cumI][i]

    return states
