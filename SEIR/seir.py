import time, itertools, multiprocessing
from numba import jit, jitclass, int64, float64
import numpy as np
from rpy2 import robjects
from rpy2.robjects import pandas2ri
pandas2ri.activate()
import pandas as pd
import warnings
from rpy2.rinterface import RRuntimeWarning
warnings.filterwarnings("ignore", category=RRuntimeWarning)
import rpy2.robjects as ro
from rpy2.robjects.conversion import localconverter
r_source = robjects.r['source']
r_assign = robjects.r['assign']
r_options = robjects.r['options']
r_options(warn=-1)
from rpy2.rinterface_lib.callbacks import logger as rpy2_logger
import logging
rpy2_logger.setLevel(logging.ERROR)

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


def onerun_SEIR(s, p, uid):
    r_source('COVIDScenarioPipeline/data/NPI_Scenario1_None.R')
    npi = robjects.r['NPI'].T
    p.addNPIfromR(npi)

    r_assign('region', 'around_md')
    r_source('COVIDScenarioPipeline/R/distribute_airport_importations_to_counties.R')
    importation = robjects.r['county_importations_total']
    importation = importation.pivot(index='date', columns='fips_cty', values='importations')
    importation.index = pd.to_datetime(importation.index)
    for col in s.spatset.data['geoid']:
        if col not in importation.columns:
            importation[col] = 0
    importation = importation.reindex(sorted(importation.columns), axis=1)
    idx = pd.date_range(s.ti, s.tf)
    importation = importation.reindex(idx, fill_value=0)

    states = steps_SEIR_nb(p.to_vector(uid),
                            s.y0, 
                            uid,
                            s.dt,
                            s.t_inter,
                            s.nnodes,
                            s.popnodes,
                            s.mobility,
                            s.dynfilter,
                            importation.to_numpy())
    return states
    
def run_parallel(s, p, processes=multiprocessing.cpu_count()):   # set to 16 when running on server

    tic = time.time()
    uids = np.arange(s.nsim)

    with multiprocessing.Pool(processes=processes) as pool:
        result = pool.starmap(onerun_SEIR, zip(itertools.repeat(s),
                                               itertools.repeat(p),
                                               uids))
    print(f">>> {s.nsim}  Simulations done in {time.time()-tic} seconds...")
    return result



#@jit(float64[:,:,:](float64[:,:], float64[:], int64), nopython=True)
@jit(nopython=True)
def steps_SEIR_nb(p_vec, y0, uid, dt, t_inter, nnodes, popnodes, 
                  mobility, dynfilter, importation): 
    """ 
        Made to run just-in-time-compiled by numba, hence very descriptive and using loop,
        because loops are expanded by the compiler hence not a problem.
        as there is very few authorized function. Needs the nopython option to be fast.
    """
    np.random.seed(uid)
    t = 0

    y = np.copy(y0)
    states = np.zeros((ncomp, nnodes, len(t_inter)))
    
    mv = np.empty(ncomp-1)
    exposeCases = np.empty(nnodes)
    incidentCases = np.empty(nnodes)
    incident2Cases = np.empty(nnodes)
    incident3Cases = np.empty(nnodes)
    recoveredCases = np.empty(nnodes)
    
    p_infect =    1 - np.exp(-dt*p_vec[1][0][0]) 
    p_recover =   1 - np.exp(-dt*p_vec[2][0][0])
    
    for it, t in enumerate(t_inter):
        if (it%int(1/dt)==0):
            y[E] =  y[E] + importation[int(t)]
        for ori in range(nnodes):
            for dest in range(nnodes):
                for c in range(ncomp-1):
                    mv[c] = np.random.binomial(y[c,ori], 1 - np.exp(-dt*mobility[ori, dest]/popnodes[ori]))
                y[:-1,dest] += mv
                y[:-1,ori] -= mv


        p_expose =   1 - np.exp(-dt*p_vec[0][it]*(y[I1]+y[I2]+y[I3])/popnodes)  # vector

        for i in range(nnodes):
            exposeCases[i] =    np.random.binomial(y[S][i],  p_expose[i])
            incidentCases[i] =  np.random.binomial(y[E][i],  p_infect)
            incident2Cases[i] = np.random.binomial(y[I1][i], p_recover)
            incident3Cases[i] = np.random.binomial(y[I2][i], p_recover)
            recoveredCases[i] = np.random.binomial(y[I3][i], p_recover)
            
        y[S]    += -exposeCases
        y[E]    += exposeCases - incidentCases
        y[I1]   += incidentCases - incident2Cases
        y[I2]   += incident2Cases - incident3Cases
        y[I3]   += incident3Cases - recoveredCases
        y[R]    += recoveredCases
        y[cumI] += incidentCases
        states[:,:,it] = y
        if (it%int(1/dt)==0):
            y[cumI] += importation[int(t)]
        if (it%(1/dt) == 0 and (y[cumI] <= dynfilter[int(it%(1/dt))]).any()):
                return -np.ones((ncomp, nnodes, len(t_inter)))

    return states
    
