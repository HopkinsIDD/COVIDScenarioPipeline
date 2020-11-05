import numpy as np
import pandas as pd
import datetime

import pytest

from Outcomes import outcomes
from SEIR.utils import config

import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt
import glob, os, sys
from pathlib import Path
#import seaborn as sns
import pyarrow.parquet as pq
import click
import pyarrow as pa

### To generate files for this test, see notebook Test Outcomes  playbook.ipynb in COVID19_Maryland

geoid = ['15005', '15007', '15009', '15001', '15003']
diffI = np.arange(5)*2
date_data = datetime.date(2020,4,15)
subclasses = ['_A', '_B']

def test_outcomes_scenario():
    config.clear()
    config.read(user=False)
    config.set_file('config.yml')
    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.run_delayframe_outcomes(config, run_id, prefix, int(index), run_id, prefix, int(index), # output
                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)

    hosp = pq.read_table('model_output/hosp/000000001.1.hosp.parquet').to_pandas()
    hosp.set_index('time', drop=True, inplace = True)
    for i, place  in enumerate(geoid):
        for dt in hosp.index:
            if dt == date_data:
                assert(hosp[hosp['geoid']==place]['incidI'][dt] == diffI[i])
                assert(hosp[hosp['geoid']==place]['incidH'][dt+datetime.timedelta(7)] == diffI[i]*.1)
                assert(hosp[hosp['geoid']==place]['incidD'][dt+datetime.timedelta(2) ] == diffI[i]*.01)
                assert(hosp[hosp['geoid']==place]['incidICU'][dt+datetime.timedelta(7)] == diffI[i]*.1*.4)
                for j in range(7):
                    assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7+j)] == diffI[i]*.1)
                assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7+8)] == 0)

            elif dt < date_data:
                assert(hosp[hosp['geoid']==place]['incidH'][dt+datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['incidI'][dt] == 0)
                assert(hosp[hosp['geoid']==place]['incidD'][dt+datetime.timedelta(2)] == 0)
                assert(hosp[hosp['geoid']==place]['incidICU'][dt+datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7)] == 0)
            elif dt > (date_data + datetime.timedelta(7)):
                assert(hosp[hosp['geoid']==place]['incidH'][dt] == 0)
                assert(hosp[hosp['geoid']==place]['incidI'][dt-datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['incidD'][dt-datetime.timedelta(4)] == 0)
                assert(hosp[hosp['geoid']==place]['incidICU'][dt] == 0)
    hpar = pq.read_table('model_output/hpar/000000001.1.hpar.parquet').to_pandas()
    for i, place  in enumerate(geoid):
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidH') & (hpar['quantity'] == 'probability')]['value']) == 0.1)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidH') & (hpar['quantity'] == 'delay')]['value']) == 7)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidH') & (hpar['quantity'] == 'duration')]['value']) == 7)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidD') & (hpar['quantity'] == 'probability')]['value']) == 0.01)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidD') & (hpar['quantity'] == 'delay')]['value']) == 2)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidICU') & (hpar['quantity'] == 'probability')]['value']) == 0.4)
        assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== 'incidICU') & (hpar['quantity'] == 'delay')]['value']) == 0)


def test_outcomes_scenario_with_load():
    config.clear()
    config.read(user=False)
    config.set_file('config_load.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.run_delayframe_outcomes(config, run_id, prefix, int(index), 2, prefix, int(index), # output
                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)
    hpar_config = pq.read_table('model_output/hpar/000000001.1.hpar.parquet').to_pandas()
    hpar_rel = pq.read_table('model_output/hpar/000000001.2.hpar.parquet').to_pandas()

    for out in ['incidH', 'incidD', 'incidICU']:
        for i, place  in enumerate(geoid):
            a = hpar_rel[(hpar_rel['outcome'] == out) & (hpar_rel['geoid'] == place)]
            b = hpar_config[(hpar_rel['outcome'] == out) & (hpar_config['geoid'] == place)]
            assert(len(a)== len(b))
            for j in range(len(a)):
                if (b.iloc[j]['quantity'] in ['delay', 'duration']):
                    assert(a.iloc[j]['value'] == b.iloc[j]['value'])
                else: #probabiliy
                    if b.iloc[j]['outcome'] == 'incidD': 
                        assert(a.iloc[j]['value'] == b.iloc[j]['value']*0.01)
                    elif b.iloc[j]['outcome'] == 'incidICU': 
                        assert(a.iloc[j]['value'] ==  b.iloc[j]['value']*0.4)
                    elif b.iloc[j]['outcome'] == 'incidH': 
                        assert(a.iloc[j]['value'] == b.iloc[j]['value']*diffI[i]*0.1)


def test_outcomes_read_write_hpar():
    config.clear()
    config.read(user=False)     
    config.set_file('config_load.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.onerun_delayframe_outcomes_load_hpar(config, 2, prefix, int(index), # input
                                                        3, prefix, int(index), # output
                                                     deathrate, stoch_traj_flag)

    hpar_read = pq.read_table('model_output/hpar/000000001.2.hpar.parquet').to_pandas()
    hpar_wrote = pq.read_table('model_output/hpar/000000001.3.hpar.parquet').to_pandas()
    assert((hpar_read == hpar_wrote).all().all())  


def test_outcomes_scenario_subclasses():
    config.clear()
    config.read(user=False)
    config.set_file('config_subclasses.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.run_delayframe_outcomes(config, run_id, prefix, int(index), 10, prefix, int(index), # output
                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)
    hosp = pq.read_table('model_output/hosp/000000001.10.hosp.parquet').to_pandas()
    hosp.set_index('time', drop=True, inplace = True)

    for i, place  in enumerate(geoid):
        for dt in hosp.index:
            if dt == date_data:
                assert(hosp[hosp['geoid']==place]['incidI'][dt] == diffI[i])
                assert(hosp[hosp['geoid']==place]['incidH'][dt+datetime.timedelta(7)] == diffI[i]*.1*len(subclasses))
                assert(hosp[hosp['geoid']==place]['incidD'][dt+datetime.timedelta(2) ] == diffI[i]*.01*len(subclasses))
                assert(hosp[hosp['geoid']==place]['incidICU'][dt+datetime.timedelta(7)] == diffI[i]*.1*.4*len(subclasses))
                for j in range(7):
                    assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7+j)] == diffI[i]*.1*len(subclasses))
                assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7+8)] == 0)

            elif dt < date_data:
                assert(hosp[hosp['geoid']==place]['incidH'][dt+datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['incidI'][dt] == 0)
                assert(hosp[hosp['geoid']==place]['incidD'][dt+datetime.timedelta(2)] == 0)
                assert(hosp[hosp['geoid']==place]['incidICU'][dt+datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['hosp_curr'][dt+datetime.timedelta(7)] == 0)
            elif dt > (date_data + datetime.timedelta(7)):
                assert(hosp[hosp['geoid']==place]['incidH'][dt] == 0)
                assert(hosp[hosp['geoid']==place]['incidI'][dt-datetime.timedelta(7)] == 0)
                assert(hosp[hosp['geoid']==place]['incidD'][dt-datetime.timedelta(4)] == 0)
                assert(hosp[hosp['geoid']==place]['incidICU'][dt] == 0)
                
    for cl in subclasses:
        for i, place  in enumerate(geoid):
            for dt in hosp.index:
                if dt == date_data:
                    assert(hosp[hosp['geoid']==place][f'incidH{cl}'][dt+datetime.timedelta(7)] == diffI[i]*.1)
                    assert(hosp[hosp['geoid']==place][f'incidD{cl}'][dt+datetime.timedelta(2) ] == diffI[i]*.01)
                    assert(hosp[hosp['geoid']==place][f'incidICU{cl}'][dt+datetime.timedelta(7)] == diffI[i]*.1*.4)
                    for j in range(7):
                        assert(hosp[hosp['geoid']==place][f'hosp_curr{cl}'][dt+datetime.timedelta(7+j)] == diffI[i]*.1)
                    assert(hosp[hosp['geoid']==place][f'hosp_curr{cl}'][dt+datetime.timedelta(7+8)] == 0)

                elif dt < date_data:
                    assert(hosp[hosp['geoid']==place][f'incidH{cl}'][dt+datetime.timedelta(7)] == 0)
                    assert(hosp[hosp['geoid']==place][f'incidD{cl}'][dt+datetime.timedelta(2)] == 0)
                    assert(hosp[hosp['geoid']==place][f'incidICU{cl}'][dt+datetime.timedelta(7)] == 0)
                    assert(hosp[hosp['geoid']==place][f'hosp_curr{cl}'][dt+datetime.timedelta(7)] == 0)
                elif dt > (date_data + datetime.timedelta(7)):
                    assert(hosp[hosp['geoid']==place][f'incidH{cl}'][dt] == 0)
                    assert(hosp[hosp['geoid']==place][f'incidD{cl}'][dt-datetime.timedelta(4)] == 0)
                    assert(hosp[hosp['geoid']==place][f'incidICU{cl}'][dt] == 0)

    hpar = pq.read_table('model_output/hpar/000000001.10.hpar.parquet').to_pandas()
    for cl in subclasses:
        for i, place  in enumerate(geoid):
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidH{cl}') & (hpar['quantity'] == 'probability')]['value']) == 0.1)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidH{cl}') & (hpar['quantity'] == 'delay')]['value']) == 7)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidH{cl}') & (hpar['quantity'] == 'duration')]['value']) == 7)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidD{cl}') & (hpar['quantity'] == 'probability')]['value']) == 0.01)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidD{cl}') & (hpar['quantity'] == 'delay')]['value']) == 2)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidICU{cl}') & (hpar['quantity'] == 'probability')]['value']) == 0.4)
            assert(float(hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidICU{cl}') & (hpar['quantity'] == 'delay')]['value']) == 0)
            assert((hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidICU{cl}')]['source'] == f'incidH{cl}').all())
            assert((hpar[(hpar['geoid']== place) & (hpar['outcome']== f'incidH{cl}')]['source'] == f'incidI').all())

def test_outcomes_scenario_with_load_subclasses():
    config.clear()
    config.read(user=False)
    config.set_file('config_load_subclasses.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.run_delayframe_outcomes(config, run_id, prefix, int(index), 11, prefix, int(index), # output
                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)

    hpar_config = pq.read_table('model_output/hpar/000000001.10.hpar.parquet').to_pandas()
    hpar_rel = pq.read_table('model_output/hpar/000000001.11.hpar.parquet').to_pandas()
    for cl in subclasses:
        for out in [f'incidH{cl}', f'incidD{cl}', f'incidICU{cl}']:
            for i, place  in enumerate(geoid):
                a = hpar_rel[(hpar_rel['outcome'] == out) & (hpar_rel['geoid'] == place)]
                b = hpar_config[(hpar_rel['outcome'] == out) & (hpar_config['geoid'] == place)]
                assert(len(a)== len(b))
                for j in range(len(a)):
                    if (b.iloc[j]['quantity'] in ['delay', 'duration']):
                        assert(a.iloc[j]['value'] == b.iloc[j]['value'])
                    else: #probabiliy
                        if cl == '_A':
                            add = .05
                        elif cl == '_B':
                            add = .075
                            
                        if b.iloc[j]['outcome'] == f'incidD{cl}': 
                            assert(a.iloc[j]['value'] == b.iloc[j]['value']*0.01)
                        elif b.iloc[j]['outcome'] == f'incidICU{cl}':
                            assert(a.iloc[j]['value'] ==  b.iloc[j]['value']*0.4)
                        elif b.iloc[j]['outcome'] == f'incidH{cl}': 
                            assert(a.iloc[j]['value'] == b.iloc[j]['value']*(diffI[i]*0.1+add))

    hosp_rel = pq.read_table('model_output/hosp/000000001.11.hosp.parquet').to_pandas()
    assert((hosp_rel['incidH'] == hosp_rel['incidH_A'] + hosp_rel['incidH_B']).all())


def test_outcomes_read_write_hpar_subclasses():
    config.clear()
    config.read(user=False)
    config.set_file('config_load.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.run_delayframe_outcomes(config, run_id, prefix, int(index), 12, prefix, int(index), # output
                            deathrate, nsim=1, n_jobs=1, stoch_traj_flag = stoch_traj_flag)
    config.clear()
    config.read(user=False)
    config.set_file('config_load.yml')

    run_id = 1
    index = 1
    deathrate = 'high_death_rate'
    prefix = ''
    stoch_traj_flag = False
    outcomes.onerun_delayframe_outcomes_load_hpar(config, 12, prefix, int(index), # input
                                                        13, prefix, int(index), # output
                                                        deathrate, stoch_traj_flag)


    hpar_read = pq.read_table('model_output/hpar/000000001.12.hpar.parquet').to_pandas()
    hpar_wrote = pq.read_table('model_output/hpar/000000001.13.hpar.parquet').to_pandas()
    assert((hpar_read == hpar_wrote).all().all())

    hosp_read = pq.read_table('model_output/hosp/000000001.12.hosp.parquet').to_pandas()
    hosp_wrote = pq.read_table('model_output/hosp/000000001.13.hosp.parquet').to_pandas()
    assert((hosp_read == hosp_wrote).all().all())