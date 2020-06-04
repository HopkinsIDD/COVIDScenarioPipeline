import numpy as np
import pandas as pd
import datetime

import pytest

from Outcomes import outcomes



def test_outcomes():
    
    # Create some input from SEIR
    places = ['Paris','Lausanne','Baltimore']
    dates = pd.Series(pd.date_range("2020-01-01","2020-02-09"),name='time')
    diffI = pd.DataFrame(np.zeros((40,3), columns = places, index = dates)
    diffI['time'] = diffI.index
    
    # Config definition:
    parameters = {'T': {'source': 'incidI',        # T mimics the incidI
                            'probability': 1,
                            'delay': 0,
                            'duration': 4,
                            'duration_name': 'TCHOU' # TCHOU is incidI with duration 4  
                    },
                'G': {'source': 'T',                 # G is T delayed 5
                            'probability': 1,
                            'delay': 5},
                'V': {'source': 'G',                 # V is G delayed 3
                            'probability': 1,
                            'delay': 3},
                'TGV': {'sum': ['T','G','V']}        # TGV is sum of T,G, V
                }

    # Put in some data
    diffI['Paris'].iloc[0] = 1
    diffI['Baltimore'].iloc[5] = 10
    diffI['Lausanne'].iloc[10] = 100

    # Run the outcomes model
    out_df = outcomes.compute_all_delayframe_outcomes(parameters, diffI, places, dates)

    # Test that copie worked: T == incidI
    assert((out_df['T'] == out_df['incidI']).all)

    # Test that sum works: TGV = T + G + V
    assert((out_df['TGV'] == out_df['T'] + out_df['G']+ out_df['V']).all)

    for pl in places:
        # Filter one place out
        df = out_df[out_df['geoid'] == pl].reset_index()
        for t, d in enumerate(dates):
            if (df['incidI'][t] != 0):
                # Test that duration work: duration of TCHOU is 4 so it's incidI shifted
                assert(df['TCHOU'][t] == df['TCHOU'][t+1] == df['TCHOU'][t+2] == df['TCHOU'][t+3] == df['incidI'][t])
                # Test that shifting work
                assert(df['G'][t+5] ==df['T'][t])
                # test that nested shifting works
                assert(df['V'][t+5+3] ==df['T'][t])

    # Original data is not changed (this close the degree of freedom for that.)
    assert((pd.melt(diffI, id_vars='time', value_name = 'incidI', var_name='geoid')['incidI'] == out_df['incidI']).all)





