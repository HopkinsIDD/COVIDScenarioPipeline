"""
    Add past dynamics to model_output files if the model is run with SetInitialConditions
"""

import pandas as pd
import glob, os, sys
from pathlib import Path
import pyarrow.parquet as pq
import pyarrow as pa
import numpy as np
import datetime
import click

#@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=False, 
#    help="configuration file for this simulation")

#config.set_file(config_file)
past_dynamics = pd.read_csv('data/results_decay_lsq_geodatapop_states.csv')
past_dynamics.set_index('time', drop =True)
folder = [x for x in Path('model_output/').glob('*') if not x.is_file()]

for fold in folder:
        print(f'loading {str(fold)[13:]} ... ', end = '')
        files_loaded = 0
        for filename in Path(str(fold)).rglob('*.csv'):
            sim = pd.read_csv(filename)
            c = pd.concat([past_dynamics, sim])
            c.to_csv(filename)

        for filename in Path(str(fold)).rglob('*.parquet'):
            sim = pq.read_table(filename).to_pandas()
            sim.set_index('time', drop=True)
            c = pd.concat([past_dynamics, sim])
            c['time'] = c.index
            pa_df = pa.Table.from_pandas(c, preserve_index = False)
            pa.parquet.write_table(pa_df,filename)
        print('DONE')