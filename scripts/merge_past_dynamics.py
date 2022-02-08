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
import multiprocessing
import pathlib
import time

sys.path.insert(1, os.path.join(sys.path[0], ".."))
from SEIR.utils import config
import click


config.set_file("config.yml")

spatial_config = config["spatial_setup"]
spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
geodata_file = spatial_base_path / spatial_config["geodata"].get()
nodenames_key = spatial_config["nodenames"].get()
geodata = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)})
past_dynamics = pd.read_csv("data/past_dynamics.csv", parse_dates=["time"])
# past_dynamics = past_dynamics[past_dynamics['time'] != max(past_dynamics['time'])]
folder = [x for x in Path("model_output/").glob("*") if not x.is_file()]

for fold in folder:
    print(f"loading {str(fold)[13:]} ... ", end="")
    files_loaded = 0
    for filename in Path(str(fold)).rglob("*.csv"):
        sim = pd.read_csv(filename)
        c = pd.concat([past_dynamics, sim])
        c.round().to_csv(filename)

    for filename in Path(str(fold)).rglob("*.parquet"):
        sim = pq.read_table(filename).to_pandas()
        sim = sim[sim["time"] != max(past_dynamics["time"])]
        # sim = sim.set_index('time', drop=True)
        c = pd.concat([past_dynamics, sim], ignore_index=True)
        # c['time'] = c.index
        only_in_sim = list(set(sim.columns) - set(past_dynamics.columns))
        only_in_pastdyn = list(set(past_dynamics.columns) - set(sim.columns))
        c.drop(only_in_pastdyn, inplace=True, axis=1)
        c.loc[
            (c["time"] <= max(past_dynamics["time"])) & (c["comp"] != "S"), only_in_sim
        ] = 0
        pop_ois = []
        for nd in only_in_sim:
            pop_ois.append(float(geodata[geodata["geoid"] == nd].pop2010))
        c.loc[
            (c["time"] <= max(past_dynamics["time"])) & (c["comp"] == "S"), only_in_sim
        ] = pop_ois
        pa_df = pa.Table.from_pandas(c.round(), preserve_index=False)
        pa.parquet.write_table(pa_df, filename)
    print("DONE")
