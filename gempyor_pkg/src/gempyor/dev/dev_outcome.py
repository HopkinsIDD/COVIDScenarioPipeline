import numpy as np
import os
import pytest
import warnings
import shutil

import pathlib
import pyarrow as pa
import pyarrow.parquet as pq
import filecmp
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import datetime

import pytest

from . import outcomes
from .utils import config

import pandas as pd
import numpy as np
import datetime
import matplotlib.pyplot as plt
import glob, os, sys
from pathlib import Path

# import seaborn as sns
import pyarrow.parquet as pq
import click
import pyarrow as pa
from . import file_paths

config.clear()
config.read(user=False)
config.set_file("config.yml")

run_id = 333
index = 1
deathrate = "high_death_rate"
prefix = ""
stoch_traj_flag = True

outcomes.run_delayframe_outcomes(
    config,
    int(index),
    run_id,
    prefix,  # input
    int(index),
    run_id,
    prefix,  # output
    deathrate,
    nsim=1,
    n_jobs=1,
    stoch_traj_flag=stoch_traj_flag,
)
