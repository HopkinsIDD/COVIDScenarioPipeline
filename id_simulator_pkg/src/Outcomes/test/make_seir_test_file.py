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

from Outcomes import outcomes
from SEIR.utils import config

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
from SEIR import file_paths

run_id = 110
prefix = ''
sim_id = 1

a = pd.read_parquet(file_paths.create_file_name(
    run_id,
    prefix,
    sim_id,
    'seir',
    'parquet'
))
print(a)
# created by running SEIR test_seir.py (comment line 530 to remove file tree) first
b = pd.read_parquet('../../SEIR/test/model_output/seir/000000101.test.seir.parquet')
b['15005'] = 0
b['15007'] = 0
b['15009'] = 0
b['15001'] = 0
b['15003'] = 0
b.drop(['10001', '20002'], axis=1, inplace=True)
# b.set_index('date', drop=True, inplace=True)

b = b[(b['date'] >= '2020-04-01') & (b['date'] <= '2020-05-15')]

geoid = ['15005', '15007', '15009', '15001', '15003']
diffI = np.arange(5) * 2
date_data = datetime.date(2020, 4, 15)
for i in range(5):
    b.loc[(b['value_type'] == 'incidence') & (b['date'] == str(date_data)), geoid[i]] = diffI[i]

pa_df = pa.Table.from_pandas(b, preserve_index=False)
pa.parquet.write_table(
    pa_df, 'new_test_no_vacc.parquet'
)

###
# cp new_test_no_vacc.parquet model_output/seir/000000001.1.seir.parquet
# cp new_test_no_vacc.parquet model_output/seir/000000001.105.seir.parquet
# cp new_test_no_vacc.parquet model_output/seir/000000001.106.seir.parquet
# cp new_test_no_vacc.parquet model_output/seir/000000001.12.seir.parquet
# cp new_test_no_vacc.parquet model_output/seir/000000001.2.seir.parquet

b1d = b.copy(deep=True)
b1d['mc_vaccination_stage'] = '1_dose'
b1d['mc_name'] = b1d['mc_name'].str.replace('unvaccinated','1_dose')
b = pd.concat((b, b1d))
for i in range(5):
    b.loc[(b['value_type'] == 'incidence') & (b['date'] == str(date_data)) & (b['mc_vaccination_stage'] == 'first_dose'), geoid[i]] = diffI[i] * 3

pa_df = pa.Table.from_pandas(b, preserve_index=False)
pa.parquet.write_table(
    pa_df, 'new_test_with_vacc.parquet'
)

# cp new_test_with_vacc.parquet model_output/seir/000000001.111.seir.parquet
# cp new_test_with_vacc.parquet model_output/seir/000000001.110.seir.parquet

