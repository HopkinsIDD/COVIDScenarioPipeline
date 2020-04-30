import itertools
import time
import warnings

import numpy as np
import pandas as pd
import scipy
import tqdm.contrib.concurrent

from . import NPI, setup
from .utils import config
import pyarrow.parquet as pq
import pyarrow as pa

try:
    # ignore DeprecationWarning inside numba
    with warnings.catch_warnings() as w:
        warnings.simplefilter("ignore")

        from steps import steps_SEIR_nb
except ModuleNotFoundError as e:
    raise RuntimeError("Missing compiled module, please run `python setup.py install`") from e

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


def onerun_SEIR(sim_id, s):
    scipy.random.seed()

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    seeding = setup.seeding_draw(s, sim_id)

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data
    p_draw = setup.parameters_quick_draw(config["seir"]["parameters"], len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(p_draw, npi, s.dt)

    states = steps_SEIR_nb(*parameters,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices, mobility_data, s.dynfilter)

    # Tidyup data for  R, to save it:
    if (s.write_csv or s.write_parquet):
        # Write output to .snpi.*, .spar.*, and .seir.* files
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
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
        if s.write_csv:
            npi.writeReductions(f"{s.paramdir}{sim_id_str}.snpi","csv")
            setup.parameters_write(parameters, f"{s.paramdir}{sim_id_str}.spar", "csv")

            out_df.to_csv(
                f"{s.datadir}{sim_id}.seir.csv",
                index='time',
                index_label='time')
        if s.write_parquet:
            npi.writeReductions(f"{s.paramdir}{sim_id_str}.snpi", "parquet")
            setup.parameters_write(p_draw, f"{s.paramdir}{sim_id_str}.spar", "parquet")

            out_df['time'] = out_df.index
            pa_df = pa.Table.from_pandas(out_df, preserve_index = False)
            pa.parquet.write_table(pa_df,f"{s.datadir}{sim_id_str}.seir.parquet")
    return 1


def run_parallel(s, *, n_jobs=1):
    start = time.monotonic()
    sim_ids = np.arange(1, s.nsim + 1)

    if n_jobs == 1:          # run single process for debugging/profiling purposes
        for sim_id in tqdm.tqdm(sim_ids):
            onerun_SEIR(sim_id, s)
    else:
        tqdm.contrib.concurrent.process_map(onerun_SEIR, sim_ids, itertools.repeat(s),
                                            max_workers=n_jobs)

    print(f"""
>> {s.nsim} simulations completed in {time.monotonic()-start:.1f} seconds
""")

