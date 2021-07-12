import pathlib
import numpy as np
import pandas as pd
import datetime
import os
import scipy.sparse
import pyarrow as pa
import pyarrow.parquet as pq
import copy
from . import parameters
from . import seeding_ic
from .utils import config
from . import file_paths
from . import compartments
from functools import reduce
import typing
import logging

logger = logging.getLogger(__name__)

def build_smart_setup(config, npi_scenario='inference', nsim=1, index=1, run_id='', prefix=''):
    """
        a setup class where most choices are made for you already, for test or development.
        Do not rely on this.
    """
    interactive = False
    write_csv = False
    write_parquet = True
    stoch_traj_flag = True

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
    s = Setup(
        setup_name=config["name"].get() + "_" + str(npi_scenario),
        spatial_setup=setup.SpatialSetup(
            setup_name=spatial_config["setup_name"].get(),
            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
            popnodes_key=spatial_config["popnodes"].get(),
            nodenames_key=spatial_config["nodenames"].get()
        ),
        nsim=nsim,
        npi_scenario=npi_scenario,
        npi_config=config["interventions"]["settings"][npi_scenario],
        seeding_config=config["seeding"],
        initial_conditions_config=config["initial_conditions"],
        parameters_config=config["seir"]["parameters"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index=index,
        in_run_id=run_id,
        in_prefix=prefix,
        out_run_id=run_id,
        out_prefix=prefix
    )
    return s



class Setup:
    """
        This class hold a setup model setup.
    """

    def __init__(self, *,
                 setup_name,
                 spatial_setup,
                 nsim,
                 ti,  # time to start
                 tf,  # time to finish
                 npi_scenario=None,
                 config_version=None,
                 npi_config={},
                 seeding_config={},
                 initial_conditions_config={},
                 parameters_config={},
                 compartments_config={},
                 interactive=True,
                 write_csv=False,
                 write_parquet=False,
                 dt=1 / 6,  # step size, in days
                 nbetas=None,  # # of betas, which are rates of infection
                 first_sim_index=1,
                 in_run_id=None,
                 in_prefix=None,
                 out_run_id=None,
                 out_prefix=None
                 ):
        self.setup_name = setup_name
        self.nsim = nsim
        self.dt = dt
        self.ti = ti  ## we start at 00:00 on ti
        self.tf = tf  ## we end on 23:59 on tf
        if self.tf <= self.ti:
            raise ValueError("tf (time to finish) is less than or equal to ti (time to start)")
        self.npi_scenario = npi_scenario
        self.npi_config = npi_config
        self.seeding_config = seeding_config
        self.initial_conditions_config = initial_conditions_config
        self.parameters_config = parameters_config
        self.compartments_config = compartments_config
        self.interactive = interactive
        self.write_csv = write_csv
        self.write_parquet = write_parquet
        self.first_sim_index = first_sim_index

        if in_run_id is None:
            in_run_id = file_paths.run_id()
        self.in_run_id = in_run_id

        if out_run_id is None:
            out_run_id = file_paths.run_id()
        self.out_run_id = out_run_id

        if in_prefix is None:
            in_prefix = f'model_output/{setup_name}/{in_run_id}/'
        self.in_prefix = in_prefix
        if out_prefix is None:
            out_prefix = f'model_output/{setup_name}/{npi_scenario}/{out_run_id}/'
        self.out_prefix = out_prefix

        if nbetas is None:
            nbetas = nsim
        self.nbetas = nbetas

        self.spatset = spatial_setup

        self.build_setup()

        if config_version is None:
            if "compartments" in self.compartments_config.keys():
                config_version = 'v2'
            else:
                config_version = 'old'

            logging.info(f"Config version not provided, infering type {config_version}")

        if config_version != 'old' and config_version != 'v2':
            print
            raise ValueError(f"Configuration version unknown: {config_version}. "
                             f"Should be either non-specified (default: 'old'), or set to 'old' or 'v2'.")

        self.parameters = parameters.Parameters(parameter_config=self.parameters_config,
                                                config_version=config_version)
        self.seedingAndIC = seeding_ic.SeedingAndIC(seeding_config=self.seeding_config,
                                                      initial_conditions_config=self.initial_conditions_config)
        self.compartments = compartments.Compartments(self.compartments_config)

        if self.write_csv or self.write_parquet:
            self.timestamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
            self.datadir = file_paths.create_dir_name(self.out_run_id, self.out_prefix, 'seir')
            os.makedirs(self.datadir, exist_ok=True)
            self.paramdir = file_paths.create_dir_name(self.out_run_id, self.out_prefix, 'spar')
            os.makedirs(self.paramdir, exist_ok=True)
            self.npidir = file_paths.create_dir_name(self.out_run_id, self.out_prefix, 'snpi')
            os.makedirs(self.npidir, exist_ok=True)

    def build_setup(self):
        self.n_days = (self.tf - self.ti).days + 1  # because we include s.ti and s.tf
        #self.t_inter = np.arange(0, (self.n_days -1) + 1e-7, self.dt)
        self.nnodes = self.spatset.nnodes
        self.popnodes = self.spatset.popnodes
        self.mobility = self.spatset.mobility


class SpatialSetup:
    def __init__(self, *, setup_name, geodata_file, mobility_file, popnodes_key, nodenames_key):
        self.setup_name = setup_name
        self.data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)})  # geoids and populations
        self.nnodes = len(self.data)  # K = # of locations

        # popnodes_key is the name of the column in geodata_file with populations
        if popnodes_key not in self.data:
            raise ValueError(f"popnodes_key: {popnodes_key} does not correspond to a column in geodata.");
        self.popnodes = self.data[popnodes_key].to_numpy()  # population
        if len(np.argwhere(self.popnodes == 0)):
            raise ValueError(
                f"There are {len(np.argwhere(self.popnodes == 0))} nodes with population zero, this is not supported.")

        # nodenames_key is the name of the column in geodata_file with geoids
        if nodenames_key not in self.data:
            raise ValueError(f"nodenames_key: {nodenames_key} does not correspond to a column in geodata.");
        self.nodenames = self.data[nodenames_key].tolist()
        if len(self.nodenames) != len(set(self.nodenames)):
            raise ValueError(f"There are duplicate nodenames in geodata.")

        mobility_file = pathlib.Path(mobility_file)
        if mobility_file.suffix == ".txt":
            print('Mobility files as matrices are not recommended. Please switch soon to long form csv files.')
            self.mobility = scipy.sparse.csr_matrix(np.loadtxt(mobility_file))  # K x K matrix of people moving
            # Validate mobility data
            if self.mobility.shape != (self.nnodes, self.nnodes):
                raise ValueError(
                    f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")

        elif mobility_file.suffix == ".csv":
            mobility_data = pd.read_csv(mobility_file, converters={"ori": str, "dest": str})
            nn_dict = {v: k for k, v in enumerate(self.nodenames)}
            mobility_data["ori_idx"] = mobility_data["ori"].apply(nn_dict.__getitem__)
            mobility_data["dest_idx"] = mobility_data["dest"].apply(nn_dict.__getitem__)
            if any(mobility_data["ori_idx"] == mobility_data["dest_idx"]):
                raise ValueError(
                    f"Mobility fluxes with same origin and destination in long form matrix. This is not supported")

            self.mobility = scipy.sparse.coo_matrix(
                (mobility_data.amount, (mobility_data.ori_idx, mobility_data.dest_idx)),
                shape=(self.nnodes, self.nnodes)).tocsr()

        elif mobility_file.suffix == ".npz":
            self.mobility = scipy.sparse.load_npz(mobility_file)
            # Validate mobility data
            if self.mobility.shape != (self.nnodes, self.nnodes):
                raise ValueError(
                    f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")
        else:
            raise ValueError(
                f"Mobility data must either be a .csv file in longform (recommended) or a .txt matrix file. Got {mobility_file}")

        # Make sure mobility values <= the population of src node
        tmp = (self.mobility.T - self.popnodes).T
        tmp[tmp < 0] = 0
        if tmp.any():
            rows, cols, values = scipy.sparse.find(tmp)
            errmsg = ""
            for r, c, v in zip(rows, cols, values):
                errmsg += f"\n({r}, {c}) = {self.mobility[r, c]} > population of '{self.nodenames[r]}' = {self.popnodes[r]}"
            raise ValueError(
                f"The following entries in the mobility data exceed the source node populations in geodata:{errmsg}")

        tmp = self.popnodes - np.squeeze(np.asarray(self.mobility.sum(axis=1)))
        tmp[tmp > 0] = 0
        if tmp.any():
            row, = np.where(tmp)
            errmsg = ""
            for r in row:
                errmsg += f"\n sum accross row {r} exceed population of node '{self.nodenames[r]}' ({self.popnodes[r]}), by {-tmp[r]}"
            raise ValueError(
                f'The following rows in the mobility data exceed the source node populations in geodata:{errmsg}')


def npi_load(fname, extension):
    # Quite ugly and should be in class NPI
    if extension == "csv":
        in_df = pd.read_csv(f"{fname}.{extension}")
    elif extension == "parquet":
        in_df = pa.parquet.read_table(f"{fname}.{extension}").to_pandas()
    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")
    return in_df


# Helper function
def _parameter_reduce(parameter: np.ndarray, modification: typing.Union[pd.DataFrame, float], method: str = "prod") -> np.ndarray:
    if isinstance(modification, pd.DataFrame):
        modification = modification.T
        modification.index = pd.to_datetime(modification.index.astype(str))
        #modification = modification.resample(str(dt * 24) + 'H').ffill().to_numpy()
        modification = modification.resample('1D').ffill().to_numpy()  # Type consistency:
    if method == "prod":
        return parameter * (1 - modification)
    elif method == "sum":
        return parameter + modification
