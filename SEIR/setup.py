import numpy as np
import pandas as pd
import datetime
import os
import scipy.sparse
import pyarrow as pa
import pyarrow.parquet as pq

from .utils import config


# Number of components
ncomp = 7
# Number of infection components
n_Icomp = 3
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


class SpatialSetup:
    def __init__(self, *, setup_name, geodata_file, mobility_file, popnodes_key, nodenames_key):
        self.setup_name = setup_name
        self.data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)}) # geoids and populations
        self.nnodes = len(self.data) # K = # of locations

        # popnodes_key is the name of the column in geodata_file with populations
        if popnodes_key not in self.data:
            raise ValueError(f"popnodes_key: {popnodes_key} does not correspond to a column in geodata.");
        self.popnodes = self.data[popnodes_key].to_numpy() # population

        # nodenames_key is the name of the column in geodata_file with geoids
        if nodenames_key not in self.data:
            raise ValueError(f"nodenames_key: {nodenames_key} does not correspond to a column in geodata.");
        self.nodenames = self.data[nodenames_key].tolist()
        if len(self.nodenames) != len(set(self.nodenames)):
            raise ValueError(f"There are duplicate nodenames in geodata.")


        if ('.txt' in str(mobility_file)):
            print('Mobility files as matrices are not recommended. Please switch soon to long form csv files.')
            self.mobility = scipy.sparse.csr_matrix(np.loadtxt(mobility_file)) # K x K matrix of people moving
            # Validate mobility data
            if self.mobility.shape != (self.nnodes, self.nnodes):
                raise ValueError(f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")

        elif ('.csv' in str(mobility_file)):
            print('Mobility files as matrices are not recommended. Please switch soon to long form csv files.')
            mobility_data = pd.read_csv(mobility_file, converters={'ori': lambda x: str(x), 'dest': lambda x: str(x)})
            self.mobility = scipy.sparse.csr_matrix((self.nnodes, self.nnodes))
            for index, row in mobility_data.iterrows():
                self.mobility[self.nodenames.index(row['ori']),self.nodenames.index(row['dest'])] = row['amount']
                if (self.nodenames.index(row['ori']) == self.nodenames.index(row['dest'])):
                    raise ValueError(f"Mobility fluxes with same origin and destination: '{row['ori']}' to {row['dest']} in long form matrix. This is not supported")
        else:
            raise ValueError(f"Mobility data must either be a .csv file in longform (recommended) or a .txt matrix file. Got {mobility_file}")

        # if (self.mobility - self.mobility.T).nnz != 0:
        #     raise ValueError(f"mobility data is not symmetric.")

        # Make sure mobility values <= the population of src node
        tmp = (self.mobility.T - self.popnodes).T
        tmp[tmp < 0] = 0
        if tmp.any():
            rows, cols, values = scipy.sparse.find(tmp)
            errmsg = ""
            for r,c,v in zip(rows, cols, values):
                errmsg += f"\n({r}, {c}) = {self.mobility[r,c]} > population of '{self.nodenames[r]}' = {self.popnodes[r]}"
            raise ValueError(f"The following entries in the mobility data exceed the source node populations in geodata:{errmsg}")


class Setup():
    """
        This class hold a setup model setup.
    """
    def __init__(self, *,
                 setup_name,
                 spatial_setup,
                 nsim,
                 ti, # time to start
                 tf, # time to finish
                 npi_scenario=None,
                 npi_config={},
                 seeding_config={},
                 interactive=True,
                 write_csv=False,
                 write_parquet=False,
                 dt=1 / 6, # step size, in days
                 nbetas=None): # # of betas, which are rates of infection
        self.setup_name = setup_name
        self.nsim = nsim
        self.dt = dt
        self.ti = ti
        self.tf = tf
        if self.tf <= self.ti:
            raise ValueError("tf (time to finish) is less than or equal to ti (time to start)")
        self.npi_scenario = npi_scenario
        self.npi_config = npi_config
        self.seeding_config = seeding_config
        self.interactive = interactive
        self.write_csv = write_csv
        self.write_parquet = write_parquet

        if nbetas is None:
            nbetas = nsim
        self.nbetas = nbetas

        self.spatset = spatial_setup

        self.build_setup()
        self.dynfilter = -np.ones((self.t_span, self.nnodes))

        if (self.write_csv or self.write_parquet):
            self.timestamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
            self.datadir = f'model_output/{self.setup_name}/'
            os.makedirs(self.datadir, exist_ok=True)
            self.paramdir = f'model_parameters/{self.setup_name}/'
            os.makedirs(self.paramdir, exist_ok=True)

    def build_setup(self):
        self.t_span = (self.tf - self.ti).days
        self.t_inter = np.arange(0, self.t_span + 0.0001, self.dt)
        self.nnodes = self.spatset.nnodes
        self.popnodes = self.spatset.popnodes
        self.mobility = self.spatset.mobility

    def set_filter(self, dynfilter):
        if dynfilter.shape != (self.t_span, self.nnodes):
            raise ValueError(f"Filter must have dimensions ({self.t_span}, {self.nnodes}). Actual: ({dynfilter.shape})")
        self.dynfilter = dynfilter

    def load_filter(self, dynfilter_path):
        self.set_filter(np.loadtxt(dynfilter_path))

def seeding_draw(s, sim_id):
    importation = np.zeros((s.t_span+1, s.nnodes))
    method = s.seeding_config["method"].as_str()
    if (method == 'PoissonDistributed'):
        seeding = pd.read_csv(s.seeding_config["lambda_file"].as_str(),
                              converters={'place': lambda x: str(x)},
                              parse_dates=['date'])

        dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
        if not dupes.empty:
            raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

        for  _, row in seeding.iterrows():
            if row['place'] not in s.spatset.nodenames:
                raise ValueError(f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

            importation[(row['date'].date()-s.ti).days][s.spatset.nodenames.index(row['place'])] = \
                np.random.poisson(row['amount'])

    elif (method == 'FolderDraw'):
        folder_path = s.seeding_config["folder_path"].as_str()
        seeding = pd.read_csv(f'{folder_path}importation_{sim_id}.csv',
                              converters={'place': lambda x: str(x)},
                              parse_dates=['date'])
        for  _, row in seeding.iterrows():
            importation[(row['date'].date()-s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']
    else:
        raise NotImplementedError(f"unknown seeding method [got: {method}]")
    return importation

# Returns alpha, beta, sigma, and gamma parameters in a tuple
# alpha, sigma and gamma are scalars
# alpha is percentage of day spent commuting
# beta is an array of shape (nt_inter, nnodes)
def parameters_quick_draw(p_config, nt_inter, nnodes, dt, npi):
    if nnodes <= 0 or nt_inter <= 0:
        raise ValueError("Invalid nt_inter or nnodes")

    alpha = 1.0
    if "alpha" in p_config:
        alpha = p_config["alpha"].as_evaled_expression()

    sigma = p_config["sigma"].as_evaled_expression()
    gamma = p_config["gamma"].as_random_distribution()() * n_Icomp
    R0s = p_config["R0s"].as_random_distribution()()

    beta = R0s * gamma / n_Icomp

    beta = np.full((nnodes, nt_inter), beta)

    npi.index = pd.to_datetime(npi.index.astype(str))
    npi = npi.resample(str(dt * 24) + 'H').ffill()
    beta = np.multiply(beta, np.ones_like(beta) - npi.to_numpy().T)

    return (alpha, beta.T, sigma, gamma)

def parameters_write(parameters, fname, extension):
    out_df = pd.DataFrame([parameters[0], parameters[1][0][0]*n_Icomp / parameters[3], parameters[2], parameters[3]], index = ["alpha","R0","sigma","gamma"], columns = ["value"])
    if extension == "csv":
        out_dict.to_csv(f"{fname}.{extension}", index_label="parameter")
    if extension == "parquet":
        out_df["parameter"] = out_df.index
        pa_df = pa.Table.from_pandas(out_df, preserve_index = False)
        pa.parquet.write_table(pa_df,f"{fname}.{extension}")

