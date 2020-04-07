import numpy as np
import pandas as pd
import datetime
import os
import scipy.sparse

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

        self.mobility = scipy.sparse.csr_matrix(np.loadtxt(mobility_file)) # K x K matrix of people moving

        # Validate mobility data
        if self.mobility.shape != (self.nnodes, self.nnodes):
             raise ValueError(f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")

        # if (self.mobility - self.mobility.T).nnz != 0:
        #     raise ValueError(f"mobility data is not symmetric.")

        # Make sure mobility values <= the population of corresponding nodes
        # tmp = self.mobility - self.popnodes.T
        # tmp = (self.mobility.T - self.popnodes).T
        # tmp[tmp < 0] = 0
        # if tmp.any():
        #     rows, cols, values = scipy.sparse.find(tmp)
        #     errmsg = ""
        #     for r,c,v in zip(rows, cols, values):
        #         errmsg += f"\n({r}, {c}) = {v} > population of one of these nodes {set([self.nodenames[r], self.popnodes[r]])}"
        # 
        #     raise ValueError(f"The following entries in the mobility data exceed the populations in geodata:{errmsg}")
        # print("HERE")


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

        if nbetas is None:
            nbetas = nsim
        self.nbetas = nbetas

        self.spatset = spatial_setup

        self.build_setup()
        self.dynfilter = -np.ones((self.t_span, self.nnodes))

        if self.write_csv:
            self.timestamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
            self.datadir = f'model_output/{self.setup_name}/'
            if not os.path.exists(self.datadir):
                os.makedirs(self.datadir)

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

def seeding_draw(s, uid):
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
        nfile = uid%len(os.listdir(folder_path)) + 1
        seeding = pd.read_csv(f'{folder_path}importation_{nfile}.csv',
                              converters={'place': lambda x: str(x)},
                              parse_dates=['date'])
        for  _, row in seeding.iterrows():
            importation[(row['date'].date()-s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']
    else:
        raise NotImplementedError(f"unknown seeding method [got: {method}]")
    return importation


def parameters_quick_draw(s, npi):
    sigma = config["seir"]["parameters"]["sigma"].as_evaled_expression()
    gamma = config["seir"]["parameters"]["gamma"].as_random_distribution()() * n_Icomp
    R0s = config["seir"]["parameters"]["R0s"].as_random_distribution()()

    beta = np.multiply(R0s, gamma) / n_Icomp

    beta = np.hstack([beta] * len(s.t_inter))
    gamma = np.hstack([gamma] * len(s.t_inter))
    sigma = np.hstack([sigma] * len(s.t_inter))

    beta = np.vstack([beta] * s.nnodes)
    gamma = np.vstack([gamma] * s.nnodes)
    sigma = np.vstack([sigma] * s.nnodes)

    npi.index = pd.to_datetime(npi.index.astype(str))
    npi = npi.resample(str(s.dt * 24) + 'H').ffill()
    beta = np.multiply(beta, np.ones_like(beta) - npi.to_numpy().T)

    return (np.array([beta.T, sigma.T, gamma.T]))
