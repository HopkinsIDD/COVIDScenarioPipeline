import numpy as np
import pandas as pd
import datetime
import os

from .utils import config


ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


class SpatialSetup:
    def __init__(self, *, setup_name, folder, geodata_file, mobility_file, popnodes_key, nodenames_key):
        self.setup_name = setup_name
        self.folder = folder
        self.data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)})
        self.mobility = np.loadtxt(mobility_file)
        self.popnodes = self.data[popnodes_key].to_numpy()
        self.nodenames = self.data[nodenames_key].tolist()
        self.nnodes = len(self.data)


class Setup():
    """
        This class hold a setup model setup.
    """
    def __init__(self, *,
                 setup_name,
                 spatial_setup,
                 nsim,
                 ti,
                 tf,
                 npi_scenario=None,
                 npi_config={},
                 seeding_config={},
                 interactive=True,
                 write_csv=False,
                 dt=1 / 6,
                 nbetas=None):
        self.setup_name = setup_name
        self.nsim = nsim
        self.dt = dt
        self.ti = ti
        self.tf = tf
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

    #def buildIC(self, seeding_places, seeding_amount):
    #    self.y0 = np.zeros((ncomp, self.nnodes))
    #    self.y0[S, :] = self.popnodes
    #    for i, pl in enumerate(seeding_places):
    #        self.y0[S, pl] = self.popnodes[pl] - seeding_amount[i]
    #        self.y0[I1, pl] = seeding_amount[i]
    #    return self.y0

    #def buildICfromfilter(self):
    #    y0 = np.zeros((ncomp, self.nnodes))
    #    draw = np.random.poisson(5 * self.dynfilter[31] + 0.1)
    #    y0[S, :] = self.popnodes - draw
    #    y0[E, :] = (draw / 4).astype(np.int)
    #    y0[I1, :] = (draw / 4).astype(np.int)
    #    y0[I2, :] = (draw / 4).astype(np.int)
    #    y0[I3, :] = (draw / 4).astype(np.int)
    #    y0[cumI, :] = (3 * draw / 4).astype(np.int)
    #    return y0

    def set_filter(self, dynfilter):
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
        for  _, row in seeding.iterrows():
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
    n_Icomp = 3

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
