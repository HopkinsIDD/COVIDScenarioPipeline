import numpy as np
import pandas as pd
import datetime
import os

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


class SpatialSetup:
    def __init__(self, *, setup_name, folder, geodata_file, mobility_file, popnodes_key):
        self.setup_name = setup_name
        self.folder = folder
        self.data = pd.read_csv(geodata_file)
        self.mobility = np.loadtxt(mobility_file)
        self.popnodes = self.data[popnodes_key].to_numpy()
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
                 script_npi=None,
                 interactive=True,
                 write_csv=False,
                 dt=1 / 6,
                 nbetas=None):
        self.setup_name = setup_name
        self.nsim = nsim
        self.dt = dt
        self.ti = ti
        self.tf = tf
        self.script_npi = script_npi
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

    def buildIC(self, seeding_places, seeding_amount):
        self.y0 = np.zeros((ncomp, self.nnodes))
        self.y0[S, :] = self.popnodes
        for i, pl in enumerate(seeding_places):
            self.y0[S, pl] = self.popnodes[pl] - seeding_amount[i]
            self.y0[I1, pl] = seeding_amount[i]
        return self.y0

    def buildICfromfilter(self):
        y0 = np.zeros((ncomp, self.nnodes))
        draw = np.random.poisson(5 * self.dynfilter[31] + 0.1)
        y0[S, :] = self.popnodes - draw
        y0[E, :] = (draw / 4).astype(np.int)
        y0[I1, :] = (draw / 4).astype(np.int)
        y0[I2, :] = (draw / 4).astype(np.int)
        y0[I3, :] = (draw / 4).astype(np.int)
        y0[cumI, :] = (3 * draw / 4).astype(np.int)
        return y0

    def set_filter(self, dynfilter):
        self.dynfilter = dynfilter

    def load_filter(self, dynfilter_path):
        self.set_filter(np.loadtxt(dynfilter_path))


class COVID19Parameters():
    """ Class to hold parameters for COVID19 transmission.
        When temporal rates, unit is [d^-1]
    """
    def __init__(self, s):
        self.s = s
        # https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus
        # incubation period 5.2 days based on an estimate from Lauer et al. 2020
        self.sigma = 1 / 5.2

        # Number of infected compartiments
        n_Icomp = 3

        # time from symptom onset to recovery per compartiment
        self.gamma = np.random.uniform(
            1 / 6, 1 / 2.6,
            s.nbetas) * n_Icomp  # range of serial from 8.2 to 6.5

        if 'low' in s.setup_name:
            self.R0s = np.random.uniform(
                1.5, 2, s.nbetas)  # np.random.uniform(1.5, 2, nbetas)
        if 'mid' in s.setup_name:
            self.R0s = np.random.uniform(2, 3, s.nbetas)

        self.betas = np.multiply(self.R0s, self.gamma) / n_Icomp

        self.betas = np.vstack([self.betas] * len(s.t_inter))
        self.gamma = np.vstack([self.gamma] * len(s.t_inter))
        self.sigma = np.hstack([self.sigma] * len(s.t_inter))

        self.betas = np.dstack([self.betas] * s.nnodes)
        self.gamma = np.dstack([self.gamma] * s.nnodes)
        self.sigma = np.vstack([self.sigma] * s.nnodes)

    def draw(self, beta_id):
        """ for speed, to use with numba JIT compilation"""
        return (np.array([
            self.betas[:, beta_id % self.s.nbetas], self.sigma.T,
            self.gamma[:, beta_id % self.s.nbetas]
        ]))

    def addNPIfromcsv(self, filename):
        npi = pd.read_csv(filename).T
        npi.columns = npi.iloc[0]
        npi = npi.drop('Unnamed: 0')
        npi.index = pd.to_datetime(npi.index)
        npi = npi.resample(str(self.s.dt * 24) + 'H').ffill()
        for i in range(self.s.nbetas):
            self.betas[:, i, :] = np.multiply(
                self.betas[:, i, :],
                np.ones_like(self.betas[:, i, :]) - npi.to_numpy())

        print(f'>>> Added NPI as specicied in file {filename}')

    def addNPIfromR(self, npi):
        npi.index = pd.to_datetime(npi.index.astype(str))
        npi = npi.resample(str(self.s.dt * 24) + 'H').ffill()
        for i in range(self.s.nbetas):
            self.betas[:, i, :] = np.multiply(
                self.betas[:, i, :],
                np.ones_like(self.betas[:, i, :]) - npi.to_numpy())


def parameters_quick_draw(s, npi):
    sigma = 1 / 5.2
    n_Icomp = 3
    gamma = np.random.uniform(
        1 / 6, 1 / 2.6) * n_Icomp  # range of serial from 8.2 to 6.5

    if 'low' in s.setup_name:
        R0s = np.random.uniform(1.5, 2)  # np.random.uniform(1.5, 2, nbetas)
    if 'mid' in s.setup_name:
        R0s = np.random.uniform(2, 3)

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
