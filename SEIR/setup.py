import numpy as np
import pandas as pd
import geopandas as gpd
import datetime
from shapely.geometry import Point, Polygon
import seir

ncomp = 7
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)


class Setup():
    """ 
        This class hold a setup model setup.
    """
    def __init__(self, setup_name, spatial_setup, nsim, ti, tf, 
                 interactive = True, write_csv = False, 
                 dt = 1/6, nbetas = None):
        self.setup_name = setup_name
        self.nsim = nsim
        self.dt = dt
        self.ti = ti
        self.tf = tf
        self.interactive = interactive
        self.write_csv = write_csv
        
        if nbetas is None:
            nbetas = nsim
        self.nbetas = nbetas

        self.spatset = spatial_setup

        self.build_setup()
        self.dynfilter = - np.ones((self.t_span,self.nnodes))

    def build_setup(self):
        self.t_span = (self.tf -  self.ti).days
        self.t_inter = np.arange(0, self.t_span+0.0001, self.dt)
        self.nnodes = self.spatset.nnodes
        self.popnodes = self.spatset.popnodes
        self.mobility = self.spatset.mobility

    def buildIC(self, seeding_places, seeding_amount):
        self.y0 = np.zeros((ncomp, self.nnodes))#, dtype = 'int64')
        self.y0[S,:] = self.popnodes
        for i, pl in enumerate(seeding_places):
            self.y0[S, pl] = self.popnodes[pl] - seeding_amount[i]
            self.y0[I1, pl] = seeding_amount[i]
        return self.y0

    

    def set_filter(self, dynfilter):
        self.dynfilter = dynfilter

class COVID19Parameters():
    """ Class to hold parameters for COVID19 transmission.
        When temporal rates, unit is [d^-1]
    """
    def __init__(self, scn, nbetas):
        # https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus
        # incubation period 5.2 days based on an estimate from Lauer et al. 2020
        self.sigma = 1/5.2
        # Number of infected compartiments
        n_Icomp = 3

        # time from symptom onset to recovery per compartiment
        self.gamma = 1/6 * n_Icomp
        
        if 'low' in scn: self.R0s = np.linspace(1.5, 2, nbetas)   # np.random.uniform(1.5, 2, nbetas)
        if 'mid' in scn: self.R0s = np.linspace(2, 3, nbetas)

        self.betas = self.R0s * self.gamma / n_Icomp

    def to_vector(self, beta_id):
        """ for speed, to use with numba JIT compilation"""
        return(np.array([self.betas[beta_id%len(self.betas)], self.sigma, self.gamma]))


class CaliforniaSpatialSetup():
    """
        Setup for california at the county scale.
    """
    def __init__(self):
        folder = 'california/'
        self.data = pd.read_csv(f'data/{folder}geodata.csv')
        self.mobility = np.loadtxt(f'data/{folder}mobility.txt')
        self.popnodes = self.data['new_pop'].to_numpy()
        self.nnodes = len(self.data)
        self.counties_shp = gpd.read_file(f'data/{folder}california-counties-shp/california-counties.shp')
        self.counties_shp.sort_values('GEOID', inplace=True)

class WestCoastSpatialSetup():
    """
        Setup for california at the county scale.
    """
    def __init__(self):
        folder = 'west-coast/'
        self.data = pd.read_csv(f'data/{folder}geodata.csv')
        self.mobility = np.loadtxt(f'data/{folder}mobility.txt')
        self.popnodes = self.data['pop2010'].to_numpy()
        self.nnodes = len(self.data)
        #self.counties_shp = gpd.read_file(f'data/{folder}california-counties-shp/california-counties.shp')
        #self.counties_shp.sort_values('GEOID', inplace=True)

