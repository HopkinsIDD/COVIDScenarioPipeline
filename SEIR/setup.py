import pathlib
import numpy as np
import pandas as pd
import datetime
import os
import scipy.sparse
import pyarrow as pa
import pyarrow.parquet as pq
import copy

from .utils import config
from . import file_paths
import logging
logger = logging.getLogger(__name__)


# Number of components
ncomp = 7
# Number of infection components
n_Icomp = 3
S, E, I1, I2, I3, R, cumI = np.arange(ncomp)
all_compartments = ("S", "E", "I1", "I2", "I3", "R", "cumI")  # beware, order is important here


class SpatialSetup:
    def __init__(self, *, setup_name, geodata_file, mobility_file, popnodes_key, nodenames_key):
        self.setup_name = setup_name
        self.data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)}) # geoids and populations
        self.nnodes = len(self.data) # K = # of locations

        # popnodes_key is the name of the column in geodata_file with populations
        if popnodes_key not in self.data:
            raise ValueError(f"popnodes_key: {popnodes_key} does not correspond to a column in geodata.");
        self.popnodes = self.data[popnodes_key].to_numpy() # population
        if len(np.argwhere(self.popnodes == 0)):
            raise ValueError(f"There are {len(np.argwhere(self.popnodes == 0))} nodes with population zero, this is not supported.")

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
                raise ValueError(f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")

        elif mobility_file.suffix == ".csv":
            mobility_data = pd.read_csv(mobility_file, converters={"ori": str, "dest": str})
            nn_dict = {v: k for k, v in enumerate(self.nodenames)}
            mobility_data["ori_idx"] = mobility_data["ori"].apply(nn_dict.__getitem__)
            mobility_data["dest_idx"] = mobility_data["dest"].apply(nn_dict.__getitem__)
            if any(mobility_data["ori_idx"] == mobility_data["dest_idx"]):
                raise ValueError(f"Mobility fluxes with same origin and destination in long form matrix. This is not supported")

            self.mobility = scipy.sparse.coo_matrix((mobility_data.amount, (mobility_data.ori_idx, mobility_data.dest_idx)), shape=(self.nnodes, self.nnodes)).tocsr()

        elif mobility_file.suffix == ".npz":
            self.mobility = scipy.sparse.load_npz(mobility_file)
            # Validate mobility data
            if self.mobility.shape != (self.nnodes, self.nnodes):
                raise ValueError(f"mobility data must have dimensions of length of geodata ({self.nnodes}, {self.nnodes}). Actual: {self.mobility.shape}")
        else:
            raise ValueError(f"Mobility data must either be a .csv file in longform (recommended) or a .txt matrix file. Got {mobility_file}")

        # Make sure mobility values <= the population of src node
        tmp = (self.mobility.T - self.popnodes).T
        tmp[tmp < 0] = 0
        if tmp.any():
            rows, cols, values = scipy.sparse.find(tmp)
            errmsg = ""
            for r,c,v in zip(rows, cols, values):
                errmsg += f"\n({r}, {c}) = {self.mobility[r,c]} > population of '{self.nodenames[r]}' = {self.popnodes[r]}"
            raise ValueError(f"The following entries in the mobility data exceed the source node populations in geodata:{errmsg}")

        tmp = self.popnodes - np.squeeze(np.asarray(self.mobility.sum(axis=1)))
        tmp[tmp > 0] = 0
        if tmp.any():
            row, = np.where(tmp)
            errmsg = ""
            for r in row:
                errmsg += f"\n sum accross row {r} exceed population of node '{self.nodenames[r]}' ({self.popnodes[r]}), by {-tmp[r]}"
            raise ValueError(f'The following rows in the mobility data exceed the source node populations in geodata:{errmsg}')


class Setup:
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
                 parameters_config={},
                 interactive=True,
                 write_csv=False,
                 write_parquet=False,
                 dt=1 / 6, # step size, in days
                 nbetas=None, # # of betas, which are rates of infection
                 first_sim_index = 1,
                 in_run_id = None,
                 in_prefix = None,
                 out_run_id = None,
                 out_prefix = None
    ):
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
        self.parameters_config = parameters_config
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

        self.params = Parameters(self.parameters_config)

        if (self.write_csv or self.write_parquet):
            self.timestamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
            self.datadir = file_paths.create_dir_name(self.out_run_id,self.out_prefix,'seir')
            os.makedirs(self.datadir, exist_ok=True)
            self.paramdir = file_paths.create_dir_name(self.out_run_id,self.out_prefix,'spar')
            os.makedirs(self.paramdir, exist_ok=True)
            self.npidir = file_paths.create_dir_name(self.out_run_id,self.out_prefix,'snpi')
            os.makedirs(self.npidir, exist_ok=True)

    def build_setup(self):
        self.t_span = (self.tf - self.ti).days
        self.t_inter = np.arange(0, self.t_span + 0.0001, self.dt)
        self.nnodes = self.spatset.nnodes
        self.popnodes = self.spatset.popnodes
        self.mobility = self.spatset.mobility



class Parameters:
    # Mnimal object to be easily picklable for // runs
    def __init__(self, parameters_config):

        n_parallel_compartments = 1
        n_parallel_transitions = 0
        compartments_dict = {}
        self.compartments_map = {}
        self.transition_map = {}
        if "parallel_structure" in parameters_config:
            if not "compartments" in parameters_config["parallel_structure"]:
                raise ValueError(f"A config specifying a parallel structure should assign compartments to that structure")
            self.compartments_map = parameters_config["parallel_structure"]["compartments"]
            n_parallel_compartments = len(self.compartments_map.get())
            compartments_dict = {k : v for v,k in enumerate(self.compartments_map.get())}
            if not "transitions" in parameters_config["parallel_structure"]:
                raise ValueError(f"A config specifying a parallel structure should assign transitions to that structure")
            transitions_map = parameters_config["parallel_structure"]["transitions"]
            n_parallel_transitions = len(transitions_map.get())
            self.transition_map =  transitions_map

        self.n_parallel_transitions = n_parallel_transitions
        self.compartments_dict = compartments_dict
        self.n_parallel_compartments = n_parallel_compartments

        self.alpha_val = 1.0
        if "alpha" in parameters_config:
            self.alpha_val = parameters_config["alpha"].as_evaled_expression()
        self.sigma_val = parameters_config["sigma"].as_evaled_expression()
        self.gamma_dist = parameters_config["gamma"].as_random_distribution()
        self.R0s_dist = parameters_config["R0s"].as_random_distribution()


def build_smart_setup(config, npi_scenario = 'inference', nsim = 1, index = 1, run_id = '', prefix=''):
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
        parameters_config=config["seir"]["parameters"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index = index,
        in_run_id = run_id,
        in_prefix = prefix,
        out_run_id = run_id,
        out_prefix = prefix
    )
    return s


def seeding_draw(s, sim_id):
    importation = np.zeros((s.t_span+1, s.nnodes))
    y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[S, 0, :] = s.popnodes

    method = s.seeding_config["method"].as_str()
    if (method == 'NegativeBinomialDistributed'):
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
                np.random.negative_binomial(n= 5, p = 5/(row['amount'] + 5))

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
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
        seeding = pd.read_csv(
            file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id + s.first_sim_index - 1, s.seeding_config["seeding_file_type"],"csv"),
            converters={'place': lambda x: str(x)},
            parse_dates=['date']
        )
        for  _, row in seeding.iterrows():
            importation[(row['date'].date()-s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']

    elif (method == 'SetInitialConditions'):
        states = pd.read_csv(s.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
        if (states.empty):
            raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

        y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

        for pl_idx, pl in enumerate(s.spatset.nodenames):
            if pl in list(states['place']):
                states_pl = states[states['place'] == pl]
                y0[S][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'S']['amount'])
                y0[E][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'E']['amount'])
                y0[I1][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I1']['amount'])
                y0[I2][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I2']['amount'])
                y0[I3][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I3']['amount'])
                y0[R][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'R']['amount'])
                y0[cumI][0][pl_idx] = y0[I1][0][pl_idx] + y0[I2][0][pl_idx] + y0[I3][0][pl_idx] + y0[R][0][pl_idx]
            elif s.seeding_config["ignore_missing"].get():
                print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                y0[S, 0, pl_idx] = s.popnodes[pl_idx]
            else:
                raise ValueError(f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

    elif (method == 'InitialConditionsFolderDraw'):
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
        states = pq.read_table(
          file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id + s.first_sim_index - 1, s.seeding_config["initial_file_type"],"parquet"),
        ).to_pandas()
        states = states[states["time"] == str(s.ti)]

        #if(states['p_comp'].max() > 0):
        #    raise ValueError(f"We do not currently support initial conditions with parallel compartments")
        if (states.empty):
            raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

        y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

        for comp_id, compartment in enumerate(all_compartments):
            states_compartment = states[states['comp'] == compartment]
            for pl_idx, pl in enumerate(s.spatset.nodenames):
                if pl in states.columns:
                    for p_comp_id in range(s.params.n_parallel_compartments):
                        y0[comp_id, p_comp_id, pl_idx] = float(states_compartment[states_compartment['p_comp'] == p_comp_id][pl])
                elif s.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                else:
                    raise ValueError(f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

    else:
        raise NotImplementedError(f"unknown seeding method [got: {method}]")

    return y0, importation

def seeding_load(s, sim_id):
    importation = np.zeros((s.t_span+1, s.nnodes))
    y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[S, 0, :] = s.popnodes

    method = s.seeding_config["method"].as_str()
    if (method == 'FolderDraw'):
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
        seeding = pd.read_csv(
            file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id+s.first_sim_index - 1, s.seeding_config["seeding_file_type"],"csv"),
            converters={'place': lambda x: str(x)},
            parse_dates=['date']
        )
        for  _, row in seeding.iterrows():
            importation[(row['date'].date()-s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']

    elif (method == 'SetInitialConditions'):
        states = pd.read_csv(s.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
        if (states.empty):
            raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

        y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

        for pl_idx, pl in enumerate(s.spatset.nodenames):
            if pl in list(states['place']):
                states_pl = states[states['place'] == pl]
                y0[S][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'S']['amount'])
                y0[E][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'E']['amount'])
                y0[I1][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I1']['amount'])
                y0[I2][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I2']['amount'])
                y0[I3][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I3']['amount'])
                y0[R][0][pl_idx] =  float(states_pl[states_pl['comp'] == 'R']['amount'])
                y0[cumI][0][pl_idx] = y0[I1][0][pl_idx] + y0[I2][0][pl_idx] + y0[I3][0][pl_idx] + y0[R][0][pl_idx]
            elif s.seeding_config["ignore_missing"].get():
                print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                y0[S, 0, pl_idx] = s.popnodes[pl_idx]
            else:
                raise ValueError(f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")
    
    elif (method == 'InitialConditionsFolderDraw'):
        sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
        states = pq.read_table(
          file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id + s.first_sim_index - 1, s.seeding_config["initial_file_type"],"parquet"),
        ).to_pandas()
        states = states[states["time"] == str(s.ti)]

        if (states.empty):
            raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

        y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

        for comp_id, compartment in enumerate(all_compartments):
            states_compartment = states[states['comp'] == compartment]
            for pl_idx, pl in enumerate(s.spatset.nodenames):
                if pl in states.columns:
                    for p_comp_id in range(s.params.n_parallel_compartments):
                        y0[comp_id, p_comp_id, pl_idx] = float(states_compartment[states_compartment['p_comp'] == p_comp_id][pl])
                elif s.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                else:
                    raise ValueError(f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

    else:
        raise NotImplementedError(f"Seeding method in inference run must be FolderDraw, SetInitialConditions, or InitialConditionsFolderDraw [got: {method}]")

    return y0, importation

def npi_load(fname, extension):
    # Quite ugly and should be in class NPI
    if extension == "csv":
        in_df = pd.read_csv(f"{fname}.{extension}")
    elif extension == "parquet":
        in_df = pa.parquet.read_table(f"{fname}.{extension}").to_pandas()
    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")
    return in_df

# Returns alpha, beta, sigma, and gamma parameters in a tuple.
# All parameters are arrays of shape (nt_inter, nnodes).
# They are returned as a tuple because it is numba-friendly for steps_SEIR_nb().
#
# These are drawn based on the seir::parameters section of the config, passed in as p_config.
# In the config, alpha is optional with a default of 1.0.
# The other parameters sigma, gamma, and R0s are required.
def parameters_quick_draw(p, nt_inter, nnodes):
    alpha = np.full((nt_inter, nnodes), p.alpha_val)

    sigma = np.full((nt_inter, nnodes), p.sigma_val)

    #gamma = p_config["gamma"].as_random_distribution()() * n_Icomp
    gamma = np.full((nt_inter, nnodes), p.gamma_dist() * n_Icomp)

    R0s = p.R0s_dist()
    beta = R0s * gamma / n_Icomp
    beta = np.full((nt_inter, nnodes), beta)

    susceptibility_reduction = np.zeros((nt_inter, p.n_parallel_compartments, nnodes), dtype = 'float64')
    transmissibility_reduction = np.zeros((nt_inter, p.n_parallel_compartments, nnodes), dtype = 'float64')
    transition_rate = np.zeros((nt_inter, p.n_parallel_transitions, nnodes), dtype = 'float64')
    transition_from = np.zeros((p.n_parallel_transitions), dtype = 'int32')
    transition_to = np.zeros((p.n_parallel_transitions), dtype = 'int32')

    ## JK : why 1.5?
    if p.n_parallel_compartments > 1.5:
    #if"parallel_structure" in p_config:
        #for index, compartment in enumerate(p_config["parallel_structure"]["compartments"]):
        for compartment, index in p.compartments_dict.items():
            #if "susceptibility_reduction" in p_config["parallel_structure"]["compartments"][compartment]:
            if "susceptibility_reduction" in p.compartments_map[compartment]:
                susceptibility_reduction[:,index,:] = p.compartments_map[compartment]["susceptibility_reduction"].as_random_distribution()()
            else:
                raise ValueError(f"Susceptibility Reduction not found for compartment {compartment} in config {p.compartments_map}")
            if "transmissibility_reduction" in p.compartments_map[compartment]:
                transmissibility_reduction[:,index,:] = p.compartments_map[compartment]["transmissibility_reduction"].as_random_distribution()()
            else:
                raise ValueError(f"Transmissibility Reduction not found for compartment {compartment} in config {p.compartments_map}")

        for transition in range(p.n_parallel_transitions):
            transition_rate[:,transition,:] = p.transition_map[transition]["rate"].as_random_distribution()()
            from_raw =p.transition_map[transition]["from"].get()
            transition_from[transition] = p.compartments_dict[from_raw]
            to_raw = p.transition_map[transition]["to"].get()
            transition_to[transition] = p.compartments_dict[to_raw]

    # Fix me, values have misleading names
    return (
        alpha,
        beta,
        sigma,
        gamma,
        p.n_parallel_compartments,
        susceptibility_reduction,
        transmissibility_reduction,
        p.n_parallel_transitions,
        transition_rate,
        transition_from,
        transition_to
    )

# Returns alpha, beta, sigma, and gamma parameters in a tuple.
# All parameters are arrays of shape (nt_inter, nnodes).
# They are returned as a tuple because it is numba-friendly for steps_SEIR_nb().
#
# They are reduced according to the NPI provided.
def parameters_reduce(p_draw, npi, dt):
    alpha, \
        beta, \
        sigma, \
        gamma, \
        n_parallel_compartments, \
        susceptibility_reduction, \
        transmissibility_reduction, \
        n_parallel_transitions, \
        transition_rate, \
        transition_from, \
        transition_to = copy.deepcopy(p_draw)

    alpha = _parameter_reduce(alpha, npi.getReduction("alpha"), dt)
    beta = _parameter_reduce(beta, npi.getReduction("r0"), dt)
    sigma = _parameter_reduce(sigma, npi.getReduction("sigma"), dt)
    gamma = _parameter_reduce(gamma, npi.getReduction("gamma"), dt)
    for compartment in range(n_parallel_compartments):
        susceptibility_reduction[:,compartment,:] = _parameter_reduce(
            susceptibility_reduction[:,compartment,:],
            npi.getReduction("susceptibility_reduction" + " " + str(compartment)),
            dt
        )
        transmissibility_reduction[:,compartment,:] = _parameter_reduce(
            transmissibility_reduction[:,compartment,:],
            npi.getReduction("transmissibility_reduction" + " " + str(compartment)),
            dt
        )

    for transition in range(n_parallel_transitions):
        transition_rate[:,transition,:] = _parameter_reduce(
            transition_rate[:,transition,:],
            npi.getReduction("transition_rate" + " " + str(transition)),
            dt,
            "addative"
        )

    return (
        alpha,
        beta,
        sigma,
        gamma,
        n_parallel_compartments,
        susceptibility_reduction,
        transmissibility_reduction,
        n_parallel_transitions,
        transition_rate,
        transition_from,
        transition_to
    )

# Helper function
def _parameter_reduce(parameter, modification, dt, method="multiplicative"):
    if isinstance(modification, pd.DataFrame):
        modification = modification.T
        modification.index = pd.to_datetime(modification.index.astype(str))
        modification = modification.resample(str(dt * 24) + 'H').ffill().to_numpy()
    if method == "multiplicative":
        return parameter * (1 - modification)
    elif method == "addative":
        return parameter + modification


# Write parameters generated by parameters_quick_draw() to file
def parameters_write(parameters, fname, extension):
    alpha, \
        beta, \
        sigma, \
        gamma, \
        n_parallel_compartments, \
        susceptibility_reduction, \
        transmissibility_reduction, \
        n_parallel_transitions, \
        transition_rate, \
        transition_from, \
        transition_to = parameters

    out_df = pd.DataFrame([alpha[0][0],
                            beta[0][0] * n_Icomp / gamma[0][0],
                            sigma[0][0],
                            gamma[0][0] / n_Icomp,
                            n_parallel_compartments,
                            *[effect for effect in susceptibility_reduction[0,:,0]],
                            *[reduction for reduction in transmissibility_reduction[0,:,0]],
                            n_parallel_transitions,
                            *[rate for rate in transition_rate[0,:,0] ],
                            *[compartment for compartment in transition_from ],
                            *[compartment for compartment in transition_to ]
    ], \
                            index = ["alpha",
                                     "R0",
                                     "sigma",
                                     "gamma",
                                     "n_parallel_compartments",
                                     *[str(x) + " susceptibility reduction" for x in range(n_parallel_compartments)],
                                     *[str(x) + " transmissibility reduction" for x in range(n_parallel_compartments)],
                                     "n_parallel_transitions",
                                     *[str(x) + " transition rate" for x in range(n_parallel_transitions)],
                                     *[str(x) + " transition from" for x in range(n_parallel_transitions)],
                                     *[str(x) + " transition to" for x in range(n_parallel_transitions)],
                            ], columns = ["value"])

    if extension == "csv":
        out_df.to_csv(f"{fname}.{extension}", index_label="parameter")
    elif extension == "parquet":
        out_df["parameter"] = out_df.index
        pa_df = pa.Table.from_pandas(out_df, preserve_index = False)
        pa.parquet.write_table(pa_df,f"{fname}.{extension}")

    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

# Write seeding used to file
def seeding_write(seeding, fname, extension):
    raise NotImplementedError(f"It is not yet possible to write the seeding to a file")

# drop-in equivalent to param_quick_draw() that take a file as parameter_write()
def parameters_load(fname, extension, nt_inter, nnodes):
    if extension == "csv":
        pars = pd.read_csv(f"{fname}.{extension}", index_label="parameter")
    elif extension == "parquet":
        pars = pq.read_table(f"{fname}.{extension}").to_pandas()
    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

    alpha = float(pars[pars['parameter'] == 'alpha'].value)
    gamma = float(pars[pars['parameter'] == 'gamma'].value) * n_Icomp
    beta =  float(pars[pars['parameter'] == 'R0'].value) * gamma / n_Icomp
    sigma = float(pars[pars['parameter'] == 'sigma'].value)

    alpha = np.full((nt_inter, nnodes), alpha)
    beta =  np.full((nt_inter, nnodes), beta)
    sigma = np.full((nt_inter, nnodes), sigma)
    gamma = np.full((nt_inter, nnodes), gamma)


    n_parallel_compartments = int(pars[pars['parameter'] == 'n_parallel_compartments'].value)
    n_parallel_transitions = int(pars[pars['parameter'] == 'n_parallel_transitions'].value)

    susceptibility_reduction = np.ones((nt_inter, n_parallel_compartments, nnodes), dtype = 'float64')
    transmissibility_reduction = np.ones((nt_inter, n_parallel_compartments, nnodes), dtype = 'float64')
    transition_rate = np.zeros((nt_inter, n_parallel_transitions, nnodes), dtype = 'float64')
    transition_from = np.zeros((n_parallel_transitions), dtype = 'int32')
    transition_to = np.zeros((n_parallel_transitions), dtype = 'int32')

    for compartment in range(n_parallel_compartments):
        susceptibility_reduction[:,compartment,:] = \
            float(pars[pars['parameter'] == (str(compartment) + ' susceptibility reduction')].value)
        transmissibility_reduction[:,compartment,:] = \
            float(pars[pars['parameter'] == (str(compartment) + ' transmissibility reduction')].value)
    for transition in range(n_parallel_transitions):
        logging.debug(f""" all parameters are : { pars }""")
        logging.debug(f""" expected name is : { (str(transition) + " " + "transition rate") }""")
        logging.debug(f""" appropriate parameters are : { pars[pars['parameter'] == (str(transition) + " " + "transition rate")] }""")
        transition_rate[:,transition,:] = \
            float(pars[pars['parameter'] == (str(transition) + " " + "transition rate")].value)
        transition_from[transition] = \
            int(pars[pars['parameter'] == (str(transition) + " " + "transition from")].value)
        transition_to[transition] = \
            int(pars[pars['parameter'] == (str(transition) + " " + "transition to")].value)



    return (
        alpha,
        beta,
        sigma,
        gamma,
        n_parallel_compartments,
        susceptibility_reduction,
        transmissibility_reduction,
        n_parallel_transitions,
        transition_rate,
        transition_from,
        transition_to
    )
