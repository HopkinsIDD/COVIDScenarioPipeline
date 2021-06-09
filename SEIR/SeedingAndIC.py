import pathlib
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from . import file_paths
import confuse
import logging

logger = logging.getLogger(__name__)


class SeedingAndIC:
    def __init__(self, seeding_config: confuse.ConfigView = None, initial_conditions_config: confuse.ConfigView = None):
        self.seeding_config = seeding_config
        self.initial_conditions_config = initial_conditions_config

    def get_y0(self, sim_id):
        y0 = np.zeros((self.compartments.compartments.shape[0], self.nnodes))

        method = "Default"
        if "method" in self.initial_conditions_config.keys():
            method = self.initial_conditions_config["method"].as_str()

        if method == "Default":
            ## JK : This could be specified in the config
            y0[0, :] = self.popnodes
        elif (method == 'SetInitialConditions'):
            states = pd.read_csv(s.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
            if (states.empty):
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((ncomp, s.parameters.n_parallel_compartments, s.nnodes))

            for pl_idx, pl in enumerate(s.spatset.nodenames):
                if pl in list(states['place']):
                    states_pl = states[states['place'] == pl]
                    y0[S][0][pl_idx] = float(states_pl[states_pl['comp'] == 'S']['amount'])
                    y0[E][0][pl_idx] = float(states_pl[states_pl['comp'] == 'E']['amount'])
                    y0[I1][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I1']['amount'])
                    y0[I2][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I2']['amount'])
                    y0[I3][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I3']['amount'])
                    y0[R][0][pl_idx] = float(states_pl[states_pl['comp'] == 'R']['amount'])
                    y0[cumI][0][pl_idx] = y0[I1][0][pl_idx] + y0[I2][0][pl_idx] + y0[I3][0][pl_idx] + y0[R][0][pl_idx]
                elif s.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                else:
                    raise ValueError(
                        f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        elif (method == 'InitialConditionsFolderDraw'):
            sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
            states = pq.read_table(
                file_paths.create_file_name(s.in_run_id, s.in_prefix, sim_id + s.first_sim_index - 1,
                                            s.seeding_config["initial_file_type"], "parquet"),
            ).to_pandas()
            states = states[states["time"] == str(s.ti)]

            # if(states['p_comp'].max() > 0):
            #    raise ValueError(f"We do not currently support initial conditions with parallel compartments")
            if (states.empty):
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((ncomp, s.parameters.n_parallel_compartments, s.nnodes))

            for comp_id, compartment in enumerate(all_compartments):
                states_compartment = states[states['comp'] == compartment]
                for pl_idx, pl in enumerate(s.spatset.nodenames):
                    if pl in states.columns:
                        for p_comp_id in range(s.parameters.n_parallel_compartments):
                            y0[comp_id, p_comp_id, pl_idx] = float(
                                states_compartment[states_compartment['p_comp'] == p_comp_id][pl])
                    elif s.seeding_config["ignore_missing"].get():
                        print(
                            f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                        y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                    else:
                        raise ValueError(
                            f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        else:
            raise NotImplementedError(f"unknown initial conditions method [got: {method}]")

        return y0

    def get_seeding(self, sim_id):

        print(self.seeding_config)
        print(self.initial_conditions_config)
        method = self.seeding_config["method"].as_str()
        if method == 'NegativeBinomialDistributed':
            seeding = pd.read_csv(self.seeding_config["lambda_file"].as_str(),
                                  converters={'place': lambda x: str(x)},
                                  parse_dates=['date'])

            dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
            if not dupes.empty:
                raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

            for _, row in seeding.iterrows():
                if row['place'] not in self.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                importation[(row['date'].date() - self.ti).days][self.spatset.nodenames.index(row['place'])] = \
                    np.random.negative_binomial(n=5, p=5 / (row['amount'] + 5))

        if method == 'PoissonDistributed':
            seeding = pd.read_csv(self.seeding_config["lambda_file"].as_str(),
                                  converters={'place': lambda x: str(x)},
                                  parse_dates=['date'])

            dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
            if not dupes.empty:
                raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

            for _, row in seeding.iterrows():
                if row['place'] not in self.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                importation[(row['date'].date() - self.ti).days][self.spatset.nodenames.index(row['place'])] = \
                    np.random.poisson(row['amount'])

        elif method == 'FolderDraw':
            sim_id_str = str(sim_id + self.first_sim_index - 1).zfill(9)
            seeding = pd.read_csv(
                file_paths.create_file_name(self.in_run_id, self.in_prefix, sim_id + self.first_sim_index - 1,
                                            self.seeding_config["seeding_file_type"], "csv"),
                converters={'place': lambda x: str(x)},
                parse_dates=['date']
            )
            for _, row in seeding.iterrows():
                importation[(row['date'].date() - self.ti).days][self.spatset.nodenames.index(row['place'])] = row[
                    'amount']
        else:
            raise NotImplementedError(f"unknown seeding method [got: {method}]")

        importation_starts = np.zeros((self.t_span + 1))
        importation_data = np.zeros(len(["source_compartment", "destination_compartment", "spatial_node", "value"]),
                                    seeding.shape[0])
        print(importation.shape)

        return seeding_data, seeding_starts

    def seeding_draw(s, sim_id):
        importation = np.zeros((s.t_span + 1, s.nnodes))
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

            for _, row in seeding.iterrows():
                if row['place'] not in s.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                importation[(row['date'].date() - s.ti).days][s.spatset.nodenames.index(row['place'])] = \
                    np.random.negative_binomial(n=5, p=5 / (row['amount'] + 5))

        if (method == 'PoissonDistributed'):
            seeding = pd.read_csv(s.seeding_config["lambda_file"].as_str(),
                                  converters={'place': lambda x: str(x)},
                                  parse_dates=['date'])

            dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
            if not dupes.empty:
                raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

            for _, row in seeding.iterrows():
                if row['place'] not in s.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                importation[(row['date'].date() - s.ti).days][s.spatset.nodenames.index(row['place'])] = \
                    np.random.poisson(row['amount'])

        elif (method == 'FolderDraw'):
            sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
            seeding = pd.read_csv(
                file_paths.create_file_name(s.in_run_id, s.in_prefix, sim_id + s.first_sim_index - 1,
                                            s.seeding_config["seeding_file_type"], "csv"),
                converters={'place': lambda x: str(x)},
                parse_dates=['date']
            )
            for _, row in seeding.iterrows():
                importation[(row['date'].date() - s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']

        elif (method == 'SetInitialConditions'):
            states = pd.read_csv(s.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
            if (states.empty):
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

            for pl_idx, pl in enumerate(s.spatset.nodenames):
                if pl in list(states['place']):
                    states_pl = states[states['place'] == pl]
                    y0[S][0][pl_idx] = float(states_pl[states_pl['comp'] == 'S']['amount'])
                    y0[E][0][pl_idx] = float(states_pl[states_pl['comp'] == 'E']['amount'])
                    y0[I1][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I1']['amount'])
                    y0[I2][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I2']['amount'])
                    y0[I3][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I3']['amount'])
                    y0[R][0][pl_idx] = float(states_pl[states_pl['comp'] == 'R']['amount'])
                    y0[cumI][0][pl_idx] = y0[I1][0][pl_idx] + y0[I2][0][pl_idx] + y0[I3][0][pl_idx] + y0[R][0][pl_idx]
                elif s.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                else:
                    raise ValueError(
                        f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        elif (method == 'InitialConditionsFolderDraw'):
            sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
            states = pq.read_table(
                file_paths.create_file_name(s.in_run_id, s.in_prefix, sim_id + s.first_sim_index - 1,
                                            s.seeding_config["initial_file_type"], "parquet"),
            ).to_pandas()
            states = states[states["time"] == str(s.ti)]

            # if(states['p_comp'].max() > 0):
            #    raise ValueError(f"We do not currently support initial conditions with parallel compartments")
            if (states.empty):
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

            for comp_id, compartment in enumerate(all_compartments):
                states_compartment = states[states['comp'] == compartment]
                for pl_idx, pl in enumerate(s.spatset.nodenames):
                    if pl in states.columns:
                        for p_comp_id in range(s.params.n_parallel_compartments):
                            y0[comp_id, p_comp_id, pl_idx] = float(
                                states_compartment[states_compartment['p_comp'] == p_comp_id][pl])
                    elif s.seeding_config["ignore_missing"].get():
                        print(
                            f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                        y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                    else:
                        raise ValueError(
                            f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        else:
            raise NotImplementedError(f"unknown seeding method [got: {method}]")

        return y0, importation

    def seeding_load(s, sim_id):
        importation = np.zeros((len(self.t_inter) + 1, s.nnodes))
        y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))
        y0[S, 0, :] = s.popnodes

        method = s.seeding_config["method"].as_str()
        if (method == 'FolderDraw'):
            sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
            seeding = pd.read_csv(
                file_paths.create_file_name(s.in_run_id, s.in_prefix, sim_id + s.first_sim_index - 1,
                                            s.seeding_config["seeding_file_type"], "csv"),
                converters={'place': lambda x: str(x)},
                parse_dates=['date']
            )
            for _, row in seeding.iterrows():
                importation[(row['date'].date() - s.ti).days][s.spatset.nodenames.index(row['place'])] = row['amount']

        elif (method == 'SetInitialConditions'):
            states = pd.read_csv(s.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
            if (states.empty):
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((ncomp, s.params.n_parallel_compartments, s.nnodes))

            for pl_idx, pl in enumerate(s.spatset.nodenames):
                if pl in list(states['place']):
                    states_pl = states[states['place'] == pl]
                    y0[S][0][pl_idx] = float(states_pl[states_pl['comp'] == 'S']['amount'])
                    y0[E][0][pl_idx] = float(states_pl[states_pl['comp'] == 'E']['amount'])
                    y0[I1][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I1']['amount'])
                    y0[I2][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I2']['amount'])
                    y0[I3][0][pl_idx] = float(states_pl[states_pl['comp'] == 'I3']['amount'])
                    y0[R][0][pl_idx] = float(states_pl[states_pl['comp'] == 'R']['amount'])
                    y0[cumI][0][pl_idx] = y0[I1][0][pl_idx] + y0[I2][0][pl_idx] + y0[I3][0][pl_idx] + y0[R][0][pl_idx]
                elif s.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                else:
                    raise ValueError(
                        f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        elif (method == 'InitialConditionsFolderDraw'):
            sim_id_str = str(sim_id + s.first_sim_index - 1).zfill(9)
            states = pq.read_table(
                file_paths.create_file_name(s.in_run_id, s.in_prefix, sim_id + s.first_sim_index - 1,
                                            s.seeding_config["initial_file_type"], "parquet"),
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
                            y0[comp_id, p_comp_id, pl_idx] = float(
                                states_compartment[states_compartment['p_comp'] == p_comp_id][pl])
                    elif s.seeding_config["ignore_missing"].get():
                        print(
                            f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                        y0[S, 0, pl_idx] = s.popnodes[pl_idx]
                    else:
                        raise ValueError(
                            f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        else:
            raise NotImplementedError(
                f"Seeding method in inference run must be FolderDraw, SetInitialConditions, or InitialConditionsFolderDraw [got: {method}]")

        return y0, importation

    # Write seeding used to file
    def seeding_write(seeding, fname, extension):
        raise NotImplementedError(f"It is not yet possible to write the seeding to a file")
