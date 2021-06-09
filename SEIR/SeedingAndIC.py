import pathlib
import numpy as np
import pandas as pd
import pyarrow.parquet as pq

import SEIR.compartments
from . import file_paths
import confuse
import logging
from SEIR import compartments
from SEIR import setup
import numba as nb

logger = logging.getLogger(__name__)


class SeedingAndIC:
    def __init__(self, seeding_config: confuse.ConfigView, initial_conditions_config: confuse.ConfigView):
        self.seeding_config = seeding_config
        self.initial_conditions_config = initial_conditions_config

    def draw_ic(self, sim_id: int, setup) -> np.ndarray:
        method = "Default"
        if "method" in self.initial_conditions_config.keys():
            method = self.initial_conditions_config["method"].as_str()

        if method == "Default":
            ## JK : This could be specified in the config
            y0 = np.zeros((setup.compartments.compartments.shape[0], setup.nnodes))
            y0[0, :] = setup.popnodes
        elif method == 'SetInitialConditions':
            # TODO: this format should allow not complete configurations
            #       - Does not support the new way of doing compartiment indexing
            logger.critical("Untested method SetInitialConditions !!! Please report this messsage.")
            ic_df = pd.read_csv(self.seeding_config["states_file"].as_str(), converters={'place': lambda x: str(x)})
            if ic_df.empty:
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")
            y0 = np.zeros((setup.compartments.compartments.shape[0], setup.nnodes))
            for pl_idx, pl in enumerate(setup.spatset.nodenames):  #
                if pl in list(ic_df['place']):
                    states_pl = ic_df[ic_df['place'] == pl]
                    for comp_idx, comp_name in setup.compartments.compartments['name'].iteritems():
                        y0[comp_idx, pl_idx] = float(states_pl[states_pl['comp'] == comp_name]['amount'])
                elif self.seeding_config["ignore_missing"].get():
                    print(f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                    y0[0, pl_idx] = setup.popnodes[pl_idx]
                else:
                    raise ValueError(
                        f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")

        elif method == 'InitialConditionsFolderDraw':
            logger.critical("Untested method SetInitialConditions !!! Please report this messsage.")
            ic_df = pq.read_table(
                file_paths.create_file_name(setup.in_run_id, setup.in_prefix, sim_id + setup.first_sim_index - 1,
                                            setup.seeding_config["initial_file_type"], "parquet"),
            ).to_pandas()
            ic_df = ic_df[ic_df["time"] == str(setup.ti)]
            if ic_df.empty:
                raise ValueError(f"There is no entry for initial time ti in the provided seeding::states_file.")

            y0 = np.zeros((setup.compartments.compartments.shape[0], setup.nnodes))
            for comp_idx, comp_name in setup.compartments.compartments['name'].iteritems():
                ic_df_compartment = ic_df[ic_df['comp'] == comp_name]
                for pl_idx, pl in enumerate(setup.spatset.nodenames):
                    if pl in ic_df.columns:
                        y0[comp_idx, pl_idx] = float(ic_df_compartment[pl])
                    elif setup.seeding_config["ignore_missing"].get():
                        logging.warning(
                            f'WARNING: State load does not exist for node {pl}, assuming fully susceptible population')
                        y0[0, pl_idx] = setup.popnodes[pl_idx]
                    else:
                        raise ValueError(
                            f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error")
        else:
            raise NotImplementedError(f"unknown initial conditions method [got: {method}]")
        return y0

    def draw_seeding(self, sim_id: int, setup) -> nb.typed.Dict:
        cmp_grp_names = [[col for col in setup.compartments.compartments.columns if col != 'name']]

        seeding = nb.typed.Dict.empty(
            key_type=nb.types.unicode_type,
            value_type=nb.types.float64[:],
        )

        nb_seed_perday = np.zeros(setup.t_span)

        method = self.seeding_config["method"].as_str()
        if method == 'NegativeBinomialDistributed':
            seeding = pd.read_csv(self.seeding_config["lambda_file"].as_str(),
                                  converters={'place': lambda x: str(x)},
                                  parse_dates=['date'])

            dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
            if not dupes.empty:
                raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

            seeding['seeding_source_col'] = np.zeros(len(seeding))
            seeding['seeding_destination_col'] = np.zeros(len(seeding))
            seeding['seeding_place_col'] = np.zeros(len(seeding))
            seeding['seeding_amount_col'] = np.zeros(len(seeding))

            # Sorting by date is very important here for the seeding format necessary !
            for _, row in seeding.sort_values(by='date', axis='index').iterrows():
                if row['place'] not in setup.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                nb_seed_perday[(row['date'].date() - setup.ti).days] = nb_seed_perday[
                                                                           (row['date'].date() - setup.ti).days] + 1

                source_dict = {grp_name: row[f'source_{grp_name}'] for grp_name in cmp_grp_names}
                destination_dict = {grp_name: row[f'destination_{grp_name}'] for grp_name in cmp_grp_names}
                seeding['seeding_sources'] = setup.compartment.get_comp_idx(source_dict)
                seeding['seeding_destinations'] = setup.compartment.get_comp_idx(destination_dict)
                seeding['seeding_places'] = setup.spatset.nodenames.index(row['place'])
                seeding['seeding_amounts'] = np.random.negative_binomial(n=5, p=5 / (row['amount'] + 5))

        if method == 'PoissonDistributed':
            seeding = pd.read_csv(self.seeding_config["lambda_file"].as_str(),
                                  converters={'place': lambda x: str(x)},
                                  parse_dates=['date'])

            dupes = seeding[seeding.duplicated(['place', 'date'])].index + 1
            if not dupes.empty:
                raise ValueError(f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file.")

            seeding['seeding_source_col'] = np.zeros(len(seeding))
            seeding['seeding_destination_col'] = np.zeros(len(seeding))
            seeding['seeding_place_col'] = np.zeros(len(seeding))
            seeding['seeding_amount_col'] = np.zeros(len(seeding))

            # Sorting by date is very important here for the seeding format necessary !
            for _, row in seeding.sort_values(by='date', axis='index').iterrows():
                if row['place'] not in setup.spatset.nodenames:
                    raise ValueError(
                        f"Invalid place '{row['place']}' in row {_ + 1} of seeding::lambda_file. Not found in geodata.")

                nb_seed_perday[(row['date'].date() - setup.ti).days] = nb_seed_perday[
                                                                           (row['date'].date() - setup.ti).days] + 1

                source_dict = {grp_name: row[f'source_{grp_name}'] for grp_name in cmp_grp_names}
                destination_dict = {grp_name: row[f'destination_{grp_name}'] for grp_name in cmp_grp_names}
                seeding['seeding_sources'] = setup.compartment.get_comp_idx(source_dict)
                seeding['seeding_destinations'] = setup.compartment.get_comp_idx(destination_dict)
                seeding['seeding_places'] = setup.spatset.nodenames.index(row['place'])
                seeding['seeding_amounts'] = np.random.poisson(row['amount'])

        elif method == 'FolderDraw':
            seeding = pd.read_csv(
                file_paths.create_file_name(setup.in_run_id, setup.in_prefix, sim_id + setup.first_sim_index - 1,
                                            setup.seeding_config["seeding_file_type"], "csv"),
                converters={'place': lambda x: str(x)},
                parse_dates=['date']
            )
            seeding['seeding_source_col'] = np.zeros(len(seeding))
            seeding['seeding_destination_col'] = np.zeros(len(seeding))
            seeding['seeding_place_col'] = np.zeros(len(seeding))
            seeding['seeding_amount_col'] = np.zeros(len(seeding))

            for _, row in seeding.sort_values(by='date', axis='index').iterrows():
                nb_seed_perday[(row['date'].date() - setup.ti).days] = nb_seed_perday[
                                                                           (row['date'].date() - setup.ti).days] + 1

                source_dict = {grp_name: row[f'source_{grp_name}'] for grp_name in cmp_grp_names}
                destination_dict = {grp_name: row[f'destination_{grp_name}'] for grp_name in cmp_grp_names}
                seeding['seeding_sources'] = setup.compartment.get_comp_idx(source_dict)
                seeding['seeding_destinations'] = setup.compartment.get_comp_idx(destination_dict)
                seeding['seeding_places'] = setup.spatset.nodenames.index(row['place'])
                seeding['seeding_amounts'] = row['amount']
        else:
            raise NotImplementedError(f"unknown seeding method [got: {method}]")

        return seeding


    def seeding_load(self, sim_id):
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
    def seeding_write(self, seeding, fname, extension):
        raise NotImplementedError(f"It is not yet possible to write the seeding to a file")
