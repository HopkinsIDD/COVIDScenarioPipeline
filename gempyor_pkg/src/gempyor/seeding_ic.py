import pathlib
from typing import Dict, Any, Union

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
from numba.typed import Dict
from . import file_paths
import confuse
import logging
from . import compartments
from . import setup
import numba as nb

logger = logging.getLogger(__name__)


def _DataFrame2NumbaDict(df, amounts, setup) -> nb.typed.Dict:
    if not df["date"].is_monotonic_increasing:
        raise ValueError(
            "_DataFrame2NumbaDict got an unsorted dataframe, exposing itself to non-sense"
        )

    cmp_grp_names = [
        col for col in setup.compartments.compartments.columns if col != "name"
    ]
    seeding_dict: nb.typed.Dict = nb.typed.Dict.empty(
        key_type=nb.types.unicode_type,
        value_type=nb.types.int64[:],
    )
    seeding_dict["seeding_sources"] = np.zeros(len(amounts), dtype=np.int64)
    seeding_dict["seeding_destinations"] = np.zeros(len(amounts), dtype=np.int64)
    seeding_dict["seeding_places"] = np.zeros(len(amounts), dtype=np.int64)
    seeding_amounts = np.zeros(len(amounts), dtype=np.float64)

    nb_seed_perday = np.zeros(setup.n_days, dtype=np.int64)

    for idx, (row_index, row) in enumerate(df.iterrows()):
        if row["place"] not in setup.spatset.nodenames:
            raise ValueError(
                f"Invalid place '{row['place']}' in row {row_index + 1} of seeding::lambda_file. Not found in geodata."
            )

        if (row["date"].date() - setup.ti).days >= 0:
            nb_seed_perday[(row["date"].date() - setup.ti).days] = (
                nb_seed_perday[(row["date"].date() - setup.ti).days] + 1
            )

            source_dict = {
                grp_name: row[f"source_{grp_name}"] for grp_name in cmp_grp_names
            }
            destination_dict = {
                grp_name: row[f"destination_{grp_name}"] for grp_name in cmp_grp_names
            }
            seeding_dict["seeding_sources"][idx] = setup.compartments.get_comp_idx(
                source_dict
            )
            seeding_dict["seeding_destinations"][idx] = setup.compartments.get_comp_idx(
                destination_dict
            )
            seeding_dict["seeding_places"][idx] = setup.spatset.nodenames.index(
                row["place"]
            )
            seeding_amounts[idx] = amounts[idx]
        else:
            print(
                "WARNING IGNORING SEEDING DATE PRIOR TO START_DATE. Probably caused by a bug in create seeding"
            )

    day_start_idx = np.zeros(setup.n_days + 1, dtype=np.int64)
    day_start_idx[1:] = np.cumsum(nb_seed_perday)
    seeding_dict["day_start_idx"] = day_start_idx

    return seeding_dict, seeding_amounts


class SeedingAndIC:
    def __init__(
        self,
        seeding_config: confuse.ConfigView,
        initial_conditions_config: confuse.ConfigView,
    ):
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
        elif method == "SetInitialConditions":
            # TODO: this format should allow not complete configurations
            #       - Does not support the new way of doing compartiment indexing
            logger.critical(
                "Untested method SetInitialConditions !!! Please report this messsage."
            )
            ic_df = pd.read_csv(
                self.initial_conditions_config["states_file"].as_str(),
                converters={"place": lambda x: str(x)},
            )
            if ic_df.empty:
                raise ValueError(
                    f"There is no entry for initial time ti in the provided seeding::states_file."
                )
            y0 = np.zeros((setup.compartments.compartments.shape[0], setup.nnodes))
            for pl_idx, pl in enumerate(setup.spatset.nodenames):  #
                if pl in list(ic_df["place"]):
                    states_pl = ic_df[ic_df["place"] == pl]
                    for comp_idx, comp_name in setup.compartments.compartments[
                        "name"
                    ].iteritems():
                        y0[comp_idx, pl_idx] = float(
                            states_pl[states_pl["comp"] == comp_name]["amount"]
                        )
                elif self.seeding_config["ignore_missing"].get():
                    print(
                        f"WARNING: State load does not exist for node {pl}, assuming fully susceptible population"
                    )
                    y0[0, pl_idx] = setup.popnodes[pl_idx]
                else:
                    raise ValueError(
                        f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error"
                    )

        elif method == "InitialConditionsFolderDraw":
            ic_df = setup.read_simID(
                ftype=self.initial_conditions_config["initial_file_type"], sim_id=sim_id
            )
            ic_df = ic_df[
                (ic_df["date"] == str(setup.ti))
                & (ic_df["mc_value_type"] == "prevalence")
            ]
            if ic_df.empty:
                raise ValueError(
                    f"There is no entry for initial time ti in the provided seeding::states_file."
                )

            y0 = np.zeros((setup.compartments.compartments.shape[0], setup.nnodes))
            for comp_idx, comp_name in setup.compartments.compartments[
                "name"
            ].iteritems():
                ic_df_compartment = ic_df[ic_df["mc_name"] == comp_name]
                for pl_idx, pl in enumerate(setup.spatset.nodenames):
                    if pl in ic_df.columns:
                        y0[comp_idx, pl_idx] = float(ic_df_compartment[pl])
                    elif setup.seeding_config["ignore_missing"].get():
                        logging.warning(
                            f"WARNING: State load does not exist for node {pl}, assuming fully susceptible population"
                        )
                        y0[0, pl_idx] = setup.popnodes[pl_idx]
                    else:
                        raise ValueError(
                            f"place {pl} does not exist in seeding::states_file. You can set ignore_missing=TRUE to bypass this error"
                        )
        else:
            raise NotImplementedError(
                f"unknown initial conditions method [got: {method}]"
            )
        return y0

    def draw_seeding(self, sim_id: int, setup) -> nb.typed.Dict:
        method = "NoSeeding"
        if "method" in self.seeding_config.keys():
            method = self.seeding_config["method"].as_str()

        if method == "NegativeBinomialDistributed" or method == "PoissonDistributed":
            seeding = pd.read_csv(
                self.seeding_config["lambda_file"].as_str(),
                converters={"place": lambda x: str(x)},
                parse_dates=["date"],
            )
            dupes = seeding[seeding.duplicated(["place", "date"])].index + 1
            if not dupes.empty:
                raise ValueError(
                    f"Repeated place-date in rows {dupes.tolist()} of seeding::lambda_file."
                )
        elif method == "FolderDraw":
            seeding = pd.read_csv(
                setup.get_input_filename(
                    ftype=setup.seeding_config["seeding_file_type"],
                    sim_id=sim_id,
                    extension_override="csv",
                ),
                converters={"place": lambda x: str(x)},
                parse_dates=["date"],
            )
        elif method == "NoSeeding":
            seeding = pd.DataFrame(columns=["date", "place"])
            return _DataFrame2NumbaDict(df=seeding, amounts=[], setup=setup)
        else:
            raise NotImplementedError(f"unknown seeding method [got: {method}]")

        # Sorting by date is very important here for the seeding format necessary !!!!
        seeding = seeding.sort_values(by="date", axis="index").reset_index()

        amounts = np.zeros(len(seeding))
        if method == "PoissonDistributed":
            amounts = np.random.poisson(seeding["amount"])
        elif method == "NegativeBinomialDistributed":
            amounts = np.random.negative_binomial(n=5, p=5 / (seeding["amount"] + 5))
        elif method == "FolderDraw":
            amounts = seeding["amount"]

        return _DataFrame2NumbaDict(df=seeding, amounts=amounts, setup=setup)

    def load_seeding(self, sim_id: int, setup) -> nb.typed.Dict:
        method = "NoSeeding"
        if "method" in self.seeding_config.keys():
            method = self.seeding_config["method"].as_str()
        if method not in [
            "FolderDraw",
            "SetInitialConditions",
            "InitialConditionsFolderDraw",
            "NoSeeding",
        ]:
            raise NotImplementedError(
                f"Seeding method in inference run must be FolderDraw, SetInitialConditions, or InitialConditionsFolderDraw [got: {method}]"
            )
        return self.draw_seeding(sim_id=sim_id, setup=setup)

    def load_ic(self, sim_id: int, setup) -> nb.typed.Dict:
        return self.draw_ic(sim_id=sim_id, setup=setup)

    # Write seeding used to file
    def seeding_write(self, seeding, fname, extension):
        raise NotImplementedError(
            f"It is not yet possible to write the seeding to a file"
        )
