import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq
import copy
import confuse
from numpy import ndarray

import SEIR.setup
import logging

logger = logging.getLogger(__name__)


class Parameters():
    # Minimal object to be easily picklable for // runs
    def __init__(self, parameter_config: confuse.ConfigView, config_version: str = 'old'):
        self.pconfig = parameter_config#["parameters"]
        self.pnames = []
        self.npar = len(self.pnames)

        self.pdata = {}
        self.pnames2pindex = {}
        self.intervention_overlap_operation = {'sum': [], 'prod': []}

        if config_version == 'v2':
            self.pnames = self.pconfig.keys()
            self.npar = len(self.pnames)
            if self.npar != len(set([name.lower() for name in self.pnames])):
                raise ValueError(
                    'Parameters of the SEIR model have the same name (remember that case is not sufficient!)')

            # Attributes of dictionary
            for idx, pn in enumerate(self.pnames):
                self.pnames2pindex[pn] = idx
                self.pdata[pn] = {}
                self.pdata[pn]['idx'] = idx
                self.pdata[pn]['dist'] = self.pconfig[pn]['value'].as_random_distribution()
                if self.pconfig[pn]['intervention_overlap_operation'].exists():
                    self.pdata[pn]['intervention_overlap_operation'] = self.pconfig[pn][
                        "intervention_overlap_operation"].as_str()
                else:
                    self.pdata[pn]['intervention_overlap_operation'] = 'prod'
                    logging.debug(
                        f"No 'intervention_overlap_operation' for parameter {pn}, assuming multiplicative NPIs")
                self.intervention_overlap_operation[self.pdata[pn]['intervention_overlap_operation']].append(pn.lower())

        elif config_version == 'old':
            n_parallel_compartments = 1
            n_parallel_transitions = 0
            compartments_dict = {}
            compartments_map = {}
            transition_map = {}
            if "parallel_structure" in self.pconfig:
                if "compartments" not in self.pconfig["parallel_structure"]:
                    raise ValueError(
                        f"A config specifying a parallel structure should assign compartments to that structure")
                compartments_map = self.pconfig["parallel_structure"]["compartments"]
                n_parallel_compartments = len(compartments_map.get())
                compartments_dict = {k: v for v, k in enumerate(compartments_map.get())}
                if not "transitions" in self.pconfig["parallel_structure"]:
                    raise ValueError(
                        f"A config specifying a parallel structure should assign transitions to that structure")
                transitions_map = self.pconfig["parallel_structure"]["transitions"]
                n_parallel_transitions = len(transitions_map.get())
                transition_map = transitions_map

            alpha_val = 1.0
            if "alpha" in self.pconfig:
                alpha_val = self.pconfig["alpha"].as_evaled_expression()
            sigma_val = self.pconfig["sigma"].as_evaled_expression()
            gamma_dist = self.pconfig["gamma"].as_random_distribution()
            R0s_dist = self.pconfig["R0s"].as_random_distribution()

            ### Do some conversions
            # Convert numbers to distribution like object that can be called
            alpha_dist = lambda: alpha_val
            sigma_dist = lambda: sigma_val

            p_dists = {'alpha': alpha_dist,
                       'sigma': sigma_dist,
                       'gamma': gamma_dist,
                       'R0s': R0s_dist}
            for key in p_dists:
                self.intervention_overlap_operation['prod'].append(key.lower())

            if n_parallel_compartments > 1.5:
                for compartment, index in compartments_dict.items():
                    if "susceptibility_reduction" in compartments_map[compartment]:
                        pn = f"susceptibility_reduction {index}"
                        p_dists[pn] = compartments_map[compartment]["susceptibility_reduction"].as_random_distribution()
                        self.intervention_overlap_operation['prod'].append(pn.lower())
                    else:
                        raise ValueError(f"Susceptibility Reduction not found for comp {compartment}")
                    if "transmissibility_reduction" in compartments_map[compartment]:
                        pn = f"transmissibility_reduction {index}"
                        p_dists[pn] = compartments_map[compartment][
                            "transmissibility_reduction"].as_random_distribution()
                        self.intervention_overlap_operation['prod'].append(pn.lower())
                    else:
                        raise ValueError(f"Transmissibility Reduction not found for comp {compartment}")
                for transition in range(n_parallel_transitions):
                    pn = f"transition_rate {transition}"
                    p_dists[pn] = transition_map[transition]["rate"].as_random_distribution()
                    self.intervention_overlap_operation['sum'].append(pn.lower())

            ### Build the new structure
            for idx, pn in enumerate(p_dists):
                self.pnames.append(pn)
                self.pnames2pindex[pn] = idx
                self.pdata[pn] = {}
                self.pdata[pn]['idx'] = idx
                self.pdata[pn]['dist'] = p_dists[pn]
            self.npar = len(self.pnames)
        logging.debug(f"We have {self.npar} parameter: {self.pnames}")
        logging.debug(f"Data to sample is: {self.pdata}")
        logging.debug(f"Index in arrays are: {self.pnames2pindex}")
        logging.debug(f"NPI overlap operation is {self.intervention_overlap_operation} ")

    def get_pnames2pindex(self) -> dict:
        return self.pnames2pindex

    def parameters_quick_draw(self, nt_inter: int, nnodes: int) -> ndarray:
        """
        Returns all parameter in an array. These are drawn based on the seir::parameters section of the config, passed in as p_config.
        :param nt_inter: number of time interval
        :param nnodes: number of spatial nodes
        :return:  array of shape (nparam, nt_inter, nnodes) with all parameters for all nodes and all time (same value)
        """
        param_arr = np.empty((self.npar, nt_inter, nnodes), dtype='float64')
        param_arr[:] = np.nan  # fill with NaNs so we don't fail silently

        for idx, pn in enumerate(self.pnames):
            param_arr[idx] = np.full((nt_inter, nnodes), self.pdata[pn]['dist']())

        return param_arr  # we don't store it as a member because this object needs to be small to be pickable

    def parameters_load(self, fname: str, nt_inter: int, nnodes: int, extension: str = 'parquet', ) -> ndarray:
        """
        drop-in equivalent to param_quick_draw() that take a file as written parameter_write()
        :param fname: 
        :param nt_inter: 
        :param nnodes: 
        :param extension: 
        :return: array of shape (nparam, nt_inter, nnodes) with all parameters for all nodes and all time.
        """
        if extension == "csv":
            param_df = pd.read_csv(f"{fname}.{extension}", index_label="parameter")
        elif extension == "parquet":
            param_df = pq.read_table(f"{fname}.{extension}").to_pandas()
        else:
            raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

        param_arr = np.empty((self.npar, nt_inter, nnodes), dtype='float64')
        param_arr[:] = np.nan  # fill with NaNs so we don't fail silently

        for idx, pn in enumerate(self.pnames):
            pval = float(param_df[param_df['parameter'] == pn].value)
            param_arr[idx] = np.full((nt_inter, nnodes), pval)

        return param_arr

    def parameters_write(self, p_draw: ndarray, fname: str, extension: str = 'parquet') -> pd.DataFrame:
        """
        Write parameters generated by parameters_quick_draw() to file, just the first value as they are all similar.
        :param p_draw:
        :param fname:
        :param extension:
        :return: The dataframe written to disk
        """
        out_df = pd.DataFrame([p_draw[idx, 0, 0] for idx, pn in enumerate(self.pnames)],
                              columns=["value"], index=self.pnames)

        if extension == "csv":
            out_df.to_csv(f"{fname}.{extension}", index_label="parameter")
        elif extension == "parquet":
            out_df["parameter"] = out_df.index
            pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
            pa.parquet.write_table(pa_df, f"{fname}.{extension}")
        else:
            raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

        return out_df

    def parameters_reduce(self, p_draw: ndarray, npi: object, dt: float) -> ndarray:
        """
        Params reduced according to the NPI provided.
        :param p_draw: array of shape (nparam, nt_inter, nnodes) from p_draw
        :param npi: NPI object with the reduction
        :param dt:
        :return: array of shape (nparam, nt_inter, nnodes) with all parameters for all nodes and all time, reduced
        """
        p_reduced = copy.deepcopy(p_draw)

        for idx, pn in enumerate(self.pnames):
            p_reduced[idx] = SEIR.setup._parameter_reduce(parameter=p_draw[idx],
                                                          modification=npi.getReduction(pn.lower()),
                                                          dt=dt,
                                                          method=self.pdata[pn]['intervention_overlap_operation'])

        return p_reduced