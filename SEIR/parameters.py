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


class Parameters:
    # Minimal object to be easily picklable for // runs
    def __init__(self, parameter_config: confuse.ConfigView, config_version: str = 'old'):
        self.pconfig = parameter_config
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
                print("HERE")
            print("DONE")

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

            self.alpha_val = 1.0
            if "alpha" in self.pconfig:
                self.alpha_val = self.pconfig["alpha"].as_evaled_expression()
            self.sigma_val = self.pconfig["sigma"].as_evaled_expression()
            gamma_dist = self.pconfig["gamma"].as_random_distribution()
            R0s_dist = self.pconfig["R0s"].as_random_distribution()

            ### Do some conversions
            # Convert numbers to distribution like object that can be called
            p_dists = {'alpha': self.picklable_lamda_alpha,
                       'sigma': self.picklable_lamda_sigma,
                       'gamma': gamma_dist,
                       'R0': R0s_dist}
            for key in p_dists:
                self.intervention_overlap_operation['prod'].append(key.lower())

            if n_parallel_compartments > 1.5:
                for compartment, index in compartments_dict.items():
                    if "susceptibility_reduction" in compartments_map[compartment]:
                        pn = f"susceptibility_reduction{index}"
                        p_dists[pn] = compartments_map[compartment]["susceptibility_reduction"].as_random_distribution()
                        self.intervention_overlap_operation['prod'].append(pn.lower())
                    else:
                        raise ValueError(f"Susceptibility Reduction not found for comp {compartment}")
                    if "transmissibility_reduction" in compartments_map[compartment]:
                        pn = f"transmissibility_reduction{index}"
                        p_dists[pn] = compartments_map[compartment][
                            "transmissibility_reduction"].as_random_distribution()
                        self.intervention_overlap_operation['prod'].append(pn.lower())
                    else:
                        raise ValueError(f"Transmissibility Reduction not found for comp {compartment}")
                for transition in range(n_parallel_transitions):
                    pn = f"transition_rate{transition}"
                    p_dists[pn] = transition_map[transition]["rate"].as_random_distribution()
                    self.intervention_overlap_operation['sum'].append(pn.lower())

            ### Build the new structure
            for idx, pn in enumerate(p_dists):
                self.pnames.append(pn)
                self.pnames2pindex[pn] = idx
                self.pdata[pn] = {}
                self.pdata[pn]['idx'] = idx
                self.pdata[pn]['dist'] = p_dists[pn]
                if 'transition_rate' not in pn:
                    self.pdata[pn]['intervention_overlap_operation'] = 'prod'
                else:
                    self.pdata[pn]['intervention_overlap_operation'] = 'sum'
            self.npar = len(self.pnames)
        logging.debug(f"We have {self.npar} parameter: {self.pnames}")
        logging.debug(f"Data to sample is: {self.pdata}")
        logging.debug(f"Index in arrays are: {self.pnames2pindex}")
        logging.debug(f"NPI overlap operation is {self.intervention_overlap_operation} ")

    def picklable_lamda_alpha(self):
        """ These two functions were lambda in __init__ before, it was more elegant. but as the object needs to be pickable,
        we cannot use second order function, hence these ugly definitions"""
        return self.alpha_val

    def picklable_lamda_sigma(self):
        return self.sigma_val

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

    def parameters_reduce(self, p_draw: ndarray, npi: object) -> ndarray:
        """
        Params reduced according to the NPI provided.
        :param p_draw: array of shape (nparam, nt_inter, nnodes) from p_draw
        :param npi: NPI object with the reduction
        :return: array of shape (nparam, nt_inter, nnodes) with all parameters for all nodes and all time, reduced
        """
        p_reduced = copy.deepcopy(p_draw)

        for idx, pn in enumerate(self.pnames):
            p_reduced[idx] = SEIR.setup._parameter_reduce(parameter=p_draw[idx],
                                                          modification=npi.getReduction(pn.lower()),
                                                          method=self.pdata[pn]['intervention_overlap_operation'])

        return p_reduced


### DEprecatated code for reference:
# Returns alpha, beta, sigma, and gamma parameters in a tuple.
# All parameters are arrays of shape (nt_inter, nnodes).
# They are returned as a tuple because it is numba-friendly for steps_SEIR_nb().
#
# These are drawn based on the seir::parameters section of the config, passed in as p_config.
# In the config, alpha is optional with a default of 1.0.
# The other parameters sigma, gamma, and R0s are required.
def parameters_quick_draw_deprecated(p, nt_inter, nnodes):
    alpha = np.full((nt_inter, nnodes), p.alpha_val)

    sigma = np.full((nt_inter, nnodes), p.sigma_val)

    # gamma = p_config["gamma"].as_random_distribution()() * n_Icomp
    gamma = np.full((nt_inter, nnodes), p.gamma_dist() * n_Icomp)

    R0s = p.R0s_dist()
    beta = R0s * gamma / n_Icomp
    beta = np.full((nt_inter, nnodes), beta)

    susceptibility_reduction = np.zeros((nt_inter, p.n_parallel_compartments, nnodes), dtype='float64')
    transmissibility_reduction = np.zeros((nt_inter, p.n_parallel_compartments, nnodes), dtype='float64')
    transition_rate = np.zeros((nt_inter, p.n_parallel_transitions, nnodes), dtype='float64')
    transition_from = np.zeros((p.n_parallel_transitions), dtype='int32')
    transition_to = np.zeros((p.n_parallel_transitions), dtype='int32')

    ## JK : why 1.5?
    if p.n_parallel_compartments > 1.5:
        # if"parallel_structure" in p_config:
        # for index, compartment in enumerate(p_config["parallel_structure"]["compartments"]):
        for compartment, index in p.compartments_dict.items():
            # if "susceptibility_reduction" in p_config["parallel_structure"]["compartments"][compartment]:
            if "susceptibility_reduction" in p.compartments_map[compartment]:
                susceptibility_reduction[:, index, :] = p.compartments_map[compartment][
                    "susceptibility_reduction"].as_random_distribution()()
            else:
                raise ValueError(
                    f"Susceptibility Reduction not found for compartment {compartment} in config {p.compartments_map}")
            if "transmissibility_reduction" in p.compartments_map[compartment]:
                transmissibility_reduction[:, index, :] = p.compartments_map[compartment][
                    "transmissibility_reduction"].as_random_distribution()()
            else:
                raise ValueError(
                    f"Transmissibility Reduction not found for compartment {compartment} in config {p.compartments_map}")

        for transition in range(p.n_parallel_transitions):
            transition_rate[:, transition, :] = p.transition_map[transition]["rate"].as_random_distribution()()
            from_raw = p.transition_map[transition]["from"].get()
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
def parameters_reduce_deprecated(p_draw, npi, dt):
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
        susceptibility_reduction[:, compartment, :] = _parameter_reduce(
            susceptibility_reduction[:, compartment, :],
            npi.getReduction("susceptibility_reduction" + " " + str(compartment)),
            dt
        )
        transmissibility_reduction[:, compartment, :] = _parameter_reduce(
            transmissibility_reduction[:, compartment, :],
            npi.getReduction("transmissibility_reduction" + " " + str(compartment)),
            dt
        )

    for transition in range(n_parallel_transitions):
        transition_rate[:, transition, :] = _parameter_reduce(
            transition_rate[:, transition, :],
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


# Write parameters generated by parameters_quick_draw() to file
def parameters_write_deprecated(parameters, fname, extension):
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
                           *[effect for effect in susceptibility_reduction[0, :, 0]],
                           *[reduction for reduction in transmissibility_reduction[0, :, 0]],
                           n_parallel_transitions,
                           *[rate for rate in transition_rate[0, :, 0]],
                           *[compartment for compartment in transition_from],
                           *[compartment for compartment in transition_to]
                           ], \
                          index=["alpha",
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
                                 ], columns=["value"])

    if extension == "csv":
        out_df.to_csv(f"{fname}.{extension}", index_label="parameter")
    elif extension == "parquet":
        out_df["parameter"] = out_df.index
        pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
        pa.parquet.write_table(pa_df, f"{fname}.{extension}")

    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")


# drop-in equivalent to param_quick_draw() that take a file as parameter_write()
def parameters_load_deprecated(fname, extension, nt_inter, nnodes):
    if extension == "csv":
        pars = pd.read_csv(f"{fname}.{extension}", index_label="parameter")
    elif extension == "parquet":
        pars = pq.read_table(f"{fname}.{extension}").to_pandas()
    else:
        raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

    alpha = float(pars[pars['parameter'] == 'alpha'].value)
    gamma = float(pars[pars['parameter'] == 'gamma'].value) * n_Icomp
    beta = float(pars[pars['parameter'] == 'R0'].value) * gamma / n_Icomp
    sigma = float(pars[pars['parameter'] == 'sigma'].value)

    alpha = np.full((nt_inter, nnodes), alpha)
    beta = np.full((nt_inter, nnodes), beta)
    sigma = np.full((nt_inter, nnodes), sigma)
    gamma = np.full((nt_inter, nnodes), gamma)

    n_parallel_compartments = int(pars[pars['parameter'] == 'n_parallel_compartments'].value)
    n_parallel_transitions = int(pars[pars['parameter'] == 'n_parallel_transitions'].value)

    susceptibility_reduction = np.ones((nt_inter, n_parallel_compartments, nnodes), dtype='float64')
    transmissibility_reduction = np.ones((nt_inter, n_parallel_compartments, nnodes), dtype='float64')
    transition_rate = np.zeros((nt_inter, n_parallel_transitions, nnodes), dtype='float64')
    transition_from = np.zeros((n_parallel_transitions), dtype='int32')
    transition_to = np.zeros((n_parallel_transitions), dtype='int32')

    for compartment in range(n_parallel_compartments):
        susceptibility_reduction[:, compartment, :] = \
            float(pars[pars['parameter'] == (str(compartment) + ' susceptibility reduction')].value)
        transmissibility_reduction[:, compartment, :] = \
            float(pars[pars['parameter'] == (str(compartment) + ' transmissibility reduction')].value)
    for transition in range(n_parallel_transitions):
        logging.debug(f""" all parameters are : {pars}""")
        logging.debug(f""" expected name is : {(str(transition) + " " + "transition rate")}""")
        logging.debug(
            f""" appropriate parameters are : {pars[pars['parameter'] == (str(transition) + " " + "transition rate")]}""")
        transition_rate[:, transition, :] = \
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


class Parameters_deprecated:
    # Mnimal object to be easily picklable for // runs
    def __init__(self, parameters_config):

        self.parameters_config = parameters_config
        n_parallel_compartments = 1
        n_parallel_transitions = 0
        compartments_dict = {}
        self.compartments_map = {}
        self.transition_map = {}
        if "parallel_structure" in parameters_config:
            if not "compartments" in parameters_config["parallel_structure"]:
                raise ValueError(
                    f"A config specifying a parallel structure should assign compartments to that structure")
            self.compartments_map = parameters_config["parallel_structure"]["compartments"]
            n_parallel_compartments = len(self.compartments_map.get())
            compartments_dict = {k: v for v, k in enumerate(self.compartments_map.get())}
            if not "transitions" in parameters_config["parallel_structure"]:
                raise ValueError(
                    f"A config specifying a parallel structure should assign transitions to that structure")
            transitions_map = parameters_config["parallel_structure"]["transitions"]
            n_parallel_transitions = len(transitions_map.get())
            self.transition_map = transitions_map

        self.n_parallel_transitions = n_parallel_transitions
        self.compartments_dict = compartments_dict
        self.n_parallel_compartments = n_parallel_compartments

        self.alpha_val = 1.0
        if "alpha" in parameters_config:
            self.alpha_val = parameters_config["alpha"].as_evaled_expression()
        self.sigma_val = parameters_config["sigma"].as_evaled_expression()
        self.gamma_dist = parameters_config["gamma"].as_random_distribution()
        self.R0s_dist = parameters_config["R0s"].as_random_distribution()
