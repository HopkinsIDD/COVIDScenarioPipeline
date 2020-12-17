import collections
import warnings

import confuse
import pandas as pd

from .base import NPIBase

REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]
PARALLEL_COMP_PARAMS = ["transmissibility_reduction", "susceptibility_reduction"]
PARALLEL_TRANS_PARAMS = ["transition_rate"]

"Cap on # of reduction metadata entries to store in memory"
REDUCTION_METADATA_CAP = 325


class Stacked(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df=None):
        super().__init__(name=npi_config.name)

        n_parallel_compartments = 1
        n_parallel_transitions = 0
        if "parallel_structure" in global_config["seir"]["parameters"]:
            if not "compartments" in global_config["seir"]["parameters"]["parallel_structure"]:
                raise ValueError(f"A config specifying a parallel structure should assign compartments to that structure")
            compartments_map = global_config["seir"]["parameters"]["parallel_structure"]["compartments"].get()
            n_parallel_compartments = len(compartments_map)
            compartments_dict = {k : v for v,k in enumerate(compartments_map)}
            if not "transitions" in global_config["seir"]["parameters"]["parallel_structure"]:
                raise ValueError(f"A config specifying a parallel structure should assign transitions to that structure")
            transitions_map = global_config["seir"]["parameters"]["parallel_structure"]["transitions"].get()
            n_parallel_transitions = len(transitions_map)
        self.all_parameters = REDUCE_PARAMS
        for param in PARALLEL_COMP_PARAMS:
            for compartment in range(n_parallel_compartments):
                self.all_parameters += [param + " " + str(compartment)]
        for param in PARALLEL_TRANS_PARAMS:
            for transition in range(n_parallel_transitions):
                self.all_parameters += [param + " " + str(transition)]

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.geoids = geoids
        self.reductions = {param: 1 for param in self.all_parameters}
        self.reduction_params = collections.deque()
        self.reduction_cap_exceeded = False

        # the confuse library's config resolution mechanism makes slicing the configuration object expensive; instead,
        # just preload all settings
        settings_map = global_config["interventions"]["settings"].get()
        for scenario in npi_config["scenarios"].get():
            # if it's a string, look up the scenario name's config
            if isinstance(scenario, str):
                settings = settings_map.get(scenario)
                if settings is None:
                    raise RuntimeError(f"couldn't find scenario in config file [got: {scenario}]")
                # via profiling: faster to recreate the confuse view than to fetch+resolve due to confuse isinstance
                # checks
                scenario_npi_config = confuse.RootView([settings])
                scenario_npi_config.key = scenario
            else:
                # otherwise use the specified map as the config
                scenario_npi_config = confuse.RootView([scenario])
                scenario_npi_config.key = "unnamed-{hash(scenario)}"

            sub_npi = NPIBase.execute(npi_config=scenario_npi_config, global_config=global_config, geoids=geoids,
                                      loaded_df=loaded_df)
            for param in self.all_parameters:
                reduction = sub_npi.getReduction(param, default=0.0)
                if re.match("^transition_rate \d+$",param):
                    self.reductions[param] += reduction
                else:
                    self.reductions[param] *= (1 - reduction)

            # FIXME: getReductionToWrite() returns a concat'd set of stacked scenario params, which is
            # serialized as a giant dataframe to parquet. move this writing to be incremental, but need to
            # verify there are no downstream consumers of the dataframe. in the meantime, limit the amount
            # of data we'll pin in memory
            if not self.reduction_cap_exceeded:
                if len(self.reduction_params) < REDUCTION_METADATA_CAP:
                    self.reduction_params.append(sub_npi.getReductionToWrite())
                else:
                    self.reduction_cap_exceeded = True
                    self.reduction_params.clear()

        for param in self.all_parameters:
            self.reductions[param] = 1 - self.reductions[param]

        self.__checkErrors()

    def __checkErrors(self):
        for param, reduction in self.reductions.items():
            if isinstance(reduction, pd.DataFrame) and (reduction > 1).any(axis=None):
                raise ValueError(f"The intervention in config: {self.name} has reduction of {param} which is greater than 100% reduced.")

    def getReduction(self, param, default=0.0):
        return self.reductions.get(param, default)

    def getReductionToWrite(self):
        if self.reduction_cap_exceeded:
            warnings.warn("Not writing reduction metadata (*.snpi.*) as memory buffer cap exceeded")
            return pd.DataFrame({"error": ["No reduction metadata as memory buffer cap exceeded"]})
        return pd.concat(self.reduction_params, ignore_index=True)
