import functools
import pandas as pd

from .base import NPIBase

REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]

class Stacked(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df = None):
        super().__init__(npi_config)

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.geoids = geoids

        # Gather parameter reductions
        reduction_lists = {}
        for param in REDUCE_PARAMS:
            reduction_lists[param] = []

        self.sub_npis = []

        self.intervention_name = npi_config.name

        for scenario in npi_config["scenarios"]:
            # if it's a string, look up the scenario name's config
            if isinstance(scenario.get(), str):
                scenario_npi_config = global_config["interventions"]["settings"][scenario.get()]
                if not scenario_npi_config.exists():
                    raise RuntimeError(f"couldn't find scenario in config file [got: {scenario}]")
            else:
                # otherwise use the specified map as the config
                scenario_npi_config = scenario

            sub_npi = NPIBase.execute(npi_config=scenario_npi_config, global_config=global_config, geoids=geoids, loaded_df = loaded_df)
            self.sub_npis.append(sub_npi)


        self.reductions = {}
        for param in REDUCE_PARAMS:
            self.reductions[param] = 1 - functools.reduce(lambda a,b : a * (1 - b.getReduction(param)), self.sub_npis, 1)

        self.__checkErrors()

    def __checkErrors(self):

        # Validate
        for param in self.reductions.keys():
            if (self.reductions[param] > 1).any(axis=None):
                raise ValueError(f"The intervention in config: {self.intervention_name} has reduction of {param} which is greater than 100% reduced.")


    def getReduction(self, param):
        return self.reductions[param]

    def getReductionToWrite(self):
        return pd.concat([sub_npi.getReductionToWrite() for sub_npi in self.sub_npis], ignore_index=True)
