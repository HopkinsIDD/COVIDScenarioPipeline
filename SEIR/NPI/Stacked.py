import functools
import pandas as pd

from .base import NPIBase

REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]

class Stacked(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df = None):
        super().__init__(npi_config)

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.sub_npis = []

        # Gather parameter reductions
        reduction_lists = {}
        for param in REDUCE_PARAMS:
            reduction_lists[param] = []

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

            for p in REDUCE_PARAMS:
                reduction_lists[p].append(sub_npi.getReduction(p))

        # Calculate reductions
        self.reductions = {}
        for param, reduction_list in reduction_lists.items():
            self.reductions[param] = 1 - functools.reduce(lambda a,b : a * (1-b) , reduction_list, 1)

    def getReduction(self, param):
        return self.reductions[param]

    def getReductionToWrite(self):
        return pd.concat([sub_npi.getReductionToWrite() for sub_npi in self.sub_npis], ignore_index=True)

    def setReductionFromFile(self, npi, param):
        self.reductions[param] = npi
