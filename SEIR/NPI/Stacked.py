import functools
import pandas as pd
import pyarrow as pa

from .base import NPIBase

REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]

class Stacked(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids):
        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

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

            sub_npi = NPIBase.execute(npi_config=scenario_npi_config, global_config=global_config, geoids=geoids)

            for p in REDUCE_PARAMS:
                reduction_lists[p].append(sub_npi.getReduction(p))

        # Calculate reductions
        self.reductions = {}
        for param, reduction_list in reduction_lists.items():
            self.reductions[param] = 1 - functools.reduce(lambda a,b : a * (1-b) , reduction_list, 1)

    def getReduction(self, param):
        return self.reductions[param]

    def writeReductions(self, fname, extension):
        p_dfs = []
        for p_name, p_df in self.reductions.items():
            if (p_df == 0.0).all(axis=None):
                continue
            p_dfs.append(p_df.T.assign(parameter=p_name))
        out_df = pd.concat(p_dfs)

        if extension == "csv":
            out_df.to_csv(f"{fname}.{extension}", index_label="time")
        elif extension == "parquet":
            out_df["parameter"] = out_df.index
            out_df = pa.Table.from_pandas(out_df, preserve_index = False)
            pa.parquet.write_table(out_df,f"{fname}.{extension}")
        else:
            raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")
