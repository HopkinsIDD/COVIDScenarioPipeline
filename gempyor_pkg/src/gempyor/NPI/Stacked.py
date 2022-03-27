import collections
import warnings

import confuse
import pandas as pd
import os

from .base import NPIBase

debug_print = False

"Cap on # of reduction metadata entries to store in memory"

REDUCTION_METADATA_CAP = int(os.getenv("COVID_MAX_STACK_SIZE", 50000))


class Stacked(NPIBase):
    def __init__(
        self,
        *,
        npi_config,
        global_config,
        geoids,
        loaded_df=None,
        pnames_overlap_operation_sum=[],
    ):
        super().__init__(name=npi_config.name)

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.geoids = geoids
        self.param_name = []
        self.reductions = {}  # {param: 1 for param in REDUCE_PARAMS}
        self.reduction_params = collections.deque()
        self.reduction_cap_exceeded = False
        self.reduction_number = 0
        sub_npis_unique_names = []

        # the confuse library's config resolution mechanism makes slicing the configuration object expensive; instead,
        # just preload all settings
        settings_map = global_config["interventions"]["settings"].get()

        for scenario in npi_config["scenarios"].get():
            # if it's a string, look up the scenario name's config
            if isinstance(scenario, str):
                settings = settings_map.get(scenario)
                if settings is None:
                    raise RuntimeError(
                        f"couldn't find scenario in config file [got: {scenario}]"
                    )
                # via profiling: faster to recreate the confuse view than to fetch+resolve due to confuse isinstance
                # checks
                scenario_npi_config = confuse.RootView([settings])
                scenario_npi_config.key = scenario
            else:
                # otherwise use the specified map as the config
                scenario_npi_config = confuse.RootView([scenario])
                scenario_npi_config.key = "unnamed-{hash(scenario)}"

            sub_npi = NPIBase.execute(
                npi_config=scenario_npi_config,
                global_config=global_config,
                geoids=geoids,
                loaded_df=loaded_df,
            )

            new_params = sub_npi.param_name  # either a list (if stacked) or a string
            new_params = (
                [new_params] if isinstance(new_params, str) else new_params
            )  # convert to list
            # Add each parameter at first encounter
            for new_p in new_params:
                if new_p not in self.param_name:
                    self.param_name.append(new_p)
                    if (
                        new_p in pnames_overlap_operation_sum
                    ):  # re.match("^transition_rate [1234567890]+$",new_p):
                        self.reductions[new_p] = 0
                    else:
                        self.reductions[new_p] = 1

            for param in self.param_name:
                reduction = sub_npi.getReduction(param, default=0.0)
                if (
                    param in pnames_overlap_operation_sum
                ):  # re.match("^transition_rate [1234567890]+$",param):
                    self.reductions[param] += reduction
                else:
                    self.reductions[param] *= 1 - reduction

            # FIXME: getReductionToWrite() returns a concat'd set of stacked scenario params, which is
            # serialized as a giant dataframe to parquet. move this writing to be incremental, but need to
            # verify there are no downstream consumers of the dataframe. in the meantime, limit the amount
            # of data we'll pin in memory
            if not self.reduction_cap_exceeded:
                if len(self.reduction_params) < REDUCTION_METADATA_CAP:
                    sub_npi_df = sub_npi.getReductionToWrite()
                    # build a list of unique npi names
                    sub_npis_unique_names.extend(sub_npi_df["npi_name"].unique())
                    self.reduction_params.append(sub_npi_df)
                    self.reduction_number += len(self.reduction_params)
                else:
                    self.reduction_cap_exceeded = True
                    self.reduction_params.clear()

        for param in self.param_name:
            if (
                not param in pnames_overlap_operation_sum
            ):  # re.match("^transition_rate \d+$",param):
                self.reductions[param] = 1 - self.reductions[param]

        # check that no NPI is called several times, and retourn them
        if len(sub_npis_unique_names) != len(set(sub_npis_unique_names)):
            raise ValueError(
                f"Stacked NPI {self.name} calls a NPI, which calls another NPI. The NPI that is called multiple time is/are: {set([x for x in sub_npis_unique_names if sub_npis_unique_names.count(x) > 1])}"
            )

        self.__checkErrors()

    def __checkErrors(self):
        for param, reduction in self.reductions.items():
            if isinstance(reduction, pd.DataFrame) and (reduction > 1).any(axis=None):
                raise ValueError(
                    f"The intervention in config: {self.name} has reduction of {param} with value {self.reductions.get(param).max().max()} which is greater than 100% reduced."
                )

    def getReduction(self, param, default=0.0):
        return self.reductions.get(param, default)

    def getReductionToWrite(self):
        if self.reduction_cap_exceeded:
            warnings.warn(
                f"""Not writing reduction metadata (*.snpi.*) as memory buffer cap exceeded {self.reduction_number}"""
            )
            raise RuntimeError(
                "error : Not writing reduction metadata (*.snpi.*) as memory buffer cap exceeded. Try setting `export COVID_MAX_STACK_SIZE=[BIGNUMBER]`"
            )
            # return pd.DataFrame({"error": ["No reduction metadata as memory buffer cap exceeded"]})
        return pd.concat(self.reduction_params, ignore_index=True)
