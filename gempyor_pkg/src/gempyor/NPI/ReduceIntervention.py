import collections
import warnings

import confuse
import numpy as np
import datetime
import pandas as pd
import re
import os

from .base import NPIBase

debug_print = False

"Cap on # of reduction metadata entries to store in memory"

REDUCTION_METADATA_CAP = int(os.getenv("COVID_MAX_STACK_SIZE", 5000))


class ReduceIntervention(NPIBase):
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

        self.parameters = pd.DataFrame(
            0.0,
            index=self.geoids,
            columns=["npi_name", "start_date", "end_date", "parameter", "reduction"],
        )

        if (loaded_df is not None) and self.name in loaded_df["npi_name"].values:
            self.__createFromDf(loaded_df, npi_config)
        else:
            self.__createFromConfig(npi_config)

        # if parameters are exceeding global start/end dates, index of parameter df will be out of range so check first
        if (
            self.parameters["start_date"].min() < self.start_date
            or self.parameters["end_date"].max() > self.end_date
        ):
            raise ValueError(
                f"""{self.name} : at least one period start or end date is not between global dates"""
            )

        self.param_name = []
        self.reductions = {}
        self.reduction_params = collections.deque()

        # the confuse library's config resolution mechanism makes slicing the configuration object expensive; instead,
        # just preload all settings
        settings_map = global_config["interventions"]["settings"].get()
        scenario = npi_config["baseline_scenario"].get()
        settings = settings_map.get(scenario)
        if settings is None:
            raise RuntimeError(
                f"couldn't find baseline scenario ({scenario}) in config file for intervention {self.name}"
            )
        # via profiling: faster to recreate the confuse view than to fetch+resolve due to confuse isinstance
        # checks
        scenario_npi_config = confuse.RootView([settings])
        scenario_npi_config.key = scenario

        self.sub_npi = NPIBase.execute(
            npi_config=scenario_npi_config,
            global_config=global_config,
            geoids=geoids,
            loaded_df=loaded_df,
        )
        new_params = self.sub_npi.param_name  # either a list (if stacked) or a string
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
                    self.reductions[new_p] = 0

        # self.scenario_start_date = scenario_npi_config.start_date.as_date()
        # self.scenario_end_date = scenario_npi_config.end_date.as_date()

        if debug_print:
            for param in self.param_name:
                print(f"""{self.name} : param is {param}""")

        for param in self.param_name:
            reduction = self.sub_npi.getReduction(param, default=0.0)
            if (
                param in pnames_overlap_operation_sum
            ):  # re.match("^transition_rate [1234567890]+$",param):
                self.reductions[param] = reduction.copy()
            else:
                self.reductions[param] = reduction.copy()

        # FIXME: getReductionToWrite() returns a concat'd set of stacked scenario params, which is
        # serialized as a giant dataframe to parquet. move this writing to be incremental, but need to
        # verify there are no downstream consumers of the dataframe. in the meantime, limit the amount
        # of data we'll pin in memory
        self.reduction_params.append(self.sub_npi.getReductionToWrite())

        for index in self.parameters.index:
            for param in self.param_name:
                period_range = pd.date_range(
                    self.parameters["start_date"][index],
                    self.parameters["end_date"][index],
                )
                self.reductions[param].loc[index, period_range] *= (
                    1 - self.parameters["reduction"][index]
                )

        # self.__checkErrors()

    def __checkErrors(self):
        min_start_date = self.parameters["start_date"].min()
        max_start_date = self.parameters["start_date"].max()
        min_end_date = self.parameters["end_date"].min()
        max_end_date = self.parameters["end_date"].max()
        if not (
            (self.start_date <= min_start_date) & (max_start_date <= self.end_date)
        ):
            raise ValueError(
                f"at least one period_start_date [{min_start_date}, {max_start_date}] is not between global dates [{self.start_date}, {self.end_date}]"
            )
        if not ((self.start_date <= min_end_date) & (max_end_date <= self.end_date)):
            raise ValueError(
                f"at least one period_end_date ([{min_end_date}, {max_end_date}] is not between global dates [{self.start_date}, {self.end_date}]"
            )

        if not (self.parameters["start_date"] <= self.parameters["end_date"]).all():
            raise ValueError(
                f"at least one period_start_date is greater than the corresponding period end date"
            )

        for n in self.affected_geoids:
            if n not in self.geoids:
                raise ValueError(f"Invalid config value {n} not in geoids")

        # if not ((min_start_date >= self.scenario_start_date)):
        #     raise ValueError(f"{self.name} : at least one period_start_date occurs before the baseline intervention begins")
        # if not ((max_end_date <= self.scenario_end_date)):
        #     raise ValueError(f"{self.name} : at least one period_end_date occurs after the baseline intervention ends")

        for param, reduction in self.reductions.items():
            if isinstance(reduction, pd.DataFrame) and (reduction < 0).any(axis=None):
                raise ValueError(
                    f"The intervention in config: {self.name} has reduction of {param} with value {self.reductions.get(param).max().max()} which is greater than 100% reduced."
                )
            elif isinstance(reduction, pd.DataFrame) and (reduction > 1).any(axis=None):
                raise ValueError(
                    f"The intervention in config: {self.name} has reduction of {param} with value {self.reductions.get(param).max().max()} which is greater than 100% reduced."
                )
            elif not isinstance(reduction, pd.DataFrame):
                raise ValueError(
                    f"Testing assumes that reduction is a pandas DataFrame, but it isn't in this cases. It's value is : {reduction}"
                )

    def getReduction(self, param, default=0.0):
        return self.reductions.get(param, default)

    def getReductionToWrite(self):
        return pd.concat(self.reduction_params, ignore_index=True)

    def __createFromDf(self, loaded_df, npi_config):
        loaded_df.index = loaded_df.geoid
        loaded_df = loaded_df[loaded_df["npi_name"] == self.name]
        self.parameters = loaded_df[
            ["npi_name", "start_date", "end_date", "parameter", "reduction"]
        ].copy()

        self.parameters["start_date"] = (
            npi_config["period_start_date"].as_date()
            if npi_config["period_start_date"].exists()
            else self.start_date
        )
        self.parameters["end_date"] = (
            npi_config["period_end_date"].as_date()
            if npi_config["period_end_date"].exists()
            else self.end_date
        )

        ## This is more legible to me, but if we change it here, we should change it in __createFromConfig as well
        # if npi_config["period_start_date"].exists():
        #    self.parameters["start_date"] = [datetime.date.fromisoformat(date) for date in self.parameters["start_date"]]
        # else:
        #    self.parameters["start_date"] = self.start_date
        # if npi_config["period_end_date"].exists():
        #    self.parameters["end_date"] = [datetime.date.fromisoformat(date) for date in self.parameters["end_date"]]
        # else:
        #    self.parameters["start_date"] = self.end_date

        self.affected_geoids = set(self.parameters.index)
        # parameter name is picked from config too: (before: )
        # self.param_name = self.parameters["parameter"].unique()[0]  # [0] to convert ndarray to str
        # now:
        self.param_name = npi_config["parameter"].as_str().lower().replace(" ", "")
        self.parameters["parameter"] = self.param_name

    def __createFromConfig(self, npi_config):
        # Get name of the parameter to reduce
        self.param_name = npi_config["parameter"].as_str().lower().replace(" ", "")

        # Optional config field "affected_geoids"
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        self.affected_geoids = set(self.geoids)
        if (
            npi_config["affected_geoids"].exists()
            and npi_config["affected_geoids"].get() != "all"
        ):
            self.affected_geoids = {str(n.get()) for n in npi_config["affected_geoids"]}

        self.parameters = self.parameters[
            self.parameters.index.isin(self.affected_geoids)
        ]
        # Create reduction
        self.dist = npi_config["value"].as_random_distribution()

        self.parameters["npi_name"] = self.name
        self.parameters["start_date"] = (
            npi_config["period_start_date"].as_date()
            if npi_config["period_start_date"].exists()
            else self.start_date
        )
        self.parameters["end_date"] = (
            npi_config["period_end_date"].as_date()
            if npi_config["period_end_date"].exists()
            else self.end_date
        )
        self.parameters["parameter"] = self.param_name
        self.parameters["reduction"] = self.dist(size=self.parameters.shape[0])
