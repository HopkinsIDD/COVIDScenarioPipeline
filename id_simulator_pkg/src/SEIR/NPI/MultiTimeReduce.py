import pandas as pd
import numpy as np
import datetime

from .base import NPIBase


class MultiTimeReduce(NPIBase):
    def __init__(
        self,
        *,
        npi_config,
        global_config,
        geoids,
        loaded_df=None,
        pnames_overlap_operation_sum=[],
        sanitize=False,
    ):
        super().__init__(
            name=getattr(
                npi_config,
                "key",
                (npi_config["scenario"].exists() and npi_config["scenario"].get())
                or "unknown",
            )
        )

        self.sanitize = sanitize
        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.geoids = geoids

        self.npi = pd.DataFrame(
            0.0,
            index=self.geoids,
            columns=pd.date_range(self.start_date, self.end_date),
        )

        self.parameters = pd.DataFrame(
            data={
                "npi_name": [""] * len(self.geoids),
                "parameter": [""] * len(self.geoids),
                "start_date": [[self.start_date]] * len(self.geoids),
                "end_date": [[self.end_date]] * len(self.geoids),
                "reduction": [0.0] * len(self.geoids),
            },
            index=self.geoids,
        )

        self.param_name = npi_config["parameter"].as_str().lower()

        if (loaded_df is not None) and self.name in loaded_df["npi_name"].values:
            self.__createFromDf(loaded_df, npi_config)
        else:
            self.__createFromConfig(npi_config)

        # if parameters are exceeding global start/end dates, index of parameter df will be out of range so check first
        if self.sanitize:
            too_early = (
                min([min(i) for i in self.parameters["start_date"]]) < self.start_date
            )
            too_late = (
                max([max(i) for i in self.parameters["end_date"]]) > self.end_date
            )
            if too_early or too_late:
                raise ValueError(
                    "at least one period start or end date is not between global dates"
                )

        for grp_config in npi_config["groups"]:
            affected_geoids_grp = self.__get_affected_geoids_grp(grp_config)
            for sub_index in range(
                len(self.parameters["start_date"][affected_geoids_grp[0]])
            ):
                period_range = pd.date_range(
                    self.parameters["start_date"][affected_geoids_grp[0]][sub_index],
                    self.parameters["end_date"][affected_geoids_grp[0]][sub_index],
                )
                self.npi.loc[affected_geoids_grp, period_range] = np.tile(
                    self.parameters["reduction"][affected_geoids_grp],
                    (len(period_range), 1),
                ).T

        # for index in self.parameters.index:
        #    for sub_index in range(len(self.parameters["start_date"][index])):
        #        period_range = pd.date_range(self.parameters["start_date"][index][sub_index], self.parameters["end_date"][index][sub_index])
        #        ## This the line that does the work
        #        self.npi_old.loc[index, period_range] = np.tile(self.parameters["reduction"][index], (len(period_range), 1)).T
        # print(f'{self.name}, : {(self.npi_old == self.npi).all().all()}')

        self.__checkErrors()

    def __checkErrors(self):
        if not self.sanitize:
            return
        min_start_date = min([min(i) for i in self.parameters["start_date"]])
        max_start_date = max([max(i) for i in self.parameters["start_date"]])
        min_end_date = min([min(i) for i in self.parameters["end_date"]])
        max_end_date = max([max(i) for i in self.parameters["end_date"]])
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

        ### if self.param_name not in REDUCE_PARAMS:
        ###     raise ValueError(f"Invalid parameter name: {self.param_name}. Must be one of {REDUCE_PARAMS}")

        # Validate
        if (self.npi == 0).all(axis=None):
            print(f"Warning: The intervention in config: {self.name} does nothing.")

        if (self.npi > 1).any(axis=None):
            raise ValueError(
                f"The intervention in config: {self.name} has reduction of {self.param_name} is greater than 1"
            )

    def __createFromConfig(self, npi_config):
        # Get name of the parameter to reduce

        # JK : This could have broken something
        self.param_name = npi_config["parameter"].as_str().lower().replace(" ", "")

        self.affected_geoids = self.__get_affected_geoids(npi_config)

        self.parameters = self.parameters[
            self.parameters.index.isin(self.affected_geoids)
        ]
        dist = npi_config["value"].as_random_distribution()
        self.parameters["npi_name"] = self.name
        self.parameters["parameter"] = self.param_name

        for grp_config in npi_config["groups"]:
            affected_geoids_grp = self.__get_affected_geoids_grp(grp_config)
            # Create reduction
            start_dates = []
            end_dates = []
            if grp_config["periods"].exists():
                for period in grp_config["periods"]:
                    start_dates = start_dates + [period["start_date"].as_date()]
                    end_dates = end_dates + [period["end_date"].as_date()]
            else:
                start_dates = [self.start_date]
                end_dates = [self.end_date]
            for geoid in affected_geoids_grp:
                self.parameters.at[geoid, "start_date"] = start_dates
                self.parameters.at[geoid, "end_date"] = end_dates
                self.parameters.at[geoid, "reduction"] = dist(size=1)

    def __get_affected_geoids_grp(self, grp_config):
        if grp_config["affected_geoids"].get() == "all":
            affected_geoids_grp = self.geoids
        else:
            affected_geoids_grp = [str(n.get()) for n in grp_config["affected_geoids"]]
        return affected_geoids_grp

    def __createFromDf(self, loaded_df, npi_config):
        loaded_df.index = loaded_df.geoid
        loaded_df = loaded_df[loaded_df["npi_name"] == self.name]
        self.parameters = loaded_df[
            ["npi_name", "start_date", "end_date", "parameter", "reduction"]
        ].copy()
        # self.parameters["start_date"] = [[datetime.date.fromisoformat(date) for date in strdate.split(",")] for strdate in self.parameters["start_date"]]
        # self.parameters["end_date"] =   [[datetime.date.fromisoformat(date) for date in strdate.split(",")] for strdate in self.parameters["end_date"]]
        # self.affected_geoids = set(self.parameters.index)

        self.affected_geoids = self.__get_affected_geoids(npi_config)
        if self.sanitize:
            if len(self.affected_geoids) != len(self.parameters):
                print(f"loading {self.name} and we got {len(self.parameters)} geoids")
                print(
                    f"getting from config that it affects {len(self.affected_geoids)}"
                )

        for grp_config in npi_config["groups"]:
            affected_geoids_grp = self.__get_affected_geoids_grp(grp_config)
            # Create reduction
            start_dates = []
            end_dates = []
            if grp_config["periods"].exists():
                for period in grp_config["periods"]:
                    start_dates = start_dates + [period["start_date"].as_date()]
                    end_dates = end_dates + [period["end_date"].as_date()]
            else:
                start_dates = [self.start_date]
                end_dates = [self.end_date]
            for geoid in affected_geoids_grp:
                if not geoid in self.parameters.index:
                    self.parameters.at[geoid, "start_date"] = start_dates
                    self.parameters.at[geoid, "end_date"] = end_dates
                    dist = npi_config["value"].as_random_distribution()
                    self.parameters.at[geoid, "reduction"] = dist(size=1)
                    self.parameters.at[geoid, "npi_name"] = self.name
                    self.parameters.at[geoid, "parameter"] = self.param_name
                self.parameters.at[geoid, "start_date"] = start_dates
                self.parameters.at[geoid, "end_date"] = end_dates

        self.parameters = self.parameters.loc[self.affected_geoids]
        # self.parameters = self.parameters[self.parameters.index.isin(self.affected_geoids) ]
        # self.parameters = self.parameters[self.affected_geoids]

        # parameter name is picked from config too: (before: )
        # self.param_name = self.parameters["parameter"].unique()[0]  # [0] to convert ndarray to str
        # now:
        self.param_name = npi_config["parameter"].as_str().lower().replace(" ", "")
        self.parameters["parameter"] = self.param_name

    def __get_affected_geoids(self, npi_config):
        # Optional config field "affected_geoids"
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        affected_geoids_grp = []
        for grp_config in npi_config["groups"]:
            if grp_config["affected_geoids"].get() == "all":
                affected_geoids_grp = self.geoids
            else:
                affected_geoids_grp += [
                    str(n.get()) for n in grp_config["affected_geoids"]
                ]
        affected_geoids = set(affected_geoids_grp)
        if len(affected_geoids) != len(affected_geoids_grp):
            raise ValueError(
                f"In NPI {self.name}, some geoids belong to several groups. This is unsupported."
            )
        return affected_geoids

    def getReduction(self, param, default=0.0):
        "Return the reduction for this param, `default` if no reduction defined"

        if param == self.param_name:
            return self.npi
        return default

    def getReductionToWrite(self):
        df = self.parameters
        df.index.name = "geoid"
        df["start_date"] = df["start_date"].apply(
            lambda l: ",".join([d.strftime("%Y-%m-%d") for d in l])
        )
        df["end_date"] = df["end_date"].apply(
            lambda l: ",".join([d.strftime("%Y-%m-%d") for d in l])
        )
        df = df.reset_index()
        return df
