import pandas as pd
import numpy as np

from .base import NPIBase

REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]

class Reduce(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df = None):
        super().__init__(npi_config)

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.geoids = geoids

        self.npi = pd.DataFrame(0.0, index=self.geoids,
                                columns=pd.date_range(self.start_date, self.end_date))

        self.parameters = pd.DataFrame(0.0, index=self.geoids,
                                       columns=["npi_name","start_date","end_date","parameter","reduction"])

        if loaded_df is None:
            self.__createFromConfig(npi_config)
        else:
            self.__createFromDf(loaded_df)

        self.__checkErrors()

        for index in self.parameters.index:
            period_range = pd.date_range(self.parameters["start_date"][index], self.parameters["end_date"][index])

            ## This the line that does the work
            self.npi.loc[index, period_range] = np.tile(self.parameters["reduction"][index], (len(period_range), 1)).T

    def __checkErrors(self):
        min_start_date = self.parameters["start_date"].min()
        max_start_date = self.parameters["start_date"].max()
        min_end_date = self.parameters["end_date"].min()
        max_end_date = self.parameters["end_date"].max()
        if not ((self.start_date <= min_start_date) & (max_start_date <= self.end_date)):
            raise ValueError(f"at least one period_start_date [{min_start_date}, {max_start_date}] is not between global dates [{self.start_date}, {self.end_date}]")
        if not ((self.start_date <= min_end_date) & (max_end_date <= self.end_date)):
            raise ValueError(f"at least one period_end_date ([{min_end_date}, {max_end_date}] is not between global dates [{self.start_date}, {self.end_date}]")

        if not (self.parameters["start_date"] <= self.parameters["end_date"]).all():
            bad_parameters = self.parameters["start_date"] > self.parameters["end_date"]
            raise ValueError(f"at least one period_start_date is greater than the corresponding period end date")

        for n in self.affected_geoids:
            if n not in self.geoids:
                raise ValueError(f"Invalid config value {n.name} ({node}) not in geoids")

        if self.param_name not in REDUCE_PARAMS:
            raise ValueError(f"Invalid parameter name: {param_name}. Must be one of {REDUCE_PARAMS}")

        # Validate
        if (self.npi == 0).all(axis=None):
            print(f"Warning: The intervention in config: {self.name} does nothing.")

        if (self.npi > 1).any(axis=None):
            raise ValueError(f"The intervention in config: {self.name} has reduction of {param_name} is greater than 1")

    def __createFromConfig(self, npi_config):
        # Get name of the parameter to reduce
        self.param_name = npi_config["parameter"].as_str().lower()

        # Optional config field "affected_geoids"
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        self.affected_geoids = self.geoids
        if "affected_geoids" in npi_config and npi_config["affected_geoids"].get() != "all":
            self.affected_geoids = []
            for n in npi_config["affected_geoids"]:
                node = str(n.get()) # because confuse may read as an int
                self.affected_geoids.append(node)

        self.parameters = self.parameters[self.parameters.index.isin(self.affected_geoids)]
        # Create reduction
        self.dist = npi_config["value"].as_random_distribution()

        self.parameters["npi_name"] = self.name
        self.parameters["start_date"] = npi_config["period_start_date"].as_date()
        self.parameters["end_date"] = npi_config["period_end_date"].as_date()
        self.parameters["parameter"] = self.param_name
        self.parameters["reduction"] = self.dist(size=len(self.affected_geoids))


    def __createFromDf(self, loaded_df):
        loaded_df.index = loaded_df.geoid
        loaded_df = loaded_df[loaded_df['npi_name'] == self.name]
        self.parameters = loaded_df[['npi_name','start_date','end_date','parameter','reduction']]
        self.affected_geoids = self.parameters.index
        self.param_name = self.parameters["parameter"].unique()

    def getReduction(self, param):
        if param == self.param_name:
            return self.npi
        return pd.DataFrame(0.0, index=self.geoids,
                            columns=pd.date_range(self.start_date, self.end_date))

    def getReductionToWrite(self):
        df = self.parameters
        df.index.name = "geoid"
        df = df.reset_index()
        return df
