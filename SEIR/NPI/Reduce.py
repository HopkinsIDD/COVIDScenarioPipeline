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

        self.period_start_date = npi_config["period_start_date"].as_date() \
            if npi_config["period_start_date"].exists() else self.start_date
        self.period_end_date = npi_config["period_end_date"].as_date() \
            if npi_config["period_end_date"].exists() else self.end_date

        self.intervention_name = npi_config.name
        if loaded_df is None:
            self.__createFromConfig(npi_config)
        else:
            self.__createFromDf(loaded_df)

        self.__checkErrors()

    def __checkErrors(self):
        if not (self.start_date <= self.period_start_date <= self.end_date):
            raise ValueError(f"period_start_date ({self.period_start_date}) is not between global dates [{self.start_date}, {self.end_date}]")
        if not (self.start_date <= self.period_end_date <= self.end_date):
            raise ValueError(f"period_end_date ({self.period_end_date}) is not between global dates [{self.start_date}, {self.end_date}]")
        if self.period_end_date < self.start_date:
            raise ValueError(f"period_end_date  ({self.period_end_date}) is less than period_start_date ({self.period_start_date})")

        for n in self.affected_geoids:
            if n not in self.geoids:
                raise ValueError(f"Invalid config value {n.name} ({node}) not in geoids")

        if self.param_name not in REDUCE_PARAMS:
            raise ValueError(f"Invalid parameter name: {param_name}. Must be one of {REDUCE_PARAMS}")

        # Validate
        if (self.npi == 0).all(axis=None):
            print(f"Warning: The intervention in config: {self.intervention_name} does nothing.")

        if (self.npi > 1).any(axis=None):
            raise ValueError(f"The intervention in config: {self.intervention_name} has reduction of {param_name} is greater than 1")

    def __createFromConfig(self, npi_config):
        # Get name of the parameter to reduce
        self.param_name = npi_config["parameter"].as_str().lower()
        self.reduced_param = self.param_name

        # Optional config field "affected_geoids"
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        self.affected_geoids = self.geoids
        if "affected_geoids" in npi_config and npi_config["affected_geoids"].get() != "all":
            self.affected_geoids = []
            for n in npi_config["affected_geoids"]:
                node = str(n.get()) # because confuse may read as an int
                self.affected_geoids.append(node)

        # Create reduction
        self.dist = npi_config["value"].as_random_distribution()

        period_range = pd.date_range(self.period_start_date, self.period_end_date)

        ## This the line that does the work
        self.npi.loc[self.affected_geoids, period_range] = np.tile(self.dist(size=len(self.affected_geoids)), (len(period_range), 1)).T

    def __createFromDf(self, loaded_df):
        loaded_df.index = loaded_df.time
        loaded_df = loaded_df[loaded_df['npi_name'] == self.name]
        loaded_df.drop(['time', 'parameter', 'npi_name'], inplace = True, axis = 1)
        self.npi = loaded_df.T

    def getReduction(self, param):
        if param == self.reduced_param:
            return self.npi
        return pd.DataFrame(0.0, index=self.geoids,
                                    columns=pd.date_range(self.start_date, self.end_date))

    def getReductionToWrite(self):
        df = self.npi.T.assign(parameter="r0", npi_name=self.name)
        df.index.name = "time"
        df = df.reset_index()
        return df
