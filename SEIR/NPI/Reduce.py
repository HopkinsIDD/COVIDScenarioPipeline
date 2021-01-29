import pandas as pd
import numpy as np
import datetime

from .base import NPIBase

### REDUCE_PARAMS = ["alpha", "r0", "gamma", "sigma"]
### PARALLEL_COMP_PARAMS = ["transmissibility_reduction", "susceptibility_reduction"]
### PARALLEL_TRANS_PARAMS = ["transition_rate"]

class Reduce(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df=None):
        super().__init__(name=getattr(npi_config, "key",
                                      (npi_config["scenario"].exists() and npi_config["scenario"].get()) or "unknown"))

        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        ### n_parallel_compartments = 1
        ### n_parallel_transitions = 0
        ### if "parallel_structure" in global_config["seir"]["parameters"]:
            ### if not "compartments" in global_config["seir"]["parameters"]["parallel_structure"]:
                ### raise ValueError(f"A config specifying a parallel structure should assign compartments to that structure")
            ### compartments_map = global_config["seir"]["parameters"]["parallel_structure"]["compartments"].get()
            ### n_parallel_compartments = len(compartments_map)
            ### compartments_dict = {k : v for v,k in enumerate(compartments_map)}
            ### if not "transitions" in global_config["seir"]["parameters"]["parallel_structure"]:
                ### raise ValueError(f"A config specifying a parallel structure should assign transitions to that structure")
            ### transitions_map = global_config["seir"]["parameters"]["parallel_structure"]["transitions"].get()
            ### n_parallel_transitions = len(transitions_map)
        ### self.all_parameters = REDUCE_PARAMS
        ### for param in PARALLEL_COMP_PARAMS:
            ### for compartment in range(n_parallel_compartments):
                ### self.all_parameters += [param + " " + str(compartment)]
        ### for param in PARALLEL_TRANS_PARAMS:
            ### for transition in range(n_parallel_transitions):
                ### self.all_parameters += [param + " " + str(transition)]
###
        self.geoids = geoids

        self.npi = pd.DataFrame(0.0, index=self.geoids,
                                columns=pd.date_range(self.start_date, self.end_date))

        self.parameters = pd.DataFrame(0.0, index=self.geoids,
                                       columns=["npi_name","start_date","end_date","parameter","reduction"])

        if (loaded_df is not None) and self.name in loaded_df['npi_name']:
            self.__createFromDf(loaded_df)
        else:
            self.__createFromConfig(npi_config)

        # if parameters are exceeding global start/end dates, index of parameter df will be out of range so check first
        if self.parameters["start_date"].min() < self.start_date or self.parameters["end_date"].max() > self.end_date:
            raise ValueError(f"""{self.name} : at least one period start or end date is not between global dates""")

        for index in self.parameters.index:
            period_range = pd.date_range(self.parameters["start_date"][index], self.parameters["end_date"][index])

            ## This the line that does the work
            self.npi.loc[index, period_range] = np.tile(self.parameters["reduction"][index], (len(period_range), 1)).T

        self.__checkErrors()

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
            raise ValueError(f"at least one period_start_date is greater than the corresponding period end date")

        for n in self.affected_geoids:
            if n not in self.geoids:
                raise ValueError(f"Invalid config value {n} not in geoids")

        ### if self.param_name not in REDUCE_PARAMS:
        ###     raise ValueError(f"Invalid parameter name: {self.param_name}. Must be one of {REDUCE_PARAMS}")




        # Validate
        if (self.npi == 0).all(axis=None):
            print(f"Warning: The intervention in config: {self.name} does nothing.")

        if (self.npi > 1).any(axis=None):
            raise ValueError(f"The intervention in config: {self.name} has reduction of {self.param_name} is greater than 1")

    def __createFromConfig(self, npi_config):
        # Get name of the parameter to reduce
        self.param_name = npi_config["parameter"].as_str().lower()

        # Optional config field "affected_geoids"
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        self.affected_geoids = set(self.geoids)
        if npi_config["affected_geoids"].exists() and npi_config["affected_geoids"].get() != "all":
            self.affected_geoids = {str(n.get()) for n in npi_config["affected_geoids"]}

        self.parameters = self.parameters[self.parameters.index.isin(self.affected_geoids)]
        # Create reduction
        self.dist = npi_config["value"].as_random_distribution()

        self.parameters["npi_name"] = self.name
        self.parameters["start_date"] = npi_config["period_start_date"].as_date()  \
            if npi_config["period_start_date"].exists() else self.start_date
        self.parameters["end_date"] = npi_config["period_end_date"].as_date() \
            if npi_config["period_end_date"].exists() else self.end_date
        self.parameters["parameter"] = self.param_name
        self.parameters["reduction"] = self.dist(size=self.parameters.shape[0])

    def __createFromDf(self, loaded_df):
        loaded_df.index = loaded_df.geoid
        loaded_df = loaded_df[loaded_df['npi_name'] == self.name]
        self.parameters = loaded_df[['npi_name','start_date','end_date','parameter','reduction']].copy()
        self.parameters["start_date"] = [datetime.date.fromisoformat(date) for date in self.parameters["start_date"]]
        self.parameters["end_date"] = [datetime.date.fromisoformat(date) for date in self.parameters["end_date"]]
        self.affected_geoids = set(self.parameters.index)
        self.param_name = self.parameters["parameter"].unique()[0]  # [0] to convert ndarray to str

    def getReduction(self, param, default=0.0):
        "Return the reduction for this param, `default` if no reduction defined"
        if param == self.param_name:
            return self.npi
        return default

    def getReductionToWrite(self):
        df = self.parameters
        df.index.name = "geoid"
        df["start_date"] = df["start_date"].astype("str")
        df["end_date"] = df["end_date"].astype("str")
        df = df.reset_index()
        return df
