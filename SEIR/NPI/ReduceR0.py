import pandas as pd
import numpy as np

from .base import NPIBase


class ReduceR0(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids):
        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.period_start_date = npi_config["period_start_date"].as_date() \
            if npi_config["period_start_date"].exists() else self.start_date
        self.period_end_date = npi_config["period_end_date"].as_date() \
            if npi_config["period_end_date"].exists() else self.end_date

        if not (self.start_date <= self.period_start_date <= self.end_date):
            raise ValueError(f"period_start_date ({self.period_start_date}) is not between global dates [{self.start_date}, {self.end_date}]")
        if not (self.start_date <= self.period_end_date <= self.end_date):
            raise ValueError(f"period_end_date ({self.period_end_date}) is not between global dates [{self.start_date}, {self.end_date}]")
        if self.period_end_date < self.start_date:
            raise ValueError(f"period_end_date  ({self.period_end_date}) is less than period_start_date ({self.period_start_date})")

        self.dist = npi_config["value"].as_random_distribution()

        # Optional "affected_geoids" config field.
        # If values of "affected_geoids" is "all" or unspecified, run on all geoids.
        # Otherwise, run only on geoids specified.
        affected = geoids
        if "affected_geoids" in npi_config and npi_config["affected_geoids"].get() != "all":
            affected = []
            for n in npi_config["affected_geoids"]:
                node = str(n.get()) # because confuse may read as an int
                if node not in geoids:
                    raise ValueError(f"Invalid config value {n.name} ({node}) not in geoids")
                affected.append(node)

        self.npi = pd.DataFrame(0.0, index=geoids,
                                columns=pd.date_range(self.start_date, self.end_date))
        period_range = pd.date_range(self.period_start_date, self.period_end_date)
        self.npi.loc[affected, period_range] = np.tile(self.dist(size=len(affected)), (len(period_range), 1)).T

        if self.npi.to_numpy(copy=True).nonzero()[0].size == 0:
            print(f"Warning: The intervention in config: {npi_config.name} does nothing.")

    def get(self):
        return self.npi
