import pandas as pd

from .base import NPIBase


class ReduceR0(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids):
        self.start_date = global_config["start_date"].as_date()
        self.end_date = global_config["end_date"].as_date()

        self.period_start_date = npi_config["period_start_date"].as_date() \
            if npi_config["period_start_date"].exists() else self.start_date
        self.period_end_date = npi_config["period_end_date"].as_date() \
            if npi_config["period_end_date"].exists() else self.end_date
        self.dist = npi_config["value"].as_random_distribution()

        self.npi = pd.DataFrame(0.0, index=geoids,
                                columns=pd.date_range(self.start_date, self.end_date))
        for dt in pd.date_range(self.period_start_date, self.period_end_date):
            self.npi[dt] = self.dist(size=len(self.npi))

    def get(self):
        return self.npi
