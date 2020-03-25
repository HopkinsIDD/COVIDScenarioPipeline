import numpy as np
import pandas as pd

class R0ReductionNPI(object):
    def __init__(self, geo_ids, start_date, end_date, intervention_config, full_config):
        self.start_date = start_date
        self.end_date = end_date
        self.geo_ids = geo_ids
        self.r0_reduction = pd.DataFrame(0.0, index=geo_ids, columns=pd.date_range(start_date, end_date))
        self.intervention_start_date = intervention_config["start_date"].get()
        self.intervention_end_date = intervention_config["end_date"].get()
        self.intervention_value = intervention_config["value"].get() # Hoping this will also get tham as a random distribution
        self.r0_reduction = pd.DataFrame(0.0, index=geo_ids, columns=pd.date_range(self.start_date, self.end_date))
        for dt in pd.date_range(self.intervention_start_date(), self.intervention_end_date()):
            self.r0_reduction[dt] = self.intervention_value()
        return self
    def r0_reduction(self):
        return self.r0_reduction
    def get(self):
        return self.r0_reduction # for backwards compatibility
