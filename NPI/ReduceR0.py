import numpy as np
import pandas as pd
class NPI(object):
    def __init__(self, geo_ids, period_start_date, period_end_date):
        self.period_start_date = period_start_date
        self.period_end_date = period_end_date
        self.npi = pd.DataFrame(0.0, index=geo_ids, columns=pd.date_range(period_start_date, period_end_date))
    def uniform_npi(self, low, high, start_date, end_date=None):
        for dt in pd.date_range(start_date, end_date if end_date else self.period_end_date):
            self.npi[dt] = np.random.uniform(low, high, size = len(self.npi))
        return self
    def get(self):
        return self.npi
