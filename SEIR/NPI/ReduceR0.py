import pandas as pd
import numpy as np

from .base import NPIBase
from .Reduce import Reduce

class ReduceR0(Reduce):
    def __init__(self, *, npi_config, global_config, geoids, loaded_df = None,  pnames_overlap_operation_sum = []):
        npi_config["parameter"] = "r0"
        super().__init__(npi_config = npi_config, global_config = global_config, geoids = geoids, loaded_df = loaded_df, pnames_overlap_operation_sum = pnames_overlap_operation_sum)
