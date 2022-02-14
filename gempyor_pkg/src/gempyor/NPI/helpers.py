import pandas as pd
import pyarrow as pa
import numpy as np
import typing

def npi_fileload(fname: str, extension: str = "") -> pd.DataFrame:
    """ Extension: could be empty if the filename already countains it """
    # Quite ugly and should be in class NPI
    if extension: # Empty strings are falsy in python
        fname = f"{fname}.{extension}"
    extension = fname.split('.')[-1]
    if extension == "csv":
        out_df = pd.read_csv(fname)
    elif extension == "parquet":
        out_df = pa.parquet.read_table(fname).to_pandas()
    else:
        raise NotImplementedError(
            f"Invalid extension {extension}. Must be 'csv' or 'parquet'"
        )
    return out_df

# Helper function
def reduce_parameter(
    parameter: np.ndarray,
    modification: typing.Union[pd.DataFrame, float],
    method: str = "prod",
) -> np.ndarray:
    if isinstance(modification, pd.DataFrame):
        modification = modification.T
        modification.index = pd.to_datetime(modification.index.astype(str))
        modification = (
            modification.resample("1D").ffill().to_numpy()
        )  # Type consistency:
    if method == "prod":
        return parameter * (1 - modification)
    elif method == "sum":
        return parameter + modification
    else:
        raise ValueError(f"Unknown method to do NPI reduction, got {method}")
