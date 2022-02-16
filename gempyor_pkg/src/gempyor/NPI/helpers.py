import pandas as pd
import numpy as np
import typing


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
