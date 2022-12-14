#!/usr/bin/env python

import click
import pandas as pd
import os
import scipy.sparse

from .utils import config


@click.command()
@click.option(
    "-c",
    "--config",
    "config_file",
    envvar="CONFIG_PATH",
    type=click.Path(exists=True),
    required=True,
    help="configuration file for this simulation",
)
def convert(config_file):
    config.set_file(config_file)

    spatial_config = config["spatial_setup"]
    spatial_base_path = spatial_config["base_path"].get()

    geodata_file = os.path.join(spatial_base_path, spatial_config["geodata"].get())
    mobility_file = os.path.join(spatial_base_path, spatial_config["mobility"].get())
    mobility_npz = os.path.join(spatial_base_path, spatial_config["mobility"].get().replace(".csv", ".npz"))

    nodenames_key = spatial_config["nodenames"].get()
    data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)})  # geoids and populations
    nnodes = len(data)
    nodenames = data[nodenames_key].tolist()
    mobility_data = pd.read_csv(mobility_file, converters={"ori": lambda x: str(x), "dest": lambda x: str(x)})

    mobility = scipy.sparse.lil_matrix((nnodes, nnodes))
    for index, row in mobility_data.iterrows():
        mobility[nodenames.index(row["ori"]), nodenames.index(row["dest"])] = row["amount"]
        if nodenames.index(row["ori"]) == nodenames.index(row["dest"]):
            raise ValueError(
                f"Mobility fluxes with same origin and destination: '{row['ori']}' to {row['dest']} in long form matrix. This is not supported"
            )

    scipy.sparse.save_npz(mobility_npz, scipy.sparse.csr_matrix(mobility))


if __name__ == "__main__":
    convert()
