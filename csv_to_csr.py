#!/usr/bin/env python

import click
import pandas as pd
import pathlib
import scipy.sparse

from SEIR.utils import config


@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this simulation")
def convert(config_file):
    config.set_file(config_file)

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())

    geodata_file = spatial_base_path / spatial_config["geodata"].get(),
    mobility_file = spatial_base_path / spatial_config["mobility"].get()
    mobility_npz = spatial_base_path / spatial_config["mobility"].get().replace(".csv", ".npz")

    data = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)}) # geoids and populations
    nnodes = len(self.data)
    nodenames = data[spatial_config["nodenames"].get()].tolist()
    mobility_data = pd.read_csv(mobility_file, converters={'ori': lambda x: str(x), 'dest': lambda x: str(x)})

    mobility = scipy.sparse.lil_matrix((self.nnodes, self.nnodes))
    for index, row in mobility_data.iterrows():
        mobility[nodenames.index(row['ori']), nodenames.index(row['dest'])] = row['amount']
        if (nodenames.index(row['ori']) == nodenames.index(row['dest'])):
                    raise ValueError(f"Mobility fluxes with same origin and destination: '{row['ori']}' to {row['dest']} in long form matrix. This is not supported")

    with open(mobility_npz, "w") as f:
        scipy.sparse.save_npz(scipy.sparse.csr_matrix(mobility), f)


if __name__ == "__main__":
    convert()
