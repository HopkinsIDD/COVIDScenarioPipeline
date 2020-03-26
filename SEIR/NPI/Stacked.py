import functools

from .base import NPIBase


class Stacked(NPIBase):
    def __init__(self, *, npi_config, global_config, geoids):
        npis = []
        for scenario in npi_config["scenarios"]:
            # if it's a string, look up the scenario name's config
            if isinstance(scenario.get(), str):
                scenario_npi_config = global_config["interventions"]["settings"][scenario.get()]
                if not scenario_npi_config.exists():
                    raise RuntimeError(f"couldn't find scenario in config file [got: {scenario}]")
            else:
                # otherwise use the specified map as the config
                scenario_npi_config = scenario

            npi = NPIBase.execute(npi_config=scenario_npi_config, global_config=global_config, geoids=geoids)
            npis.append(npi.get())

        self.npi = 1 - functools.reduce(lambda a, b: a * (1 - b), npis)

    def get(self):
        return self.npi
