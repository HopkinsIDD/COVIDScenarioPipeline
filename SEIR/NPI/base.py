import abc


class NPIBase(abc.ABC):
    def __init__(self, *, npi_config, global_config, geoids):
        self.npi_config = npi_config

    @abc.abstractmethod
    def get(self):
        pass
