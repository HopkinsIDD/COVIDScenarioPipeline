import abc


class NPIBase(abc.ABC):
    __plugins__ = {}

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        NPIBase.__plugins__[cls.__name__] = cls

    @abc.abstractmethod
    def getReduction(self, param):
        pass

    @abc.abstractmethod
    def writeReductions(self, fname, extension):
        pass

    def execute(*, npi_config, global_config, geoids):
        template = npi_config["template"].as_str()
        npi_class = NPIBase.__plugins__[template]
        return npi_class(npi_config=npi_config, global_config=global_config, geoids=geoids)
