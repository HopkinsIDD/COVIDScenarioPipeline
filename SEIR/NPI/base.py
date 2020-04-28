import abc
import pyarrow as pa


class NPIBase(abc.ABC):
    __plugins__ = {}

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        NPIBase.__plugins__[cls.__name__] = cls

    def __init__(self, npi_config):
        self.name = npi_config.key

    @abc.abstractmethod
    def getReduction(self, param):
        pass

    # Returns dataframe with columns: <geoids>, time, parameter, name. Index is sequential.
    @abc.abstractmethod
    def getReductionToWrite(self):
        pass

    def writeReductions(self, fname, extension):
        out_df = self.getReductionToWrite()

        if extension == "csv":
            out_df.to_csv(f"{fname}.{extension}", index=False)
        elif extension == "parquet":
            out_df = pa.Table.from_pandas(out_df, preserve_index = False)
            pa.parquet.write_table(out_df,f"{fname}.{extension}")
        else:
            raise NotImplementedError(f"Invalid extension {extension}. Must be 'csv' or 'parquet'")

    def getName(self):
        return self.name

    def execute(*, npi_config, global_config, geoids):
        template = npi_config["template"].as_str()
        npi_class = NPIBase.__plugins__[template]
        return npi_class(npi_config=npi_config, global_config=global_config, geoids=geoids)
