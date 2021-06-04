import pathlib
import numpy as np
import pandas as pd
import datetime
import os
import scipy.sparse
import pyarrow as pa
import pyarrow.parquet as pq
import copy

from .utils import config
from . import file_paths
from functools import reduce
import logging
logger = logging.getLogger(__name__)

class Compartments:
    # Minimal object to be easily picklable for // runs
    def __init__(self, seir_config = None, compartments_file = None, transitions_file = None):

        times_set = 0

        ## Something like this is needed for check script:
        # self.parameters = Parameters(seir_config["parameters"])

        if (not compartments_file is None) and (not transitions_file is None):
            self.fromFile(compartments_file, transitions_file)
            times_set += 1
        if times_set == 0:
            self.constructFromConfig(seir_config)
            times_set += 1


        return
    def __eq__(self, other):
        return(
            (self.transitions == other.transitions).all().all() and
            (self.compartments == other.compartments).all().all()
        )

    def parse_compartments(self, seir_config):
        compartment_config = seir_config["compartments"]
        compartment_frame = None
        for compartment_name, compartment_value in compartment_config.get().items():
            tmp = pd.DataFrame({'key':1, compartment_name:compartment_value})
            if compartment_frame is None:
                compartment_frame = tmp
            else:
                compartment_frame = pd.merge(compartment_frame, tmp, on = 'key')
        compartment_frame = compartment_frame.drop(["key"], axis = 1)
        compartment_frame["name"] = compartment_frame.apply(
            lambda x: reduce(lambda a,b: a + "_" + b, x),
            axis = 1
        )
        self.compartments = compartment_frame

    def parse_transitions(self, seir_config):
        rc = reduce(
            lambda a, b: a.append(self.parse_single_transition(seir_config, b)),
            seir_config["transitions"],
            pd.DataFrame()
        )
        rc = rc.reset_index(drop=True)
        return(rc)

    def check_transition_element(self, single_transition_config, problem_dimension = None):
        return True

    def check_transition_elements(self, single_transition_config, problem_dimension):
        return True

    def access_original_config_by_multi_index(self, config_piece, index, dimension = None, encapsulate_as_list = False):
        if dimension is None:
            dimension = [None for i in index]
        tmp = zip(index, range(len(index)), dimension)
        tmp = [list_access_element(config_piece[x[1]], x[0], x[2], encapsulate_as_list) for x in tmp]
        return(tmp)

    def expand_transition_elements(self, single_transition_config, problem_dimension):
        proportion_size = get_list_dimension(single_transition_config["proportional_to"])
        new_transition_config = single_transition_config.copy()
        for p_idx in range(proportion_size):
            if(new_transition_config["proportional_to"][p_idx] == "source"):
                new_transition_config["proportional_to"][p_idx] = new_transition_config["source"]

        temp_array = np.zeros(problem_dimension)

        new_transition_config["source"] = np.zeros(problem_dimension, dtype = object)
        new_transition_config["destination"] = np.zeros(problem_dimension, dtype = object)
        new_transition_config["rate"] = np.zeros(problem_dimension, dtype = object)

        new_transition_config["proportional_to"] = np.zeros(
            problem_dimension,
            dtype = object
        )
        new_transition_config["proportion_exponent"] = np.zeros(
            problem_dimension,
            dtype = object
        )


        it = np.nditer(temp_array, flags=['multi_index'])
        for x in it:
            new_transition_config["source"][it.multi_index] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(single_transition_config["source"], it.multi_index)
            )

            new_transition_config["destination"][it.multi_index] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(single_transition_config["destination"], it.multi_index)
            )

            new_transition_config["rate"][it.multi_index] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(single_transition_config["rate"], it.multi_index)
            )

            new_transition_config["proportional_to"][it.multi_index] = as_list(list_recursive_convert_to_string([
                self.access_original_config_by_multi_index(
                    single_transition_config["proportional_to"][p_idx],
                    it.multi_index,
                    problem_dimension,
                    True
                ) for p_idx in range(proportion_size)
            ]))

            new_transition_config["proportion_exponent"][it.multi_index] = list_recursive_convert_to_string([
                self.access_original_config_by_multi_index(
                    single_transition_config["proportion_exponent"][p_idx],
                    it.multi_index,
                    problem_dimension
                ) for p_idx in range(proportion_size)
            ])

        return new_transition_config

    def format_source(self, source_column):
        rc = [y for y in map(
            lambda x: reduce(
                lambda a,b : str(a) + "_" + str(b),
                x
            ),
            source_column
        )]
        return(rc)

    def unformat_source(self, source_column):
        rc = [x.split("_") for x in source_column]
        return(rc)

    def format_destination(self, destination_column):
        rc = [y for y in map(
            lambda x: reduce(
                lambda a,b : str(a) + "_" + str(b),
                x
            ),
            destination_column
        )]
        return(rc)

    def unformat_destination(self, destination_column):
        rc = [x.split("_") for x in destination_column]
        return(rc)

    def format_rate(self, rate_column):
        rc = [y for y in map(
            lambda x: reduce(
                lambda a,b : str(a) + "%*%" + str(b),
                x
            ),
            rate_column
        )]
        return(rc)

    def unformat_rate(self, rate_column, compartment_dimension):
        rc = [x.split("%*%", maxsplit = compartment_dimension - 1) for x in rate_column]
        for row in rc:
            while(len(row) < compartment_dimension):
                row.append(1)
        return(rc)

    def format_proportional_to(self, proportional_to_column):
        rc = [y for y in map(
            lambda x: reduce(
                lambda a,b : str(a) + "*" + str(b),
                map(
                    lambda x : reduce(
                        lambda a,b : str(a) + "_" + str(b),
                        map(
                            lambda x: reduce(
                                lambda a,b: str(a) + "+" + str(b),
                                as_list(x)
                            ),
                            x
                        )
                    ),
                    x
                )
            ),
            proportional_to_column
        )]
        return(rc)

    def unformat_proportional_to(self, proportional_to_column):
        rc = [x.split("*") for x in proportional_to_column]
        for row in range(len(rc)):
            rc[row] = [x.split("_") for x in rc[row]]
            for elem in range(len(rc[row])):
                rc[row][elem] = [x.split("+") for x in as_list(rc[row][elem])]
        return(rc)

    def format_proportion_exponent(self, proportion_exponent_column):
        rc = [y for y in map(
            lambda x: reduce(
                lambda a,b : str(a) + "%*%" + str(b),
                map(
                    lambda x : reduce(
                        lambda a,b : str(a) + "*" + str(b),
                        x
                    ),
                    x
                )
            ),
            proportion_exponent_column
        )]
        return(rc)

    def unformat_proportion_exponent(self, proportion_exponent_column, compartment_dimension):
        rc = [x.split("%*%") for x in proportion_exponent_column]
        for row in range(len(rc)):
            rc[row] = [x.split("*", maxsplit = compartment_dimension - 1) for x in rc[row]]
            for elem in rc[row]:
                while(len(elem) < compartment_dimension):
                    elem.append(1)
        return(rc)

    def parse_single_transition(self, seir_config, single_transition_config):
        ## This method relies on having run parse_compartments
        single_transition_config = single_transition_config.get()
        self.check_transition_element(single_transition_config["source"])
        self.check_transition_element(single_transition_config["destination"])
        source_dimension = [get_list_dimension(x) for x in single_transition_config["source"]]
        destination_dimension = [get_list_dimension(x) for x in single_transition_config["destination"]]
        problem_dimension = reduce(lambda x,y : max(x,y), (source_dimension, destination_dimension))
        self.check_transition_elements(single_transition_config, problem_dimension)
        transitions = self.expand_transition_elements(single_transition_config, problem_dimension)

        tmp_array = np.zeros(problem_dimension)
        it = np.nditer(tmp_array, flags=['multi_index'])
        rc = reduce(
            lambda a,b: a.append(b),
            [pd.DataFrame(
                {
                    "source":[transitions["source"][it.multi_index]],
                    "destination":[transitions["destination"][it.multi_index]],
                    "rate":[transitions["rate"][it.multi_index]],
                    "proportional_to":[transitions["proportional_to"][it.multi_index]],
                    "proportion_exponent":[transitions["proportion_exponent"][it.multi_index]]
                },
                index = [0]
            ) for x in it]
        )

        return rc

    def toFile(self, compartments_file, transitions_file):
        out_df = self.compartments.copy()
        pa_df = pa.Table.from_pandas(out_df, preserve_index = False)
        pa.parquet.write_table(pa_df, compartments_file)

        out_df = self.transitions.copy()
        out_df["source"] = self.format_source(out_df["source"])
        out_df["destination"] = self.format_destination(out_df["destination"])
        out_df["rate"] = self.format_rate(out_df["rate"])
        out_df["proportional_to"] = self.format_proportional_to(out_df["proportional_to"])
        out_df["proportion_exponent"] = self.format_proportion_exponent(out_df["proportion_exponent"])
        pa_df = pa.Table.from_pandas(out_df, preserve_index = False)
        pa.parquet.write_table(pa_df, transitions_file)

        return

    def fromFile(self, compartments_file, transitions_file):
        self.compartments = pq.read_table(compartments_file).to_pandas()
        self.transitions = pq.read_table(transitions_file).to_pandas()
        compartment_dimension = self.compartments.shape[1] - 1
        self.transitions["source"] = self.unformat_source(self.transitions["source"])
        self.transitions["destination"] = self.unformat_destination(self.transitions["destination"])
        self.transitions["rate"] = self.unformat_rate(self.transitions["rate"], compartment_dimension)
        self.transitions["proportional_to"] = self.unformat_proportional_to(self.transitions["proportional_to"])
        self.transitions["proportion_exponent"] = self.unformat_proportion_exponent(
            self.transitions["proportion_exponent"],
            compartment_dimension
        )

        return

    def constructFromConfig(self, seir_config):
        self.parse_compartments(seir_config)
        self.transitions = self.parse_transitions(seir_config)

def get_list_dimension(thing):
    if type(thing) == list:
        return len(thing)
    return 1

def list_access_element(thing, idx, dimension = None, encapsulate_as_list = False):
    if not dimension is None:
        if dimension == 1:
            rc = as_list(thing)
        if dimension != 1:
            rc = as_list(list_access_element(thing, idx, None))
    if type(thing) == list:
        rc = thing[idx]
    else:
        rc = thing
    if encapsulate_as_list:
        return as_list(rc)
    else:
        return rc

def as_list(thing):
    if type(thing) == list:
        return(thing)
    return([thing])

def list_recursive_convert_to_string(thing):
    if type(thing) == list:
        return([list_recursive_convert_to_string(x) for x in thing])
    return(str(thing))
