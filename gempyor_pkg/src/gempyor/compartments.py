import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.parquet as pq

from .utils import config, Timer
from . import file_paths
from functools import reduce
import logging

logger = logging.getLogger(__name__)


class Compartments:
    # Minimal object to be easily picklable for // runs
    def __init__(self, seir_config=None, compartments_file=None, transitions_file=None):

        self.times_set = 0

        ## Something like this is needed for check script:

        if (not compartments_file is None) and (not transitions_file is None):
            self.fromFile(compartments_file, transitions_file)
            self.times_set += 1
        if (self.times_set == 0) and ("compartments" in seir_config.keys()):
            self.constructFromConfig(seir_config)
            self.times_set += 1
        if self.times_set == 0:
            self.defaultConstruct(seir_config)

        return

    def __eq__(self, other):
        return (self.transitions == other.transitions).all().all() and (
            self.compartments == other.compartments
        ).all().all()

    def defaultConstruct(self, seir_config):
        use_parallel = False
        if "parameters" in seir_config.keys():
            if "parallel_structure" in seir_config["parameters"].keys():
                use_parallel = True
        n_parallel_compartments = 1
        if use_parallel:
            n_parallel_compartments = len(
                seir_config["parameters"]["parallel_structure"]["compartments"].get()
            )
        self.compartments = pd.DataFrame(
            {"key": 1, "infection_stage": ["S", "E", "I1", "I2", "I3", "R"]}
        )
        parallel_frame = None
        if use_parallel:
            parallel_frame = pd.DataFrame(
                {
                    "key": 1,
                    "vaccination_stage": seir_config["parameters"][
                        "parallel_structure"
                    ]["compartments"].keys(),
                }
            )
        else:
            parallel_frame = pd.DataFrame(
                {"key": 1, "vaccination_stage": ["unvaccinated"]}
            )
        self.compartments = pd.merge(self.compartments, parallel_frame)
        self.compartments = self.compartments.drop(["key"], axis=1)
        self.compartments["name"] = self.compartments.apply(
            lambda x: reduce(lambda a, b: a + "_" + b, x), axis=1
        )

        if not use_parallel:
            transitions = [
                {
                    "source": ["S", "unvaccinated"],
                    "destination": ["E", "unvaccinated"],
                    "rate": ["R0 * gamma", 1],
                    "proportional_to": [
                        ["S", "unvaccinated"],
                        [[["I1", "I2", "I3"]], "unvaccinated"],
                    ],
                    "proportion_exponent": [["1", "1"], ["alpha", "1"]],
                },
                {
                    "source": [["E"], ["unvaccinated"]],
                    "destination": [["I1"], ["unvaccinated"]],
                    "rate": ["sigma", 1],
                    "proportional_to": [[["E"], ["unvaccinated"]]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I1"], ["unvaccinated"]],
                    "destination": [["I2"], ["unvaccinated"]],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I1"], ["unvaccinated"]]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I2"], ["unvaccinated"]],
                    "destination": [["I3"], ["unvaccinated"]],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I2"], ["unvaccinated"]]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I3"], ["unvaccinated"]],
                    "destination": [["R"], ["unvaccinated"]],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I3"], ["unvaccinated"]]],
                    "proportion_exponent": [["1", "1"]],
                },
            ]
        else:
            unique_infections_stages = (
                self.compartments["infection_stage"].unique().tolist()
            )
            unique_vaccination_stages = (
                self.compartments["vaccination_stage"].unique().tolist()
            )
            transitions = [
                {
                    "source": ["S", unique_vaccination_stages],
                    "destination": ["E", unique_vaccination_stages],
                    "rate": ["R0 * gamma", 1],
                    "proportional_to": [
                        ["S", unique_vaccination_stages],
                        [[["I1", "I2", "I3"]], unique_vaccination_stages],
                    ],
                    "proportion_exponent": [["1", "1"], ["alpha", "1"]],
                },
                {
                    "source": [["E"], unique_vaccination_stages],
                    "destination": [["I1"], unique_vaccination_stages],
                    "rate": ["sigma", 1],
                    "proportional_to": [[["E"], unique_vaccination_stages]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I1"], unique_vaccination_stages],
                    "destination": [["I2"], unique_vaccination_stages],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I1"], unique_vaccination_stages]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I2"], unique_vaccination_stages],
                    "destination": [["I3"], unique_vaccination_stages],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I2"], unique_vaccination_stages]],
                    "proportion_exponent": [["1", "1"]],
                },
                {
                    "source": [["I3"], unique_vaccination_stages],
                    "destination": [["R"], unique_vaccination_stages],
                    "rate": ["3 * gamma", 1],
                    "proportional_to": [[["I3"], unique_vaccination_stages]],
                    "proportion_exponent": [["1", "1"]],
                },
            ]
            parallel_transitions = [
                {
                    "source": [unique_infections_stages, [transition["from"].get()]],
                    "destination": [unique_infections_stages, [transition["to"].get()]],
                    "rate": [f"transition_rate{i}", 1],
                    "proportional_to": [
                        [unique_infections_stages, [transition["from"].get()]]
                    ],
                    "proportion_exponent": [["1", "1"]],
                }
                for i, transition in enumerate(
                    seir_config["parameters"]["parallel_structure"]["transitions"]
                )
            ]

            transitions = transitions + parallel_transitions

            # raise Warning("This code doesn't actually work")

        self.transitions = self.parse_transitions({"transitions": transitions}, True)

    def parse_compartments(self, seir_config):
        compartment_config = seir_config["compartments"]
        compartment_frame = None
        for compartment_name, compartment_value in compartment_config.get().items():
            tmp = pd.DataFrame({"key": 1, compartment_name: compartment_value})
            if compartment_frame is None:
                compartment_frame = tmp
            else:
                compartment_frame = pd.merge(compartment_frame, tmp, on="key")
        compartment_frame = compartment_frame.drop(["key"], axis=1)
        compartment_frame["name"] = compartment_frame.apply(
            lambda x: reduce(lambda a, b: a + "_" + b, x), axis=1
        )
        self.compartments = compartment_frame

    def parse_transitions(self, seir_config, fake_config=False):
        rc = reduce(
            lambda a, b: a.append(
                self.parse_single_transition(seir_config, b, fake_config)
            ),
            seir_config["transitions"],
            pd.DataFrame(),
        )
        rc = rc.reset_index(drop=True)
        return rc

    def check_transition_element(
        self, single_transition_config, problem_dimension=None
    ):
        return True

    def check_transition_elements(self, single_transition_config, problem_dimension):
        return True

    def access_original_config_by_multi_index(
        self, config_piece, index, dimension=None, encapsulate_as_list=False
    ):
        if dimension is None:
            dimension = [None for i in index]
        tmp = [y for y in zip(index, range(len(index)), dimension)]

        tmp = zip(index, range(len(index)), dimension)
        tmp = [
            list_access_element(config_piece[x[1]], x[0], x[2], encapsulate_as_list)
            for x in tmp
        ]
        return tmp

    def expand_transition_elements(self, single_transition_config, problem_dimension):
        proportion_size = get_list_dimension(
            single_transition_config["proportional_to"]
        )
        new_transition_config = single_transition_config.copy()
        for p_idx in range(proportion_size):
            if new_transition_config["proportional_to"][p_idx] == "source":
                new_transition_config["proportional_to"][p_idx] = new_transition_config[
                    "source"
                ]

        temp_array = np.zeros(problem_dimension)

        new_transition_config["source"] = np.zeros(problem_dimension, dtype=object)
        new_transition_config["destination"] = np.zeros(problem_dimension, dtype=object)
        new_transition_config["rate"] = np.zeros(problem_dimension, dtype=object)

        new_transition_config["proportional_to"] = np.zeros(
            problem_dimension, dtype=object
        )
        new_transition_config["proportion_exponent"] = np.zeros(
            problem_dimension, dtype=object
        )

        it = np.nditer(temp_array, flags=["multi_index"])
        for x in it:
            new_transition_config["source"][
                it.multi_index
            ] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(
                    single_transition_config["source"], it.multi_index
                )
            )

            new_transition_config["destination"][
                it.multi_index
            ] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(
                    single_transition_config["destination"], it.multi_index
                )
            )

            new_transition_config["rate"][
                it.multi_index
            ] = list_recursive_convert_to_string(
                self.access_original_config_by_multi_index(
                    single_transition_config["rate"], it.multi_index
                )
            )

            new_transition_config["proportional_to"][it.multi_index] = as_list(
                list_recursive_convert_to_string(
                    [
                        self.access_original_config_by_multi_index(
                            single_transition_config["proportional_to"][p_idx],
                            it.multi_index,
                            problem_dimension,
                            True,
                        )
                        for p_idx in range(proportion_size)
                    ]
                )
            )

            self.access_original_config_by_multi_index(
                single_transition_config["proportion_exponent"][0],
                it.multi_index,
                problem_dimension,
            )
            new_transition_config["proportion_exponent"][
                it.multi_index
            ] = list_recursive_convert_to_string(
                [
                    self.access_original_config_by_multi_index(
                        single_transition_config["proportion_exponent"][p_idx],
                        it.multi_index,
                        problem_dimension,
                    )
                    for p_idx in range(proportion_size)
                ]
            )

        return new_transition_config

    def format_source(self, source_column):
        rc = [
            y
            for y in map(
                lambda x: reduce(lambda a, b: str(a) + "_" + str(b), x), source_column
            )
        ]
        return rc

    def unformat_source(self, source_column):
        rc = [x.split("_") for x in source_column]
        return rc

    def format_destination(self, destination_column):
        rc = [
            y
            for y in map(
                lambda x: reduce(lambda a, b: str(a) + "_" + str(b), x),
                destination_column,
            )
        ]
        return rc

    def unformat_destination(self, destination_column):
        rc = [x.split("_") for x in destination_column]
        return rc

    def format_rate(self, rate_column):
        rc = [
            y
            for y in map(
                lambda x: reduce(lambda a, b: str(a) + "%*%" + str(b), x), rate_column
            )
        ]
        return rc

    def unformat_rate(self, rate_column, compartment_dimension):
        rc = [x.split("%*%", maxsplit=compartment_dimension - 1) for x in rate_column]
        for row in rc:
            while len(row) < compartment_dimension:
                row.append(1)
        return rc

    def format_proportional_to(self, proportional_to_column):
        rc = [
            y
            for y in map(
                lambda x: reduce(
                    lambda a, b: str(a) + "*" + str(b),
                    map(
                        lambda x: reduce(
                            lambda a, b: str(a) + "_" + str(b),
                            map(
                                lambda x: reduce(
                                    lambda a, b: str(a) + "+" + str(b), as_list(x)
                                ),
                                x,
                            ),
                        ),
                        x,
                    ),
                ),
                proportional_to_column,
            )
        ]
        return rc

    def unformat_proportional_to(self, proportional_to_column):
        rc = [x.split("*") for x in proportional_to_column]
        for row in range(len(rc)):
            rc[row] = [x.split("_") for x in rc[row]]
            for elem in range(len(rc[row])):
                rc[row][elem] = [x.split("+") for x in as_list(rc[row][elem])]
        return rc

    def format_proportion_exponent(self, proportion_exponent_column):
        rc = [
            y
            for y in map(
                lambda x: reduce(
                    lambda a, b: str(a) + "%*%" + str(b),
                    map(lambda x: reduce(lambda a, b: str(a) + "*" + str(b), x), x),
                ),
                proportion_exponent_column,
            )
        ]
        return rc

    def unformat_proportion_exponent(
        self, proportion_exponent_column, compartment_dimension
    ):
        rc = [x.split("%*%") for x in proportion_exponent_column]
        for row in range(len(rc)):
            rc[row] = [
                x.split("*", maxsplit=compartment_dimension - 1) for x in rc[row]
            ]
            for elem in rc[row]:
                while len(elem) < compartment_dimension:
                    elem.append(1)
        return rc

    def parse_single_transition(
        self, seir_config, single_transition_config, fake_config=False
    ):
        ## This method relies on having run parse_compartments
        if not fake_config:
            single_transition_config = single_transition_config.get()
        self.check_transition_element(single_transition_config["source"])
        self.check_transition_element(single_transition_config["destination"])
        source_dimension = [
            get_list_dimension(x) for x in single_transition_config["source"]
        ]
        destination_dimension = [
            get_list_dimension(x) for x in single_transition_config["destination"]
        ]
        problem_dimension = reduce(
            lambda x, y: max(x, y), (source_dimension, destination_dimension)
        )
        self.check_transition_elements(single_transition_config, problem_dimension)
        transitions = self.expand_transition_elements(
            single_transition_config, problem_dimension
        )

        tmp_array = np.zeros(problem_dimension)
        it = np.nditer(tmp_array, flags=["multi_index"])
        rc = reduce(
            lambda a, b: a.append(b),
            [
                pd.DataFrame(
                    {
                        "source": [transitions["source"][it.multi_index]],
                        "destination": [transitions["destination"][it.multi_index]],
                        "rate": [transitions["rate"][it.multi_index]],
                        "proportional_to": [
                            transitions["proportional_to"][it.multi_index]
                        ],
                        "proportion_exponent": [
                            transitions["proportion_exponent"][it.multi_index]
                        ],
                    },
                    index=[0],
                )
                for x in it
            ],
        )

        return rc

    def toFile(self, compartments_file, transitions_file):
        out_df = self.compartments.copy()
        pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
        pa.parquet.write_table(pa_df, compartments_file)

        out_df = self.transitions.copy()
        out_df["source"] = self.format_source(out_df["source"])
        out_df["destination"] = self.format_destination(out_df["destination"])
        out_df["rate"] = self.format_rate(out_df["rate"])
        out_df["proportional_to"] = self.format_proportional_to(
            out_df["proportional_to"]
        )
        out_df["proportion_exponent"] = self.format_proportion_exponent(
            out_df["proportion_exponent"]
        )
        pa_df = pa.Table.from_pandas(out_df, preserve_index=False)
        pa.parquet.write_table(pa_df, transitions_file)

        return

    def fromFile(self, compartments_file, transitions_file):
        self.compartments = pq.read_table(compartments_file).to_pandas()
        self.transitions = pq.read_table(transitions_file).to_pandas()
        compartment_dimension = self.compartments.shape[1] - 1
        self.transitions["source"] = self.unformat_source(self.transitions["source"])
        self.transitions["destination"] = self.unformat_destination(
            self.transitions["destination"]
        )
        self.transitions["rate"] = self.unformat_rate(
            self.transitions["rate"], compartment_dimension
        )
        self.transitions["proportional_to"] = self.unformat_proportional_to(
            self.transitions["proportional_to"]
        )
        self.transitions["proportion_exponent"] = self.unformat_proportion_exponent(
            self.transitions["proportion_exponent"], compartment_dimension
        )

        return

    def get_comp_idx(self, comp_dict: dict) -> int:
        """
        return the index of a compartiment given a filter. The filter has to isolate a compartiment,
        but it ignore columns that don't exist:
        :param comp_dict:
        :return:
        """
        mask = pd.concat(
            [self.compartments[k] == v for k, v in comp_dict.items()], axis=1
        ).all(axis=1)
        comp_idx = self.compartments[mask].index.values
        if len(comp_idx) != 1:
            raise ValueError(
                f"The provided dictionary does not allow to isolate a compartment: {comp_dict} isolate {self.compartments[mask]} from options {self.compartments}"
            )
        return comp_idx[0]

    def get_ncomp(self) -> int:
        return len(self.compartments)

    def constructFromConfig(self, seir_config):
        self.parse_compartments(seir_config)
        self.transitions = self.parse_transitions(seir_config, False)

    def get_transition_array(self):
        with Timer("SEIR.compartments"):
            transition_array = np.zeros(
                (self.transitions.shape[1], self.transitions.shape[0]), dtype="int"
            )
            for cit, colname in enumerate(("source", "destination")):
                for it, elem in enumerate(self.transitions[colname]):
                    elem = reduce(lambda a, b: a + "_" + b, elem)
                    rc = -1
                    for compartment in range(self.compartments.shape[0]):
                        if self.compartments["name"][compartment] == elem:
                            rc = compartment
                    if rc == -1:
                        print(self.compartments)
                        raise ValueError(
                            f"Could find {colname} defined by {elem} in compartments"
                        )
                    transition_array[cit, it] = rc

            unique_strings = []
            for x in self.transitions["proportion_exponent"]:
                for y in x:
                    candidate = reduce(lambda a, b: a + "*" + b, y)
                    candidate = candidate.replace(" ", "")
                    candidate = candidate.replace("*1", "")
                    if not candidate in unique_strings:
                        unique_strings.append(candidate)

            for x in self.transitions["rate"]:
                candidate = reduce(lambda a, b: a + "*" + b, x)
                candidate = candidate.replace(" ", "")
                candidate = candidate.replace("*1", "")
                if not candidate in unique_strings:
                    unique_strings.append(candidate)

            assert reduce(
                lambda a, b: a and b, [(x.find("(") == -1) for x in unique_strings]
            )
            assert reduce(
                lambda a, b: a and b, [(x.find(")") == -1) for x in unique_strings]
            )
            assert reduce(
                lambda a, b: a and b, [(x.find("%") == -1) for x in unique_strings]
            )
            assert reduce(
                lambda a, b: a and b, [(x.find(" ") == -1) for x in unique_strings]
            )

            for it, elem in enumerate(self.transitions["rate"]):
                candidate = reduce(lambda a, b: a + "*" + b, elem)
                candidate = candidate.replace(" ", "")
                candidate = candidate.replace("*1", "")
                if not candidate in unique_strings:
                    raise ValueError("Something went wrong")
                rc = [it for it, x in enumerate(unique_strings) if x == candidate][0]
                transition_array[2][it] = rc

            current_proportion_start = 0
            for it, elem in enumerate(self.transitions["proportional_to"]):
                transition_array[3][it] = current_proportion_start
                transition_array[4][it] = current_proportion_start + len(elem)
                current_proportion_start += len(elem)

            proportion_info = np.zeros((3, transition_array[4].max()), dtype="int")
            current_proportion_sum_start = 0
            current_proportion_sum_it = 0
            for it, elem in enumerate(self.transitions["proportional_to"]):
                for it2, elem2 in enumerate(elem):
                    elem_tmp = [
                        w
                        for w in pd.DataFrame(index=pd.MultiIndex.from_product(elem2))
                        .reset_index()
                        .apply(lambda z: reduce(lambda x, y: f"{x}_{y}", z), axis=1)
                    ]

                    # for it3, elem3 in enumerate(elem_tmp):
                    #     rc = -1
                    #     for compartment in range(self.compartments.shape[0]):
                    #         if self.compartments["name"][compartment] == elem3:
                    #             rc = compartment
                    #     if rc == -1:
                    #         raise ValueError(f"Could not find match for {elem3} in compartments")
                    proportion_info[0][
                        current_proportion_sum_it
                    ] = current_proportion_sum_start
                    proportion_info[1][
                        current_proportion_sum_it
                    ] = current_proportion_sum_start + len(elem_tmp)
                    current_proportion_sum_it += 1
                    current_proportion_sum_start += len(elem_tmp)
            proportion_compartment_index = 0
            for it, elem in enumerate(self.transitions["proportion_exponent"]):
                for y in elem:
                    candidate = reduce(lambda a, b: a + "*" + b, y)
                    candidate = candidate.replace(" ", "")
                    candidate = candidate.replace("*1", "")
                    if not candidate in unique_strings:
                        raise ValueError("Something went wrong")
                    rc = [it for it, x in enumerate(unique_strings) if x == candidate][
                        0
                    ]
                    proportion_info[2][proportion_compartment_index] = rc
                    proportion_compartment_index += 1

            assert proportion_compartment_index == current_proportion_sum_it

            proportion_array = np.zeros((current_proportion_sum_start), dtype="int64")

            proportion_index = 0
            for it, elem in enumerate(self.transitions["proportional_to"]):
                for it2, elem2 in enumerate(elem):
                    elem_tmp = [
                        w
                        for w in pd.DataFrame(index=pd.MultiIndex.from_product(elem2))
                        .reset_index()
                        .apply(lambda z: reduce(lambda x, y: f"{x}_{y}", z), axis=1)
                    ]

                    for it3, elem3 in enumerate(elem_tmp):
                        rc = -1
                        for compartment in range(self.compartments.shape[0]):
                            if self.compartments["name"][compartment] == elem3:
                                rc = compartment
                        if rc == -1:
                            raise ValueError(
                                f"Could find proportional_to {elem3} in compartments"
                            )

                        proportion_array[proportion_index] = rc
                        proportion_index += 1

            ## This will need to be reworked to deal with the summing bit
            ## There will be changes needed in the steps_source too
            ## They are doable though
            # for it, elem in enumerate(self.transitions['proportional_to']):
            #     elem = [y for y in map(
            #         lambda x: reduce(
            #             lambda a, b: str(a) + "_" + str(b),
            #             map(
            #                 lambda x: reduce(
            #                     lambda a, b: str(a) + "+" + str(b),
            #                     as_list(x)
            #                 ),
            #                 x
            #             )
            #         ),
            #         elem
            #     )]
            #     for it2, elem2 in enumerate(elem):
            #         rc = -1
            #         for compartment in range(self.compartments.shape[0]):
            #             if self.compartments["name"][compartment] == elem2:
            #                 rc = compartment
            #         proportion_array[it]

        return (
            unique_strings,
            transition_array,
            proportion_array,
            proportion_info,
        )

    def parse_parameters(self, parameters, parameter_names, unique_strings):
        parsed_parameters = self.parse_parameter_strings_to_numpy_arrays(
            parameters, parameter_names, unique_strings
        )
        return parsed_parameters

    def parse_parameter_strings_to_numpy_arrays(
        self,
        parameters,
        parameter_names,
        string_list,
        operator_reduce_lambdas={
            "*": lambda a, b: a * b,
            "/": lambda a, b: a / b,
            "+": lambda a, b: a + b,
            "-": lambda a, b: a - b,
            "^": lambda a, b: a**b,
        },
        operators=["^", "*", "/", "+", "-"],
    ):
        split_strings = [x.split(operators[0]) for x in string_list]
        rc_size = [len(string_list)]
        for x in parameters.shape[1:]:
            rc_size.append(x)
        rc = np.zeros(rc_size, dtype="float64")
        for sit, string in enumerate(split_strings):
            tmp_rc_size = [len(string)]
            for x in parameters.shape[1:]:
                tmp_rc_size.append(x)
            tmp_rc = np.zeros(tmp_rc_size, dtype="float64")
            is_numeric = [x.isnumeric() for x in string]
            is_parameter = [x in parameter_names for x in string]
            is_resolvable = [x[0] or x[1] for x in zip(is_numeric, is_parameter)]
            is_totally_resolvable = reduce(lambda a, b: a and b, is_resolvable)
            if not is_totally_resolvable:
                not_resolvable_indices = [
                    it for it, x in enumerate(is_resolvable) if not x
                ]

                tmp_rc[
                    not_resolvable_indices
                ] = self.parse_parameter_strings_to_numpy_arrays(
                    parameters,
                    parameter_names,
                    [string[not is_resolvable]],
                    operator_reduce_lambdas,
                    operators[1:],
                )
            for numeric_index in [x for x in range(len(is_numeric)) if is_numeric[x]]:
                tmp_rc[numeric_index] = parameters[0] * 0 + float(string[numeric_index])
            for parameter_index in [
                x for x in range(len(is_parameter)) if is_parameter[x]
            ]:
                parameter_name_index = [
                    it
                    for it, x in enumerate(parameter_names)
                    if x == string[parameter_index]
                ]
                tmp_rc[parameter_index] = parameters[parameter_name_index]
            rc[sit] = reduce(operator_reduce_lambdas[operators[0]], tmp_rc)
        return rc

    def get_compartments_explicitDF(self):
        df: pd.DataFrame = self.compartments.copy(
            deep=True
        )  # .melt(id_vars='name', var_name='meta_compartment', value_name='sub_compartment')
        # add prefix mc to all columns, even name
        rename_dict = {cn: f"mc_{cn}" for cn in df.columns}
        df = df.rename(columns=rename_dict)
        return df

    def plot(
        self, output_file="transition_graph", source_filters=[], destination_filters=[]
    ):
        """
        if source_filters is [["age0to17"], ["OMICRON", "WILD"]], it means filter all transitions that have
        as source age0to17 AND (OMICRON OR WILD).
        """
        import graphviz
        from functools import reduce, partial

        some_graph = self.parse_transitions(config["seir"])

        def filter_func(lst, this_filter=[]):
            for must in this_filter:
                if any(x in lst for x in must):
                    pass
                else:
                    return False
            return True

        some_graph = some_graph[
            [
                x
                for x in map(
                    partial(filter_func, this_filter=source_filters),
                    some_graph["source"],
                )
            ]
        ]
        some_graph = some_graph[
            [
                x
                for x in map(
                    partial(filter_func, this_filter=destination_filters),
                    some_graph["destination"],
                )
            ]
        ]

        graph_description = (
            "digraph {\n  overlap = false;"
            + reduce(
                lambda a, b: a + "\n" + b,
                some_graph.apply(
                    lambda x: f"""{reduce(lambda a,b : a + "_" + b, x["source"])} -> {reduce(lambda a,b: a + "_" + b, x["destination"])} [label="{reduce(lambda a,b: a + "*" + b, x["rate"])}"];""",
                    axis=1,
                ),
            )
            + "\n}"
        )
        src = graphviz.Source(graph_description)
        src.render(output_file, view=True)


def get_list_dimension(thing):
    if type(thing) == list:
        return len(thing)
    return 1


def list_access_element(thing, idx, dimension=None, encapsulate_as_list=False):
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
        return thing
    return [thing]


def list_recursive_convert_to_string(thing):
    if type(thing) == list:
        return [list_recursive_convert_to_string(x) for x in thing]
    return str(thing)
