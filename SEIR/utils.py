import functools
import numbers

import confuse
import numpy as np
import sympy.parsing.sympy_parser

config = confuse.Configuration("COVIDScenarioPipeline")


def add_method(cls):
    "Decorator to add a method to a class"

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)

        setattr(cls, func.__name__, wrapper)
        return func
    return decorator


@add_method(confuse.ConfigView)
def as_evaled_expression(self):
    "Evaluates an expression string, returning a float. Raises ValueError on parsing errors."

    value = self.get()
    if isinstance(value, numbers.Number):
        return value
    elif isinstance(value, str):
        try:
            return float(sympy.parsing.sympy_parser.parse_expr(value))
        except TypeError as e:
            raise ValueError(e) from e
    else:
        raise ValueError(f"expected numeric or string expression [got: {value}]")


@add_method(confuse.ConfigView)
def as_random_distribution(self):
    "Constructs a random distribution object from a distribution config key"

    dist = self["distribution"].get()
    if dist == "uniform":
        return functools.partial(np.random.uniform, self["low"].as_evaled_expression(),
                                 self["high"].as_evaled_expression())
    elif dist == "poisson":
        return functools.partial(np.random.poisson, self["lam"].as_evaled_expression())
    elif dist == "binomial":
        return functools.partial(np.random.binomial, self["n"].as_evaled_expression(), self["p"].as_evaled_expression())
    else:
        raise NotImplementedError(f"unknown distribution [got: {dist}]")
