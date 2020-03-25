import datetime
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


class ISO8601Date(confuse.Template):
    def convert(self, value, view):
        if isinstance(value, datetime.date):
            return value
        elif isinstance(value, str):
            return datetime.datetime.strptime(value, "%Y-%m-%d").date()
        else:
            self.fail("must be a date object or ISO8601 date", True)


@add_method(confuse.ConfigView)
def as_date(self):
    "Evaluates an datetime.date or ISO8601 date string, raises ValueError on parsing errors."

    return self.get(ISO8601Date())


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
    if dist == "fixed":
        return functools.partial(np.random.uniform, self["value"].as_evaled_expression(),
                                 self["value"].as_evaled_expression())
    elif dist == "uniform":
        return functools.partial(np.random.uniform, self["low"].as_evaled_expression(),
                                 self["high"].as_evaled_expression())
    elif dist == "poisson":
        return functools.partial(np.random.poisson, self["lam"].as_evaled_expression())
    elif dist == "binomial":
        return functools.partial(np.random.binomial, self["n"].as_evaled_expression(), self["p"].as_evaled_expression())
    else:
        raise NotImplementedError(f"unknown distribution [got: {dist}]")
