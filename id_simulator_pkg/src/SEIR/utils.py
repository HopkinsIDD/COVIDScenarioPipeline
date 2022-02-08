import datetime
import functools
import numbers
import time
import confuse
import numpy as np
import scipy.stats
import sympy.parsing.sympy_parser
import logging
logger = logging.getLogger(__name__)

config = confuse.Configuration("COVIDScenarioPipeline", read=False)


def add_method(cls):
    "Decorator to add a method to a class"

    def decorator(func):
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)

        setattr(cls, func.__name__, wrapper)
        return func
    return decorator

### A little timer class
class Timer(object):
    def __init__(self, name):
        self.name = name
    def __enter__(self):
        logging.debug(f'[{self.name}] started')
        self.tstart = time.time()
    def __exit__(self, type, value, traceback):
        logging.info(f'[{self.name}] completed in {time.time() - self.tstart} s')


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


def get_truncated_normal(*, mean=0, sd=1, a=0, b=10):
    "Returns the truncated normal distribution"

    return scipy.stats.truncnorm((a - mean) / sd, (b - mean) / sd, loc=mean, scale=sd)

def get_log_normal(meanlog, sdlog):
    "Returns the log normal distribution"
    return scipy.stats.lognorm(s = sdlog, scale = np.exp(meanlog), loc = 0)



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
        if((self["p"] < 0) or (self["p"] > 1)):
            raise ValueError(f"""p value { self["p"] } is out of range [0,1]""")
        return functools.partial(np.random.binomial, self["n"].as_evaled_expression(), self["p"].as_evaled_expression())
    elif dist == "truncnorm":
        return get_truncated_normal(mean=self["mean"].as_evaled_expression(), sd=self["sd"].as_evaled_expression(),
                                    a=self["a"].as_evaled_expression(), b=self["b"].as_evaled_expression()
                                    ).rvs
    elif dist == "lognorm":
        return get_log_normal(meanlog=self["meanlog"].as_evaled_expression(), sdlog=self["sdlog"].as_evaled_expression()).rvs
    else:
        raise NotImplementedError(f"unknown distribution [got: {dist}]")
