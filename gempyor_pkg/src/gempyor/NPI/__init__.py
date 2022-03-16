from pathlib import Path
import sys
import inspect
import pkgutil
from importlib import import_module
from .helpers import *

from .base import NPIBase

__all__ = ["NPIBase"]


def _load_npi_plugins():
    "Recurse through the package directory and import classes that derive from NPIBase"

    for (_, name, _) in pkgutil.iter_modules([str(Path(__file__).parent)]):
        imported_module = import_module("." + name, package=__name__)

        for i in dir(imported_module):
            attribute = getattr(imported_module, i)

            if inspect.isclass(attribute) and issubclass(attribute, NPIBase):
                setattr(sys.modules[__name__], name, attribute)


_load_npi_plugins()
