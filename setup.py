#!/usr/bin/env python
from setuptools import find_packages
from distutils.core import setup
import os

this_directory = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(this_directory, 'README.md')) as f:
    long_description = f.read()

package_name = "SEIR"
package_version = "0.0.1"
description = """Hopkins IDD SEIR Simulation and Analysis"""

setup(
    name=package_name,
    version=package_version,
    description=description,
    long_description=long_description,
    long_description_content_type='text/markdown',

    author='Joseph Lemaitre',
    author_email='Jo.lemaitresamra@gmail.com',
    url='https://github.com/HopkinsIDD/COVIDScenarioPipeline',
    packages=find_packages(),
    package_data={},
    install_requires=[
        'pandas',
        'numpy',
        'seaborn',
        'matplotlib',
        'geopy',
        'tqdm',
        'geopandas',
        'shapely',
        'numba',
        'rpy2',
        'confuse',
        'sympy'
    ]
)
