import pytest
import os
from pathlib import Path
import subprocess
import sys
import multiprocessing
import datetime


# assert helpers
def assert_file(file):
    assert Path(file).is_file(), f"{file} is not a file"

def assert_dir(file):
    assert Path(file).is_dir(), f"{file} is not a dir"

# main test runner helper
def _success(test_dir):
    os.chdir(test_dir)
    subprocess.run(["make", "clean"])

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/make_makefile.R",
            "-c", "config.yml",
            "-p", "../..",
            "-n", str(multiprocessing.cpu_count()),
            "-y", sys.executable]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"make_makefile.R failed with code {complete.returncode}"

    assert_file("Makefile")

    # Run the Makefile
    cmd = ["make"]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"make failed with code {complete.returncode}"

    assert_dir("model_parameters")
    assert_dir("model_output")
    assert_dir("hospitalization")

# teardown
def teardown_function(self):
    subprocess.run(["make", "clean"])
    os.chdir("..")


# Test definitions

def test_simple():
    _success("test_simple")

def test_importation():
    _success("test_importation")

    assert_file("data/geodata.csv")
    assert_file("data/mobility.txt")
    assert_file("data/filter.txt")
    assert_dir("data/shp")
    assert_dir("importation")

def test_report():
    _success("test_report")

    assert_file("data/geodata.csv")
    assert_file("data/mobility.csv")
    assert_file("data/filter.txt")
    assert_dir("data/shp")
    assert_dir("importation")
    assert_dir("notebooks")
    today_str = datetime.date.today().strftime("%Y%m%d")
    assert_file(f"notebooks/Hawaii_{today_str}/Hawaii_{today_str}_report.html")

def test_hosp_age_adjust():
    _success("test_hosp_age_adjust")
