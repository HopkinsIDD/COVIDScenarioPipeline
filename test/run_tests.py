import pytest
import os
from pathlib import Path
import subprocess
import sys
import multiprocessing


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
            "-c", f"config.yml",
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

    assert_dir("model_output")
    assert_dir("hospitalization")

# teardown
def teardown_function(self):
    subprocess.run(["make", "clean"])
    os.chdir("..")


# Test definitions

def test_1():
    _success("test1")

def test_importation():
    _success("test_importation")

    assert_file("data/filter.txt")
    assert_dir("data/shp")
    assert_dir("importation")


