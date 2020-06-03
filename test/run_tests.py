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



# Test other scripts  ------------------------------------------------------------

# Test build_US_setup
def _success_build_US_setup(test_dir):
    os.chdir(test_dir)

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/build_US_setup.R",
            "-c", f"config.yml",
            "-p", "../..",
            "-w", "FALSE"]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"build_US_setup.R failed with code {complete.returncode}"

    assert_file("data/mobility.csv")
    assert_file("data/geodata.csv")


# Test build_nonUS_setup
def _success_build_nonUS_setup(test_dir):
    os.chdir(test_dir)

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/build_nonUS_setup.R",
            "-c", f"config.yml",
            "-p", ".",
            "-w", "FALSE",
            "-n", f"data/geodata/population_data.csv",
            "-m", f"data/geodata/mobility_data.csv"
            ]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"build_nonUS_setup.R failed with code {complete.returncode}"

    assert_file("data/mobility.csv")
    assert_file("data/geodata.csv")



# Test create_seeding_US
def _success_create_seeding_US(test_dir):
    os.chdir(test_dir)

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/create_seeding.R",
            "-c", f"config.yml",
            "-s", "CSSE",
            ]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"create_seeding.R failed for US setup using JHU CSSE data with code {complete.returncode}"

    assert_file("data/seeding.csv")


# Test create_seeding_nonUS
def _success_create_seeding_nonUS(test_dir):
    os.chdir(test_dir)

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/create_seeding.R",
            "-c", f"config.yml",
            "-d", f"data/case_data/case_data.csv"
            ]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"create_seeding.R failed for non-US setup with code {complete.returncode}"

    assert_file("data/seeding.csv")



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
    
def test_build_US():
    _success_build_nonUS_setup("test_build_US_setup")

def test_build_nonUS():
    _success_build_nonUS_setup("test_build_nonUS_setup")

def test_create_seeding_US():
    _success_create_seeding_US("test_create_seeding_US")
    
def test_create_seeding_nonUS():
    _success_create_seeding_nonUS("test_create_seeding_nonUS")
    
