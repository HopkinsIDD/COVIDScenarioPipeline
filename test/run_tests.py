import pytest
import os
from pathlib import Path
import subprocess
import sys
import multiprocessing
import datetime
import hashlib

from SEIR import file_paths


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
    cmd = [
        "Rscript",
        "../../R/scripts/make_makefile.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-n",
        str(multiprocessing.cpu_count()),
        "-y",
        sys.executable,
    ]
    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"make_makefile.R failed with code {complete.returncode}"

    assert_file("Makefile")

    # Run the Makefile
    cmd = ["make"]
    complete = subprocess.run(cmd)
    assert complete.returncode == 0, f"make failed with code {complete.returncode}"

    assert_dir("model_output/seir")
    assert_dir("model_output/snpi")
    assert_dir("model_output/spar")
    assert_dir("model_output/hosp")


# teardown
def teardown_function(self):
    subprocess.run(["make", "clean"])
    os.chdir("..")


# Test other scripts  ------------------------------------------------------------

# Test build_US_setup
def _success_build_US_setup(test_dir):
    os.chdir(test_dir)

    # Run Build Script
    cmd = [
        "Rscript",
        "../../R/scripts/build_US_setup.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-w",
        "FALSE",
    ]
    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"build_US_setup.R failed with code {complete.returncode}"

    assert_file("data/mobility.csv")
    assert_file("data/geodata.csv")


# Test build_nonUS_setup
def _success_build_nonUS_setup(test_dir):
    os.chdir(test_dir)

    # Run Build script
    cmd = [
        "Rscript",
        "../../R/scripts/build_nonUS_setup.R",
        "-c",
        "config.yml",
        "-w",
        "FALSE",
        "-n",
        "population_data.csv",
        "-m",
        "mobility_data.csv",
    ]
    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"build_nonUS_setup.R failed with code {complete.returncode}"

    assert_file("data/mobility.csv")
    assert_file("data/geodata.csv")


# Test create_seeding_US
def _success_create_seeding_US(test_dir):
    os.chdir(test_dir)

    # Run Seeding script
    cmd = ["Rscript", "../../R/scripts/create_seeding.R", "-c", "config.yml"]
    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"create_seeding.R failed for US setup using JHU CSSE data with code {complete.returncode}"

    assert_file("data/seeding.csv")


# Test create_seeding_nonUS
def _success_create_seeding_nonUS(test_dir):
    os.chdir(test_dir)

    # Make Makefile
    cmd = ["Rscript", "../../R/scripts/create_seeding.R", "-c", "config.yml"]
    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"create_seeding.R failed for non-US setup with code {complete.returncode}"

    assert_file("data/seeding.csv")


# Test runs 2x in a row
def _success_x2(test_dir):
    os.chdir(test_dir)
    subprocess.run(["make", "clean"])

    _success(".")
    # Make clean
    subprocess.run(["make", "clean"])

    _success(".")
    # Make clean again
    subprocess.run(["make", "clean"])


# Test definitions


def test_simple():
    _success("test_simple")


def test_multitime():
    _success("test_multitime")


# def test_old_hospitalization():
#    _success("test_old_hospitalization")

# def test_importation():
# _success("test_importation")

# assert_file("data/geodata.csv")
# assert_file("data/mobility.txt")
# assert_dir("data/shp")
# assert_dir("model_output/seed")


def test_report():
    _success("test_report")

    assert_file("data/geodata.csv")
    assert_file("data/mobility.csv")
    assert_dir("data/shp")
    assert_dir("model_output/seed")
    assert_dir("notebooks")
    today_str = datetime.date.today().strftime("%Y%m%d")
    assert_file(f"notebooks/Hawaii_{today_str}/Hawaii_{today_str}_report.html")


# def test_hosp_age_adjust():
#    _success("test_hosp_age_adjust")


def test_hospitalization_branching():
    _success("test_hospitalization_branching")

    assert_dir("model_output/seir")
    assert_dir("model_output/snpi")
    assert_dir("model_output/spar")
    assert_dir("model_output/hosp")
    assert_dir("model_output/hpar")


def test_vaccination():
    _success("test_vaccination")


def test_inference():
    os.chdir("test_inference")

    # build_US_setup.R
    cmd = [
        "Rscript",
        "../../R/scripts/build_US_setup.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
    ]

    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"build_US_setup.R failed with code {complete.returncode}"

    assert_file("data/geodata.csv")
    assert_file("data/mobility.csv")

    # full_filter.R
    cmd = [
        "Rscript",
        "../../R/scripts/full_filter.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-j",
        "1",
        "-b",
        "1",
        "-u",
        "test_inference",
        "-y",
        sys.executable,
    ]

    complete = subprocess.run(cmd)

    assert (
        complete.returncode == 0
    ), f"full_filter.R failed with code {complete.returncode}"

    assert_file("data/test1/seeding.csv")
    assert_file("data/us_data.csv")
    assert_dir("model_output")
    assert_dir("model_output/seed")
    assert_dir("model_output/seir")
    assert_dir("model_output/snpi")
    assert_dir("model_output/spar")
    assert_dir("model_output/hosp")
    assert_dir("model_output/hpar")
    assert_dir("model_output/llik")
    # os.removedirs("model_output")


def test_inference_multiblock():
    os.chdir("test_inference_multiblock")

    # build_US_setup.R
    cmd = [
        "Rscript",
        "../../R/scripts/build_US_setup.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
    ]

    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"build_US_setup.R failed with code {complete.returncode}"

    assert_file("data/geodata.csv")
    assert_file("data/mobility.csv")

    # full_filter.R
    cmd_1 = [
        "Rscript",
        "../../R/scripts/full_filter.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-j",
        "1",
        "-b",
        "1",
        "-u",
        "test_inference",
        "-y",
        sys.executable,
    ]

    complete = subprocess.run(cmd_1)
    assert (
        complete.returncode == 0
    ), f"full_filter.R block 1 failed with code {complete.returncode}"

    cmd_2 = [
        "Rscript",
        "../../R/scripts/full_filter.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-j",
        "1",
        "-b",
        "2",
        "-u",
        "test_inference",
        "-y",
        sys.executable,
    ]

    complete = subprocess.run(cmd_2)

    assert (
        complete.returncode == 0
    ), f"full_filter.R block 2 failed with code {complete.returncode}"

    final_prefix = "test_inference/Scenario1/low/test_inference/global/final/"
    intermediate_prefix = (
        "test_inference/Scenario1/low/test_inference/global/intermediate/000000001."
    )
    final_filename = file_paths.create_file_name(
        "test_inference", final_prefix, 1, "llik", "parquet"
    )
    intermediate_filename = file_paths.create_file_name(
        "test_inference", intermediate_prefix, 2, "llik", "parquet"
    )

    final_hash = ""
    with open(final_filename, "rb") as f:
        bytes = f.read()
        final_hash = hashlib.md5(bytes).hexdigest()

    intermediate_hash = ""
    with open(intermediate_filename, "rb") as f:
        bytes = f.read()
        intermediate_hash = hashlib.md5(bytes).hexdigest()

    assert final_hash == intermediate_hash

    assert_file("data/test1/seeding.csv")
    assert_file("data/us_data.csv")
    assert_dir("model_output")
    assert_dir("model_output/seed")
    assert_dir("model_output/seir")
    assert_dir("model_output/snpi")
    assert_dir("model_output/spar")
    assert_dir("model_output/hosp")
    assert_dir("model_output/hpar")
    assert_dir("model_output/llik")


def test_build_US():
    _success_build_US_setup("test_build_US_setup")


def test_build_nonUS():
    _success_build_nonUS_setup("test_build_nonUS_setup")


def test_create_seeding_US():
    _success_create_seeding_US("test_create_seeding_US")


def test_create_seeding_nonUS():
    _success_create_seeding_nonUS("test_create_seeding_nonUS")


def test_simple_x2():
    _success_x2("test_simple_x2")


def test_compartmental_specification():
    os.chdir("test_compartmental_specification")

    # build_US_setup.R
    cmd = [
        "Rscript",
        "../../R/scripts/build_US_setup.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
    ]

    complete = subprocess.run(cmd)
    assert (
        complete.returncode == 0
    ), f"build_US_setup.R failed with code {complete.returncode}"

    assert_file("data/geodata.csv")
    assert_file("data/mobility.csv")

    # full_filter.R
    cmd = [
        "Rscript",
        "../../R/scripts/full_filter.R",
        "-c",
        "config.yml",
        "-p",
        "../..",
        "-j",
        "1",
        "-b",
        "1",
        "-u",
        "test_compartmental_specification",
        "-y",
        sys.executable,
    ]

    complete = subprocess.run(cmd)

    assert (
        complete.returncode == 0
    ), f"full_filter.R failed with code {complete.returncode}"

    assert_file("data/test1/seeding.csv")
    assert_file("data/us_data.csv")
    assert_dir("model_output")
    assert_dir("model_output/seed")
    assert_dir("model_output/seir")
    assert_dir("model_output/snpi")
    assert_dir("model_output/spar")
    assert_dir("model_output/hosp")
    assert_dir("model_output/hpar")
    assert_dir("model_output/llik")
    # os.removedirs("model_output")
