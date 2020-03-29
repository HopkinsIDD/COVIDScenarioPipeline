import pytest
from SEIR import setup

def test_SpatialSetup():
	ss = setup.SpatialSetup(setup_name="minimal_test",
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility.txt",
                            popnodes_key="population")

	assert(ss.setup_name == "minimal_test")

	with pytest.raises(ValueError):
		setup.SpatialSetup(setup_name="minimal_test",
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility_small.txt",
                            popnodes_key="population")

	with pytest.raises(KeyError):
		setup.SpatialSetup(setup_name="minimal_test",
                            geodata_file="test/data/geodata.csv",
                            mobility_file="test/data/mobility_small.txt",
                            popnodes_key="pops")
