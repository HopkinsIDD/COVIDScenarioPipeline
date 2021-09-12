import numpy as np
import os
import pytest
import warnings
import shutil


import pathlib
import pyarrow as pa
import pyarrow.parquet as pq

from SEIR import setup, seir, NPI, file_paths

from ..utils import config

DATA_DIR = os.path.dirname(__file__) + "/data"



def test_check_values():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_values",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_values",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        parameters_config=config["seir"]["parameters"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    with warnings.catch_warnings(record=True) as w:

        seeding = np.zeros((len(s.t_inter), s.nnodes))

        if np.all(seeding == 0):
            warnings.warn("provided seeding has only value 0", UserWarning)

        seeding[0,0] = 1

        if np.all(seeding == 0):
            warnings.warn("provided seeding has only value 0", UserWarning)

        if(np.all(s.mobility.data < 1)):
            warnings.warn("highest mobility value is less than 1", UserWarning)

        s.mobility.data[0] = 0.8
        s.mobility.data[1] = 0.5

        if(np.all(s.mobility.data < 1)):
            warnings.warn("highest mobility value is less than 1", UserWarning)

        assert(len(w) == 2)
        assert(issubclass(w[0].category, UserWarning))
        assert(issubclass(w[1].category, UserWarning))
        assert("seeding" in str(w[0].message))
        assert("mobility" in str(w[1].message))


def test_constant_population():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_seir",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_seir",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        parameters_config=config["seir"]["parameters"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))

    y0 = np.zeros((setup.ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[setup.S, 0, :] = s.popnodes

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(s.params, len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    states = seir.steps_SEIR_nb(*parameters, y0,
                       seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                       mobility_geoid_indices, mobility_data_indices,
                       mobility_data, True)

    completepop = s.popnodes.sum()
    origpop = s.popnodes
    for it in range(len(s.t_inter)):
        totalpop = 0
        for i in range(s.nnodes):
            totalpop += states[:5, :, i, it].sum()
            #Sum of S, E, I#, R for the geoid that is 'i'
            assert(origpop[i] == states[:5, :, i, it].sum())
        assert(completepop == totalpop)


def test_steps_SEIR_nb_simple_spread_with_txt_matrices():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_seir",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_seir",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        parameters_config=config["seir"]["parameters"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))
    seeding[:,0] = 100

    y0 = np.zeros((setup.ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[setup.S, 0, :] = s.popnodes

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data
    print(s.mobility.data)

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(s.params, len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    for i in range(100):
        states = seir.steps_SEIR_nb(*parameters, y0,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices,
                           mobility_data,True)


        assert states[seir.cumI, :, 1, :].max() > 0


def test_steps_SEIR_nb_simple_spread_with_csv_matrices():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_seir",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.csv",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_seir",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        parameters_config=config["seir"]["parameters"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))
    seeding[:,0] = 100

    y0 = np.zeros((setup.ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[setup.S, 0, :] = s.popnodes

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data
    print(s.mobility.data)

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(s.params, len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    for i in range(100):
        states = seir.steps_SEIR_nb(*parameters, y0,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices,
                           mobility_data,True)


        assert states[seir.cumI, :, 1, :].max() > 0

def test_steps_SEIR_no_spread():
    config.set_file(f"{DATA_DIR}/config.yml")

    ss = setup.SpatialSetup(setup_name="test_seir",
                            geodata_file=f"{DATA_DIR}/geodata.csv",
                            mobility_file=f"{DATA_DIR}/mobility.txt",
                            popnodes_key="population",
                            nodenames_key="geoid")

    s = setup.Setup(setup_name="test_seir",
                        spatial_setup=ss,
                        nsim=1,
                        npi_scenario="None",
                        npi_config=config["interventions"]["settings"]["None"],
                        parameters_config=config["seir"]["parameters"],
                        ti=config["start_date"].as_date(),
                        tf=config["end_date"].as_date(),
                        interactive=True,
                        write_csv=False,
                        dt=0.25)

    seeding = np.zeros((len(s.t_inter), s.nnodes))
    seeding[:,0] = 100

    y0 = np.zeros((setup.ncomp, s.params.n_parallel_compartments, s.nnodes))
    y0[setup.S, 0, :] = s.popnodes

    mobility_geoid_indices = s.mobility.indices
    mobility_data_indices = s.mobility.indptr
    mobility_data = s.mobility.data * 0

    npi = NPI.NPIBase.execute(npi_config=s.npi_config, global_config=config, geoids=s.spatset.nodenames)

    parameters = setup.parameters_quick_draw(s.params, len(s.t_inter), s.nnodes)
    parameters = setup.parameters_reduce(parameters, npi, s.dt)

    for i in range(100):
        states = seir.steps_SEIR_nb(*parameters, y0,
                           seeding, s.dt, s.t_inter, s.nnodes, s.popnodes,
                           mobility_geoid_indices, mobility_data_indices,
                           mobility_data, True)


        assert states[seir.cumI,:,1,:].max().shape == ()
        assert states[seir.cumI,:,1,:].max() == 0


def test_contuation_resume():
    config.clear()
    config.read(user=False)
    config.set_file('data/config.yml')
    scenario = 'Scenario1'
    sim_id2write = 100
    nsim = 1
    interactive = False
    write_csv = False
    write_parquet = True
    index = 1
    run_id = 'test'
    prefix = ''
    stoch_traj_flag = True

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
    s = setup.Setup(
        setup_name=config["name"].get() + "_" + str(scenario),
        spatial_setup=setup.SpatialSetup(
            setup_name=spatial_config["setup_name"].get(),
            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
            popnodes_key=spatial_config["popnodes"].get(),
            nodenames_key=spatial_config["nodenames"].get()
        ),
        nsim=nsim,
        npi_scenario=scenario,
        npi_config=config["interventions"]["settings"][scenario],
        parameters_config=config["seir"]["parameters"],
        seeding_config=config["seeding"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index = index,
        in_run_id = run_id,
        in_prefix = prefix,
        out_run_id = run_id,
        out_prefix = prefix
    )
    seir.onerun_SEIR(int(sim_id2write), s, stoch_traj_flag)

    states_old = pq.read_table(
          file_paths.create_file_name(s.in_run_id,s.in_prefix, 100,'seir',"parquet"),
        ).to_pandas()
    states_old = states_old[states_old["time"] == '2020-03-15'].reset_index(drop=True)

    config.clear()
    config.read(user=False)
    config.set_file('data/config_continuation_resume.yml')
    scenario = 'Scenario1'
    sim_id2write = 100
    nsim = 1
    interactive = False
    write_csv = False
    write_parquet = True
    index = 1
    run_id = 'test'
    prefix = ''
    stoch_traj_flag = True

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
    s = setup.Setup(
        setup_name=config["name"].get() + "_" + str(scenario),
        spatial_setup=setup.SpatialSetup(
            setup_name=spatial_config["setup_name"].get(),
            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
            popnodes_key=spatial_config["popnodes"].get(),
            nodenames_key=spatial_config["nodenames"].get()
        ),
        nsim=nsim,
        npi_scenario=scenario,
        npi_config=config["interventions"]["settings"][scenario],
        seeding_config=config["seeding"],
        parameters_config=config["seir"]["parameters"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index = index,
        in_run_id = run_id,
        in_prefix = prefix,
        out_run_id = run_id,
        out_prefix = prefix
    )
    seir.onerun_SEIR(sim_id2write, s, stoch_traj_flag)

    states_new = pq.read_table(
          file_paths.create_file_name(s.in_run_id,s.in_prefix, sim_id2write, 'seir',"parquet"),
        ).to_pandas()
    states_new = states_new[states_new["time"] == '2020-03-15'].reset_index(drop=True)
    assert((states_old[states_old['comp'] != 'diffI'] == states_new[states_new['comp'] != 'diffI']).all().all())

    seir.onerun_SEIR_loadID(sim_id2write=sim_id2write+1, s=s, sim_id2load=sim_id2write)
    states_new = pq.read_table(
          file_paths.create_file_name(s.in_run_id,s.in_prefix, sim_id2write+1, 'seir',"parquet"),
        ).to_pandas()
    states_new = states_new[states_new["time"] == '2020-03-15'].reset_index(drop=True)
    for path in ["model_output/seir","model_output/snpi","model_output/spar"]:
        shutil.rmtree(path)



def test_inference_resume():
    config.clear()
    config.read(user=False)
    config.set_file('data/config.yml')
    scenario = 'Scenario1'
    sim_id2write = 100
    nsim = 1
    interactive = False
    write_csv = False
    write_parquet = True
    index = 1
    run_id = 'test'
    prefix = ''
    stoch_traj_flag = True

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
    s = setup.Setup(
        setup_name=config["name"].get() + "_" + str(scenario),
        spatial_setup=setup.SpatialSetup(
            setup_name=spatial_config["setup_name"].get(),
            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
            popnodes_key=spatial_config["popnodes"].get(),
            nodenames_key=spatial_config["nodenames"].get()
        ),
        nsim=nsim,
        npi_scenario=scenario,
        npi_config=config["interventions"]["settings"][scenario],
        parameters_config=config["seir"]["parameters"],
        seeding_config=config["seeding"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index = index,
        in_run_id = run_id,
        in_prefix = prefix,
        out_run_id = run_id,
        out_prefix = prefix
    )
    seir.onerun_SEIR(int(sim_id2write), s, stoch_traj_flag)
    npis_old = pq.read_table(file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id2write, 'snpi',"parquet")).to_pandas()

    config.clear()
    config.read(user=False)
    config.set_file('data/config_inference_resume.yml')
    scenario = 'Scenario1'
    nsim = 1
    interactive = False
    write_csv = False
    write_parquet = True
    index = 1
    run_id = 'test'
    prefix = ''
    stoch_traj_flag = True

    spatial_config = config["spatial_setup"]
    spatial_base_path = pathlib.Path(spatial_config["base_path"].get())
    s = setup.Setup(
        setup_name=config["name"].get() + "_" + str(scenario),
        spatial_setup=setup.SpatialSetup(
            setup_name=spatial_config["setup_name"].get(),
            geodata_file=spatial_base_path / spatial_config["geodata"].get(),
            mobility_file=spatial_base_path / spatial_config["mobility"].get(),
            popnodes_key=spatial_config["popnodes"].get(),
            nodenames_key=spatial_config["nodenames"].get()
        ),
        nsim=nsim,
        npi_scenario=scenario,
        npi_config=config["interventions"]["settings"][scenario],
        seeding_config=config["seeding"],
        parameters_config=config["seir"]["parameters"],
        ti=config["start_date"].as_date(),
        tf=config["end_date"].as_date(),
        interactive=interactive,
        write_csv=write_csv,
        write_parquet=write_parquet,
        dt=config["dt"].as_number(),
        first_sim_index = index,
        in_run_id = run_id,
        in_prefix = prefix,
        out_run_id = run_id,
        out_prefix = prefix
    )

    seir.onerun_SEIR_loadID(sim_id2write=sim_id2write+1, s=s, sim_id2load=sim_id2write)
    npis_new = pq.read_table(file_paths.create_file_name(s.in_run_id,s.in_prefix,sim_id2write+1, 'snpi',"parquet")).to_pandas()

    print(npis_new["npi_name"])
    assert(npis_old["npi_name"].isin(['None', 'Wuhan', 'KansasCity']).all())
    assert(npis_new["npi_name"].isin(['None', 'Wuhan', 'KansasCity', 'BrandNew']).all())
    # assert((['None', 'Wuhan', 'KansasCity']).isin(npis_old["npi_name"]).all())
    # assert((['None', 'Wuhan', 'KansasCity', 'BrandNew']).isin(npis_new["npi_name"]).all())
    assert((npis_old["start_date"] == '2020-04-01').all())
    assert((npis_old["end_date"] == '2020-05-15').all())
    assert((npis_new["start_date"] == '2020-04-02').all())
    assert((npis_new["end_date"] == '2020-05-16').all())
    for path in ["model_output/seir","model_output/snpi","model_output/spar"]:
        shutil.rmtree(path)


    ## Clean up after ourselves
