
.PHONY: rerun rerun_simulations rerun_hospitalization clean_hospitalization clean clean_simulations

RSCRIPT=Rscript
PYTHON=/home/app/python_venv/bin/python
NCOREPER=16
PIPELINE=../..
CONFIG=config.yml
RUN_ID=2020.10.30.09:43:12.UTC

run: .files/1_simulation_None .files/1_hospitalization_None_low .files/1_hospitalization_None_med .files/1_simulation_Scenario1 .files/1_hospitalization_Scenario1_low .files/1_hospitalization_Scenario1_med

.files/1_simulation_None: .files/directory_exists 
	$(PYTHON) -m SEIR -c $(CONFIG) -s None -n 1 -j $(NCOREPER) --in-id $(RUN_ID) --out-id $(RUN_ID)
	touch .files/1_simulation_None
.files/1_hospitalization_None_low: .files/directory_exists .files/1_simulation_None
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d low -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/None/2020.10.30.09:43:12.UTC/ --out-prefix test_simple/None/low/2020.10.30.09:43:12.UTC/
	touch .files/1_hospitalization_None_low
.files/1_hospitalization_None_med: .files/directory_exists .files/1_simulation_None
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d med -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/None/2020.10.30.09:43:12.UTC/ --out-prefix test_simple/None/med/2020.10.30.09:43:12.UTC/
	touch .files/1_hospitalization_None_med
.files/1_simulation_Scenario1: .files/directory_exists 
	$(PYTHON) -m SEIR -c $(CONFIG) -s Scenario1 -n 1 -j $(NCOREPER) --in-id $(RUN_ID) --out-id $(RUN_ID)
	touch .files/1_simulation_Scenario1
.files/1_hospitalization_Scenario1_low: .files/directory_exists .files/1_simulation_Scenario1
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d low -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/Scenario1/2020.10.30.09:43:12.UTC/ --out-prefix test_simple/Scenario1/low/2020.10.30.09:43:12.UTC/
	touch .files/1_hospitalization_Scenario1_low
.files/1_hospitalization_Scenario1_med: .files/directory_exists .files/1_simulation_Scenario1
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d med -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/Scenario1/2020.10.30.09:43:12.UTC/ --out-prefix test_simple/Scenario1/med/2020.10.30.09:43:12.UTC/
	touch .files/1_hospitalization_Scenario1_med

.files/directory_exists:
	mkdir -p .files
	touch .files/directory_exists


rerun: rerun_simulations rerun_hospitalization

rerun_simulations: clean_simulations
	rm -f .files/*_simulation*
rerun_hospitalization:
	rm -f .files/*_hospitalization*
clean: clean_simulations clean_hospitalization
	rm -rf .files
clean_simulations: rerun_simulations
	rm -rf model_output
clean_hospitalization: rerun_hospitalization
	rm -rf hospitalization
