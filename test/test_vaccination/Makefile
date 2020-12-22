
.PHONY: rerun rerun_simulations rerun_hospitalization clean_hospitalization clean clean_simulations

RSCRIPT=Rscript
PYTHON=python3
NCOREPER=1
PIPELINE=../..
CONFIG=config.yml
RUN_ID=2020.12.22.17:12:19.UTC

run: .files/1_simulation_early_vaccination .files/1_hospitalization_early_vaccination_low .files/1_hospitalization_early_vaccination_med .files/1_simulation_late_vaccination .files/1_hospitalization_late_vaccination_low .files/1_hospitalization_late_vaccination_med

.files/1_simulation_early_vaccination: .files/directory_exists 
	$(PYTHON) -m SEIR -c $(CONFIG) -s early_vaccination -n 1 -j $(NCOREPER) --in-id $(RUN_ID) --out-id $(RUN_ID)
	touch .files/1_simulation_early_vaccination
.files/1_hospitalization_early_vaccination_low: .files/directory_exists .files/1_simulation_early_vaccination
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d low -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/early_vaccination/2020.12.22.17:12:19.UTC/ --out-prefix test_simple/early_vaccination/low/2020.12.22.17:12:19.UTC/
	touch .files/1_hospitalization_early_vaccination_low
.files/1_hospitalization_early_vaccination_med: .files/directory_exists .files/1_simulation_early_vaccination
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d med -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/early_vaccination/2020.12.22.17:12:19.UTC/ --out-prefix test_simple/early_vaccination/med/2020.12.22.17:12:19.UTC/
	touch .files/1_hospitalization_early_vaccination_med
.files/1_simulation_late_vaccination: .files/directory_exists 
	$(PYTHON) -m SEIR -c $(CONFIG) -s late_vaccination -n 1 -j $(NCOREPER) --in-id $(RUN_ID) --out-id $(RUN_ID)
	touch .files/1_simulation_late_vaccination
.files/1_hospitalization_late_vaccination_low: .files/directory_exists .files/1_simulation_late_vaccination
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d low -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/late_vaccination/2020.12.22.17:12:19.UTC/ --out-prefix test_simple/late_vaccination/low/2020.12.22.17:12:19.UTC/
	touch .files/1_hospitalization_late_vaccination_low
.files/1_hospitalization_late_vaccination_med: .files/directory_exists .files/1_simulation_late_vaccination
	$(PYTHON) $(PIPELINE)/Outcomes/simulate.py -d med -j $(NCOREPER) -c $(CONFIG) --in-id $(RUN_ID) --out-id $(RUN_ID) --in-prefix test_simple/late_vaccination/2020.12.22.17:12:19.UTC/ --out-prefix test_simple/late_vaccination/med/2020.12.22.17:12:19.UTC/
	touch .files/1_hospitalization_late_vaccination_med

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
