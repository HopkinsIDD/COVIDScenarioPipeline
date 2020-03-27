config_file = ("config.yml")

config = covidcommon::load_config(config_file)

simulations = config$nsimulations
scenarios = config$interventions$scenarios
deathrates = config$hospitalization$parameters$p_death_names

cat(simulations)
cat("\n")
cat(scenarios)
cat("\n")
cat(deathrates)
cat("\n")

hospitalizaiton_target_name <- function(simulation,scenario,deathrate, prefix = ''){
  paste0(".files/",prefix,simulation,"_hospitalization_",scenario,"_",deathrate)
}

hospitalizaiton_make_command <- function(simulation,scenario,deathrate, prefix = ''){
  target_name <- hospitalizaiton_target_name(simulation,scenario,deathrate, prefix = prefix)
  dependency_name <- simulation_target_name(simulation,scenario, prefix = prefix)
  command_name <- paste0("$(RSCRIPT) $(PIPELINE)/R/scripts/hosp_run.R -s ",scenario," -d ",deathrate, " -j $(NCOREPER)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

simulation_target_name <- function(simulation,scenario, prefix = ''){
  paste0(".files/", prefix,simulation,"_simulation_",scenario)
}

simulation_make_command <- function(simulation,scenario,previous_simulation, prefix = ''){
  target_name <- simulation_target_name(simulation,scenario, prefix = prefix)
  dependency_name <- ""
  if(!is.na(previous_simulation)){
    dependency_name <- simulation_target_name(previous_simulation,scenario, prefix = prefix)
  } else {
    previous_simulation <- 0
  }
  command_name <- paste0("$(PYTHON) $(PIPELINE)/simulate.py -c $(CONFIG) -s ",scenario," -n ",simulation - previous_simulation," -j $(NCOREPER)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

sink("Makefile")

 
cat("
.PHONY: rerun rerun_simulations rerun_hospitalization clean_hospitalization clean clean_simulations

NCOREPER=4
RSCRIPT=Rscript
PYTHON=python3
PIPELINE=COVIDScenarioPipeline/
")
cat(paste0("CONFIG=",config_file),"\n")
cat(paste0("OUTPUTBASE=",config$name),"\n")

cat("report:")
for(scenario in scenarios){
  cat(" ")
  cat(simulation_target_name(simulations,scenario))
  for(deathrate in deathrates){
    cat(" ")
    cat(hospitalizaiton_target_name(simulations,scenario,deathrate))
  }
}
cat("\n	Rscript compile_Rmd.R\n")

for(sim_idx in seq_len(length(simulations))){
  sim <- simulations[sim_idx]
  prev_sim <- dplyr::lag(simulations)[sim_idx]
  for(scenario in scenarios){
    cat(simulation_make_command(sim,scenario,prev_sim))
    for(deathrate in deathrates){
      cat(hospitalizaiton_make_command(sim,scenario,deathrate))
    }
  }
}

cat("

rerun: rerun_simulations rerun_hospitalization
rerun_simulations: clean_simulations
	rm -f .files/1*_simulation*
rerun_hospitalization:
	rm -f .files/1*_hospitalization*
clean: clean_simulations clean_hospitalization clean_reports
clean_reports:
	rm -f notebooks/*.html
clean_simulations: rerun_simulations
	rm -rf model_output
clean_hospitalization: rerun_hospitalization
	rm -rf hospitalization
")

cat("production_report:")
for(scenario in scenarios){
  cat(" ")
  cat(simulation_target_name(simulations[length(simulations)],scenario, prefix='production_'))
  for(deathrate in deathrates){
    cat(" ")
    cat('production_',hospitalizaiton_target_name(simulations[length(simulations)],scenario, deathrate,prefix='production_'))
  }
}
cat("\n	Rscript compile_Rmd.R\n")

for(sim_idx in seq_len(length(simulations))){
  sim <- simulations[sim_idx]
  prev_sim <- dplyr::lag(simulations)[sim_idx]
  for(scenario in scenarios){
    cat(simulation_make_command(sim,scenario,NA,prefix='production_'))
    for(deathrate in deathrates){
      cat(hospitalizaiton_make_command(sim,scenario,deathrate,prefix='production_'))
    }
  }
}
sink(NULL)
