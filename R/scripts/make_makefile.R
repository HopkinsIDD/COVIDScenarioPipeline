option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-p", "--pipepath"), action="store", default="COVIDScenarioPipeline", type='character', help="path to the COVIDScenarioPipeline directory"),
  optparse::make_option(c("-n", "--ncoreper"), action="store", default="1", type='character', help="Number of CPUS/jobs for pipeline"),
  optparse::make_option(c("-y", "--python"), action="store", default="python3", type='character', help="path to python executable")
)

opt = optparse::parse_args(optparse::OptionParser(option_list=option_list))

config = covidcommon::load_config(opt$c)
if(isTRUE(config$this_file_is_unedited)){
  stop(paste(
    "Please make minimal edits to the config file before running this script.
    The config file details which edits to make in it's comments."
  ))
}

simulations = config$nsimulations
scenarios = config$interventions$scenarios
deathrates = config$hospitalization$parameters$p_death_names

cat(simulations)
cat("\n")
cat(scenarios)
cat("\n")
cat(deathrates)
cat("\n")

using_importation <- ("importation" %in% names(config))
generating_report <- ("report" %in% names(config))

importation_target_name <- function(simulation, prefix = ""){
  paste0(".files/",prefix,simulation,"_importation")
}

importation_make_command <- function(simulation,prefix=""){
  target_name <- importation_target_name(simulation,prefix)
  dependency_name <- ""
  command_name <- paste0("$(RSCRIPT) $(PIPELINE)/R/scripts/importation.R -c  $(CONFIG) -j $(NCOREPER)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

filter_target_name <- function(simulation, prefix = "" ){
  paste0(".files/",prefix,simulation,"_filter")
}

filter_make_command <- function(simulation,prefix=""){
  target_name <- filter_target_name(simulation,prefix)
  dependency_name <- importation_target_name(simulation, prefix=prefix)
  command_name<- paste0("$(RSCRIPT) $(PIPELINE)/R/scripts/create_filter.R -c $(CONFIG)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

hospitalization_target_name <- function(simulation,scenario,deathrate, prefix = ''){
  paste0(".files/",prefix,simulation,"_hospitalization_",scenario,"_",deathrate)
}

hospitalization_make_command <- function(simulation,scenario,deathrate, prefix = ''){
  target_name <- hospitalization_target_name(simulation,scenario,deathrate, prefix = prefix)
  dependency_name <- simulation_target_name(simulation,scenario, prefix = prefix)
  command_name <- paste0("$(RSCRIPT) $(PIPELINE)/R/scripts/hosp_run.R -s ",scenario," -d ",deathrate, " -j $(NCOREPER) -c $(CONFIG)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files ",
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
  if(using_importation){
    dependency_name <- paste(dependency_name,filter_target_name(simulation,prefix),importation_target_name(simulation,prefix))
  }
  command_name <- paste0("$(PYTHON) $(PIPELINE)/simulate.py -c $(CONFIG) -s ",scenario," -n ",simulation - previous_simulation," -j $(NCOREPER)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

sink("Makefile")

 
cat("
.PHONY: rerun rerun_simulations rerun_hospitalization clean_hospitalization clean clean_simulations

RSCRIPT=Rscript
")
cat(paste0("PYTHON=",opt$python,"\n"))
cat(paste0("NCOREPER=",opt$ncoreper,"\n"))
cat(paste0("PIPELINE=",opt$pipepath,"\n"))
cat(paste0("CONFIG=",opt$config,"\n\n"))

cat("
.files:
\tmkdir $@
")

if(generating_report){
  cat("report:")
} else {
  cat("run:")
}
for(scenario in scenarios){
  cat(" ")
  cat(simulation_target_name(simulations,scenario))
  for(deathrate in deathrates){
    cat(" ")
    cat(hospitalization_target_name(simulations,scenario,deathrate))
  }
}
cat("\n")

if(generating_report){
  cat("\tRscript compile_Rmd.R\n")
}

if(using_importation){
  for(sim_idx in seq_len(length(simulations))){
    
    cat(filter_make_command(simulations[sim_idx]))
    cat(importation_make_command(simulations[sim_idx]))
  }
}

for(sim_idx in seq_len(length(simulations))){
  sim <- simulations[sim_idx]
  prev_sim <- dplyr::lag(simulations)[sim_idx]
  for(scenario in scenarios){
    cat(simulation_make_command(sim,scenario,prev_sim))
    for(deathrate in deathrates){
      cat(hospitalization_make_command(sim,scenario,deathrate))
    }
  }
}

cat(paste0("

rerun: rerun_simulations rerun_hospitalization"
))

if(using_importation){
  cat(paste0("rerun_importation rerun_filter
clean_filter: rerun_filter
\trm -rf ",config$dynfilter_path,"
clean_importation: rerun_importation
\trm -rf importation
"))
}
cat(paste0("
rerun_filter:
\trm -f .files/1*_filter
rerun_importation:
\trm -f .files/1*_importation
rerun_simulations: clean_simulations
\trm -f .files/1*_simulation*
rerun_hospitalization:
\trm -f .files/1*_hospitalization*
clean: clean_simulations clean_hospitalization clean_reports
\trm -rf .files"
))
if(using_importation){
  cat(" clean_importation clean_filter")
}
cat(paste0("
clean_reports:
\trm -f notebooks/*.html
clean_simulations: rerun_simulations
\trm -rf model_output
clean_hospitalization: rerun_hospitalization
\trm -rf hospitalization
"))

sink(NULL)
