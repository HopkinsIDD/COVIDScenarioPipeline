# Parse command-line
option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-p", "--pipepath"), action="store", default="COVIDScenarioPipeline", type='character', help="path to the COVIDScenarioPipeline directory"),
  optparse::make_option(c("-n", "--ncoreper"), action="store", default="1", type='character', help="Number of CPUS/jobs for pipeline"),
  optparse::make_option(c("-y", "--python"), action="store", default="python3", type='character', help="path to python executable")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}

# Parse config
config = covidcommon::load_config(opt$config)
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

if(generating_report)
{
  # Create report name (no suffix) for the .Rmd and .html
  report_name = ""
  if(length(config$report_location_name) != 0){
    report_name = config$report_location_name
  } else if(length(config$name) != 0){
    report_name = config$name
  } else {
    stop(paste("Please specify report_location_name or name in the config"))
  }
  report_name = paste0(report_name, "_",  format(Sys.Date(), format="%Y%m%d"))
}

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

# Generate first target
# If generating report, first target is the html file.
# Otherwise, first target is run.
# For both, the dependencies include all the simulation targets.
if(generating_report)
{
  rmd_file = sprintf("notebooks/%s/%s_report.Rmd", report_name, report_name)
  report_html_target_name = sprintf("notebooks/%s/%s_report.html", report_name, report_name)
  cat(paste0(report_html_target_name,":"))
} else {
  cat("run:")
}

for(scenario in scenarios)
{
  cat(" ")
  cat(simulation_target_name(simulations,scenario))
  for(deathrate in deathrates)
  {
    cat(" ")
    cat(hospitalization_target_name(simulations,scenario,deathrate))
  }
}

if(generating_report)
{
  # final target dependency for .html is the Rmd
  cat(sprintf(" %s\n", rmd_file))

  renderCmd = sprintf("\t$(RSCRIPT) -e 'rmarkdown::render(\"%s\"", rmd_file)
  renderCmd = paste0(renderCmd, sprintf(", params=list(state_usps=\"%s\"", config$report$state_usps))
  if(length(config$report$continue_on_error) != 0)
  {
    renderCmd = paste0(renderCmd, 
                      sprintf(", continue_on_error=%s", config$report$continue_on_error))
  }
  renderCmd = paste0(renderCmd, "))'")
  cat(renderCmd)

  rmd_target = sprintf("
%s:
\tmkdir -p notebooks/%s
\t$(RSCRIPT) -e 'rmarkdown::draft(\"$@\",template=\"state_report\",package=\"report.generation\",edit=FALSE)'", 
rmd_file, report_name)
  cat(rmd_target)
}
cat("\n")

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

cat("
.files:
\tmkdir $@
")


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
clean: clean_simulations clean_hospitalization"))
if(using_importation){
  cat(" clean_importation clean_filter")
}
if(generating_report)
{
  cat(" clean_reports")
}
cat("
\trm -rf .files
clean_simulations: rerun_simulations
\trm -rf model_output
clean_hospitalization: rerun_hospitalization
\trm -rf hospitalization
")
if(generating_report)
{
  cat(paste0("
clean_reports:
\trm -f ",report_html_target_name))
}


sink(NULL)
