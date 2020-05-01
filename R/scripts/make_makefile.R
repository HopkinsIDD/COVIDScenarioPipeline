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
building_US_setup <- ("modeled_states" %in% names(config$spatial_setup))
using_static_filter <- ("dynfilter_path" %in% names(config))

if(generating_report)
{
  # Create report name (no suffix) for the .Rmd and .html
  report_name <- ""
  if(length(config$report_location_name) != 0){
    report_name = config$report_location_name
  } else if(length(config$name) != 0){
    report_name = config$name
  } else {
    stop(paste("Please specify report_location_name or name in the config"))
  }
  report_name <- paste0(report_name, "_",  format(Sys.Date(), format="%Y%m%d"))
}

importation_target_name <- function(simulation, prefix = ""){
  paste0(".files/",prefix,simulation,"_importation")
}

importation_make_command <- function(simulation,prefix=""){
  target_name <- importation_target_name(simulation,prefix)
  dependency_name <- ""
  command_name <- "$(RSCRIPT) $(PIPELINE)/R/scripts/importation.R -c $(CONFIG) -j $(NCOREPER)"
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

build_US_setup_target_name <- function() {
  return(file.path(config$spatial_setup$base_path, config$spatial_setup$mobility))
}

build_US_setup_make_command <- function() {
  cmd <- paste0(build_US_setup_target_name(),":\n")
  cmd <- paste0(cmd, "\tmkdir -p ", config$spatial_setup$base_path, "\n")
  cmd <- paste0(cmd, "\t$(RSCRIPT) $(PIPELINE)/R/scripts/build_US_setup.R -c $(CONFIG) -p $(PIPELINE)")
  return(cmd)
}

filter_target_name <- function(simulation, prefix = "" ){
  paste0(".files/",prefix,simulation,"_filter")
}

filter_make_command <- function(simulation,prefix=""){
  target_name <- filter_target_name(simulation,prefix)
  dependency_name <- importation_target_name(simulation, prefix=prefix)
  if(building_US_setup) {
    dependency_name <- paste(dependency_name, build_US_setup_target_name())
  }
  command_name<- paste0("$(RSCRIPT) $(PIPELINE)/R/scripts/create_filter.R -c $(CONFIG)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files/directory_exists ",
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
  command_name <- paste("$(RSCRIPT) $(PIPELINE)/R/scripts/hosp_run.R -s",scenario,
                          "-d",deathrate,"-j $(NCOREPER) -c $(CONFIG) -p $(PIPELINE)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files/directory_exists ",
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
  if(building_US_setup){
    dependency_name <- paste(dependency_name, build_US_setup_target_name())
  }
  if(using_importation){
    dependency_name <- paste(dependency_name,importation_target_name(simulation,prefix))
  }
  if(using_static_filter){
    dependency_name <- paste(dependency_name, filter_target_name(simulation,prefix))
  }
  command_name <- paste0("$(PYTHON) -m SEIR -c $(CONFIG) -s ",scenario," -n ",simulation - previous_simulation," -j $(NCOREPER)")
  touch_name <- paste0("touch ",target_name)
  return(paste0(
    target_name, ": .files/directory_exists ",
    dependency_name, "\n", 
    "\t",command_name, "\n",
    "\t",touch_name, "\n"
  ))
}

report_html_target_name <- function(report_name) {
  return(sprintf("notebooks/%s/%s_report.html", report_name, report_name))
}

report_html_make_command <- function(report_name, scenarios, simulations, deathrates, config) {
  rmd_file = report_rmd_target_name(report_name)
  s <- run_dependencies(scenarios, simulations, deathrates)
  s <- paste0(s, " ", rmd_file,"\n")

  renderCmd = sprintf("\t$(RSCRIPT) -e 'rmarkdown::render(\"%s\"", rmd_file)
  renderCmd = paste0(renderCmd, sprintf(", params=list(state_usps=\"%s\"", config$report$state_usps))
  if(length(config$report$continue_on_error) != 0)
  {
    renderCmd = paste0(renderCmd, 
                      sprintf(", continue_on_error=%s", config$report$continue_on_error))
  }
  renderCmd = paste0(renderCmd, "))'")

  s <- paste0(s, renderCmd, "\n")
  return(s)
}

report_rmd_target_name <- function(report_name) {
  return(sprintf("notebooks/%s/%s_report.Rmd", report_name, report_name))
}

report_rmd_make_command <- function(report_name) {
  return(sprintf("%s:
\tmkdir -p notebooks/%s
\t$(RSCRIPT) -e 'rmarkdown::draft(\"$@\",template=\"state_report\",package=\"report.generation\",edit=FALSE)'\n",
report_rmd_target_name(report_name), report_name))
}

run_dependencies <- function(scenarios, simulations, deathrates) {
  s <- ":"
  for(scenario in scenarios)
  {
    s <- paste0(s, " ", simulation_target_name(simulations, scenario))
    for(deathrate in deathrates)
    {
      s <- paste0(s, " ", hospitalization_target_name(simulations, scenario, deathrate))
    }
  }
  return(s)
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
if(generating_report) {
  cat(report_html_target_name(report_name))
  cat(report_html_make_command(report_name, scenarios, simulations, deathrates, config))
  cat(report_rmd_make_command(report_name))
} else {
  cat("run")
  cat(run_dependencies(scenarios, simulations, deathrates))
}

cat("\n")

if(building_US_setup){
  cat(build_US_setup_make_command())
}

cat("\n")

if(using_importation){
  for(sim_idx in seq_len(length(simulations))){
    cat(importation_make_command(simulations[sim_idx]))
  }
}
if(using_static_filter){
  for(sim_idx in seq_len(length(simulations))){
    cat(filter_make_command(simulations[sim_idx]))
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
.files/directory_exists:
\tmkdir -p .files
\ttouch .files/directory_exists
")


cat("\n\nrerun: rerun_simulations rerun_hospitalization")

if(using_importation){
  cat(" rerun_importation")
}
if(using_static_filter){
  cat(" rerun_filter")
}
cat("\n")

if(using_static_filter){
  cat(paste0("clean_filter: rerun_filter
\trm -rf ",config$dynfilter_path,"
rerun_filter:
\trm -f .files/*_filter
"))
}
if(using_importation){
  cat(paste0("
clean_importation: rerun_importation
\trm -rf data/case_data
\trm -rf importation
rerun_importation:
\trm -f .files/*_importation
"))
}
cat(paste0("
rerun_simulations: clean_simulations
\trm -f .files/*_simulation*
rerun_hospitalization:
\trm -f .files/*_hospitalization*
clean: clean_simulations clean_hospitalization"))
if(using_importation){
  cat(" clean_importation")
}
if(using_static_filter){
  cat(" clean_filter")
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
\trm -f ",report_html_target_name(report_name)))
}


sink(NULL)
