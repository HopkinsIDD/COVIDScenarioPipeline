suppressMessages(library(parallel))

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
  optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrate"), action="store", default=Sys.getenv("COVID_DEATHRATES" 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--sims_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT"), type='integer', help = "Number of simulations to run per slot"),
  optparse::make_option(c("-n", "--slots"), action="store", default=Sys.getenv("COVID_NSIMULATIONS", 1), type='integer', help = "Number of slots to run."),
  optparse::make_option(c("-y", "--python"), action="store", default="python3", type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default="Rscript", type = 'character', help = "path to R executable")
)

# install.packages('xts', repos='http://cran.us.r-project.org')
# install.packages('zoo', repos='http://cran.us.r-project.org')
# install.packages('covidImportation',type='source',repos=NULL)
# devtools::install_github("HopkinsIDD/covidImportation")

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}

config <- covidcommon::load_config(opt$config)

deathrates <- opt$deathrates
if(all(deathrates == "all")) {
  deathrates<- config$hospitalization$parameters$p_death_names
} else if (!(deathrates %in% config$hospitalization$parameters$p_death_names)) {
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

scenarios <- opt$scenarios
if (all(scenarios == "all")){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
  quit("yes", status=1)
}

if(is.na(opt$sims_per_slot)) {
  opt$sims_per_slot <- config$filtering$simulations_per_slot
}

if(is.na(opt$slots)) {
  opt$slots <- config$nsimulations
}

library(foreach)
cl <- parallel::makeCluster(opt$j)
doParallel::registerDoParallel(cl)
foreach(scenario = scenarios) %:%
foreach(deathrate = deathrates) %:%
foreach(slot = seq_len(opt$slots)) %dopar% {
  print(paste("Slot",slot,"of",opt$slots))
  err <- system(
    paste(
      opt$rpath,
      paste(
        opt$pipepath,"R","scripts","filter_MC.R",sep='/'),
        "-c",opt$config,
        "-s",scenario,
        "-d",deathrate,
        "-j",1,
        "-k",opt$sims_per_slot,
        "-n",opt$slots,
        "-i",slot,
        "-y",opt$python,
        "-r",opt$rpath,
        "-p",opt$pipepath
      )
    )
  if(err != 0){quit("no")}
}
parallel::stopCluster(cl)
