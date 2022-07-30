suppressMessages(library(parallel))
suppressMessages(library(foreach))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
options(readr.num_columns = 0)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
  optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "number of simulations to run for this slot"),
  optparse::make_option(c("-n", "--slots"), action="store", default=Sys.getenv("COVID_NSIMULATIONS", as.numeric(NA)), type='integer', help = "Number of slots to run."),
  optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
  optparse::make_option(c("-t", "--stoch_traj_flag"), action="store", default=Sys.getenv("COVID_STOCHASTIC",FALSE), type='logical', help = "Stochastic SEIR and outcomes trajectories if true"),
  optparse::make_option(c("--ground_truth_start"), action = "store", default = Sys.getenv("COVID_GT_START", ""), type = "character", help = "First date to include groundtruth for"),
  optparse::make_option(c("--ground_truth_end"), action = "store", default = Sys.getenv("COVID_GT_END", ""), type = "character", help = "Last date to include groundtruth for"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
  optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable"),
  optparse::make_option(c("-R", "--is-resume"), action="store", default=Sys.getenv("COVID_IS_RESUME",FALSE), type = 'logical', help = "Is this run a resume"),
  optparse::make_option(c("-I", "--is-interactive"), action="store", default=Sys.getenv("COVID_INTERACTIVE",Sys.getenv("INTERACTIVE_RUN", FALSE)), type = 'logical', help = "Is this run an interactive run"),
  optparse::make_option(c("-L", "--reset_chimeric_on_accept"), action = "store", default = Sys.getenv("COVID_RESET_CHIMERICS", FALSE), type = 'logical', help = 'Should the chimeric parameters get reset to global parameters when a global acceptance occurs')
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

print("Starting")
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}

print(paste('Running ',opt$j,' jobs in parallel'))

config <- covidcommon::load_config(opt$config)

deathrates <- opt$deathrates
if(all(deathrates == "all")) {
  deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)){
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

scenarios <- opt$scenarios
if (all(scenarios == "all")){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario arguments: [",paste(setdiff(scenarios, config$interventions$scenarios)), "] did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
  quit("yes", status=1)
}

if(is.na(opt$simulations_per_slot)) {
  opt$simulations_per_slot <- config$filtering$simulations_per_slot
}

if(is.na(opt$slots)) {
  opt$slots <- config$nsimulations
}

cl <- parallel::makeCluster(opt$j)
doParallel::registerDoParallel(cl)
covidcommon::prettyprint_optlist(list(scenarios=scenarios,deathrates=deathrates,slots=seq_len(opt$slots)))
foreach(scenario = scenarios) %:%
foreach(deathrate = deathrates) %:%
foreach(slot = seq_len(opt$slots)) %dopar% {
  print(paste("Slot",slot,"of",opt$slots))
  err <- system(
    paste(
      opt$rpath,
      paste(
        opt$pipepath,"R","scripts","filter_MC.R",sep='/'),
        "-c", opt$config,
        "-u", opt$run_id,
        "-s", opt$scenarios,
        "-d", opt$deathrates,
        "-j", opt$jobs,
        "-k", opt$simulations_per_slot,
        "-i", slot,
        "-b", opt$this_block,
        "-t", opt$stoch_traj_flag,
        ifelse(nchar(opt$ground_truth_start) > 0, c("--ground_truth_start", opt$ground_truth_start), NULL),
        ifelse(nchar(opt$ground_truth_end) > 0, c("--ground_truth_end", opt$ground_truth_end), NULL),
        "-p", opt$pipepath,
        "-y", opt$python,
        "-r", opt$rpath,
        "-R", opt[["is-resume"]],
        "-I", opt[["is-interactive"]],
        "-L", opt$reset_chimeric_on_accept
      )
    )
  if(err != 0){quit("no")}
}
parallel::stopCluster(cl)
