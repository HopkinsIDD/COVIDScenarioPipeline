option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("CONFIG_PATH"), type='character', help="path to the config file"),
  optparse::make_option(c("-s", "--scenarios"), action="store", default='all', type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrates"), action="store", default='all', type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default="8", type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=NA, type='integer', help = "number of simulations to run for this slot"),
  optparse::make_option(c("-n", "--number_of_simulations"), action="store", default="1", type='integer', help = "number of slots to run"),
  optparse::make_option(c("-i", "--this_slot"), action="store", default="1", type='integer', help = "id of this slot"),
  optparse::make_option(c("-y", "--python"), action="store", default="python3", type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default="Rscript", type = 'character', help = "path to R executable"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = "COVIDScenarioPipeline/"),
  optparse::make_option(c("--clean"), action="store_true",default=FALSE,help="Remove old files if unused"),
  optparse::make_option(c("--dontclean"), action="store_false",dest="clean",help="Don't remove old files if unused")
)

filter_MC(
  config = "~/projects/COVID19_USA/config.yml",
  scenarios = "all",
  deathrates = "all",
  jobs = 4,
  simulations_per_slot = NA,
  number_of_simulations = 1,
  this_slot = 1,
  python = "python3",
  rpath = "Rscript",
  pipepath = "COVIDScenarioPipeline/",
  clean = T
)
