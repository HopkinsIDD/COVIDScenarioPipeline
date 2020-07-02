## Preamble ---------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(readr))
suppressMessages(library(covidcommon))
suppressMessages(library(report.generation))
suppressMessages(library(stringr))
suppressMessages(library(foreach))
suppressMessages(library(magrittr))
suppressMessages(library(xts))
suppressMessages(library(reticulate))
suppressMessages(library(truncnorm))
suppressMessages(library(parallel))
# suppressMessages(library(flock))
options(warn=1)

option_list = list(
  optparse::make_option(c("-c", "--config"), action="store", default=Sys.getenv("COVID_CONFIG_PATH", Sys.getenv("CONFIG_PATH")), type='character', help="path to the config file"),
  optparse::make_option(c("-u","--run_id"), action="store", type='character', help="Unique identifier for this run", default = Sys.getenv("COVID_RUN_INDEX",covidcommon::run_id())),
  optparse::make_option(c("-s", "--scenarios"), action="store", default=Sys.getenv("COVID_SCENARIOS", 'all'), type='character', help="name of the intervention to run, or 'all' to run all of them"),
  optparse::make_option(c("-d", "--deathrates"), action="store", default=Sys.getenv("COVID_DEATHRATES", 'all'), type='character', help="name of the death scenarios to run, or 'all' to run all of them"),
  optparse::make_option(c("-j", "--jobs"), action="store", default=Sys.getenv("COVID_NJOBS", parallel::detectCores()), type='integer', help="Number of jobs to run in parallel"),
  optparse::make_option(c("-k", "--simulations_per_slot"), action="store", default=Sys.getenv("COVID_SIMULATIONS_PER_SLOT", NA), type='integer', help = "number of simulations to run for this slot"),
  optparse::make_option(c("-i", "--this_slot"), action="store", default=Sys.getenv("COVID_SLOT_INDEX", 1), type='integer', help = "id of this slot"),
  optparse::make_option(c("-b", "--this_block"), action="store", default=Sys.getenv("COVID_BLOCK_INDEX",1), type='integer', help = "id of this block"),
  optparse::make_option(c("-p", "--pipepath"), action="store", type='character', help="path to the COVIDScenarioPipeline directory", default = Sys.getenv("COVID_PATH", "COVIDScenarioPipeline/")),
  optparse::make_option(c("-y", "--python"), action="store", default=Sys.getenv("COVID_PYTHON_PATH","python3"), type='character', help="path to python executable"),
  optparse::make_option(c("-r", "--rpath"), action="store", default=Sys.getenv("COVID_RSCRIPT_PATH","Rscript"), type = 'character', help = "path to R executable")
)

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

print(opt)

reticulate::use_python(Sys.which(opt$python),require=TRUE)
## Block loads the config file and geodata
if(opt$config == ""){
  optparse::print_help(parser)
  stop(paste(
    "Please specify a config YAML file with either -c option or CONFIG_PATH environment variable."
  ))
}
config = covidcommon::load_config(opt$config)

if(!('perturbation_sd' %in% names(config$seeding))) {
  stop("The key seeding::perturbation_sd is required in the config file.")
}
if(config$seeding$method != 'FolderDraw'){
  stop("This filtration method requires the seeding method 'FolderDraw'")
}
if(!('lambda_file' %in% names(config$seeding))) {
  stop("Despite being a folder draw method, filtration method requires the seeding to provide a lambda_file argument.")
}

##Load infromationon geographic locations from geodata file.
suppressMessages(geodata <- report.generation::load_geodata_file(
  paste(
    config$spatial_setup$base_path,
    config$spatial_setup$geodata, sep = "/"
  ),
  geoid_len=5 #Is this hardcode a good idea.
))
obs_nodename <- config$spatial_setup$nodenames

##Load simulations per slot from config if not defined on command line
##command options take precendence
if(is.na(opt$simulations_per_slot)){
  opt$simulations_per_slot <- config$filtering$simulations_per_slot
}
print(paste("Running",opt$simulations_per_slot,"simulations"))

##Define data directory and create if it does not exist
data_path <- config$filtering$data_path
data_dir <- dirname(data_path)
if(!dir.exists(data_dir)){
  suppressWarnings(dir.create(data_dir,recursive=TRUE))
}

# Parse scenarios arguments
##If death rates are specified check their existence
deathrates <- opt$deathrates
if(all(deathrates == "all")) {
  deathrates<- config$outcomes$scenarios
} else if (!(deathrates %in% config$outcomes$scenarios)) {
  message(paste("Invalid death rate argument:", deathrate, "did not match any of the named args in", paste( p_death, collapse = ", "), "\n"))
  quit("yes", status=1)
}

##If scenarios are specified check their existence
scenarios <- opt$scenarios
if (all(scenarios == "all")){
  scenarios <- config$interventions$scenarios
} else if (!all(scenarios %in% config$interventions$scenarios)) {
  message(paste("Invalid scenario argument:",scenario, "did not match any of the named args in ", paste(config$interventions$scenarios, collapse = ", "), "\n"))
  quit("yes", status=1)
}


##Creat heirarchical stats object if specified
hierarchical_stats <- list()
if("hierarchical_stats_geo"%in%names(config$filtering)) {
    hierarchical_stats <- config$filtering$hierarchical_stats_geo
}


##Create priors if specified
defined_priors <- list()
if("priors"%in%names(config$filtering)) {
    defined_priors <- config$filtering$priors
}



## Runner Script---------------------------------------------------------------------

obs <- inference::get_ground_truth(data_path,geodata[[obs_nodename]],obs_nodename, config$start_date, config$end_date)

geonames <- unique(obs[[obs_nodename]])

## Compute statistics
data_stats <- lapply(
  geonames,
  function(x) {
    df <- obs[obs[[obs_nodename]] == x, ]
    inference::getStats(
      df,
      "date",
      "data_var",
      stat_list = config$filtering$statistics)
  }) %>%
    set_names(geonames)

required_packages <- c("dplyr", "magrittr", "xts", "zoo", "stringr")

## python configuration for minimal_interface.py
reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
reticulate::py_run_string(paste0("run_id = '", opt$run_id, "'"))
reticulate::import_from_path("SEIR", path=opt$pipepath)
reticulate::import_from_path("Outcomes", path=opt$pipepath)
reticulate::py_run_string(paste0("index = ", 1))

for(scenario in scenarios) {

  reticulate::py_run_string(paste0("scenario = '", scenario, "'"))

  for(deathrate in deathrates) {
    # Data -------------------------------------------------------------------------
    # Load

    slot_prefix <- covidcommon::create_prefix(config$name,scenario,deathrate,opt$run_id,sep='/',trailing_separator='/')

    gf_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','final',sep='/',trailing_separator='/')
    ci_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'chimeric','intermediate',sep='/',trailing_separator='/')
    gi_prefix <- covidcommon::create_prefix(prefix=slot_prefix,'global','intermediate',sep='/',trailing_separator='/')


    chimeric_block_prefix <- covidcommon::create_prefix(prefix=ci_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
    chimeric_local_prefix <- covidcommon::create_prefix(prefix=chimeric_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')

    global_block_prefix <- covidcommon::create_prefix(prefix=gi_prefix, slot=list(opt$this_slot,"%09d"), sep='.', trailing_separator='.')
    global_local_prefix <- covidcommon::create_prefix(prefix=global_block_prefix, slot=list(opt$this_block,"%09d"), sep='.', trailing_separator='.')


    ## pass prefix to python and use
    reticulate::py_run_string(paste0("deathrate = '", deathrate, "'"))
    reticulate::py_run_string(paste0("prefix = '", chimeric_block_prefix, "'"))
    reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))


    first_spar_file <- covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block - 1,'spar','parquet')
    first_snpi_file <- covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block - 1,'snpi','parquet')
    first_hosp_file <- covidcommon::create_file_name(opt$run_id,global_block_prefix,opt$this_block - 1,'hosp','parquet')
    first_hpar_file <- covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block - 1,'hpar','parquet')
    first_seed_file <- covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block - 1,'seed','csv')
    first_chim_file <- covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block - 1,'llik','parquet')
    first_llik_file <- covidcommon::create_file_name(opt$run_id,global_block_prefix,opt$this_block - 1,'llik','parquet')

    # lock <- flock::lock(paste('.lock',gsub('/','-',config$seeding$lambda_file),sep='/'))
    err <- 0
    if(!file.exists(first_seed_file)){
      if(opt$this_block > 1){
        print("Looking for")
        print(first_seed_file)
        print("Found")
        print(list.files(dirname(first_seed_file)))
        stop("Problem resuming seeding after first block")
      }
      print(sprintf("Creating Seeding (%s) from Scratch",first_seed_file))
      if(!file.exists(config$seeding$lambda_file)){
        err <- system(paste(
          opt$rpath,
          paste(opt$pipepath,"R","scripts","create_seeding.R", sep='/'),
          "-c",opt$config
        ))
          if(err != 0){
            stop("Could not run seeding")
          }
      }
      suppressMessages(initial_seeding <- readr::read_csv(config$seeding$lambda_file, col_types=readr::cols(place=readr::col_character())))
      write.csv(
        initial_seeding,
        file = first_seed_file
      )
    }

    ##read first seeding file.
    suppressMessages(initial_seeding <- readr::read_csv(first_seed_file, col_types=readr::cols(place=readr::col_character())))
    # flock::unlock(lock)
    initial_seeding$amount <- as.integer(round(initial_seeding$amount))

    current_index <- 0

##### Load or create first NPI file
    # FIX ME : this file won't exist in general
    # TODO CHANGE TO FIRST DRAW OF SEIR CODE
    # lock <- flock::lock(paste('.lock',gsub('/','-',first_snpi_file),sep='/'))
    if((!file.exists(first_snpi_file)) | (!file.exists(first_spar_file))){
      print(sprintf("Creating parameters (%s) and (%s) from Scratch",first_snpi_file, first_spar_file))
      py$onerun_SEIR(opt$this_block - 1,py$s)
    }
    initial_snpi <- arrow::read_parquet(first_snpi_file)
    initial_spar <- arrow::read_parquet(first_spar_file)
    # flock::unlock(lock)

##### Load or create first hapr file.
    if(!file.exists(first_hpar_file)){
      if(opt$this_block > 1){stop("Problem resuming hospitalization parameters after first block")}
      print(sprintf("Creating hospitalization parameters (%s) from config specified file %s",first_hpar_file,config$outcomes$param_place_file))
      file.copy(config$outcomes$param_place_file,first_hpar_file)
    }
      initial_hpar <- arrow::read_parquet(first_hpar_file)


##### Load or create first hospitalization file
    if(!file.exists(first_hosp_file)){
      if(opt$this_block > 1){stop("Problem resuming hospitalization after first block")}
      print(sprintf("Creating hospitalization (%s) from Scratch",first_hosp_file))
      ## Generate files
      this_index <- opt$this_block - 1

      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("SEIR failed to run")
      }

      ## Run hospitalization
      err <- py$onerun_HOSP(this_index)
      err <- ifelse(err == 1,0,1)
      if(length(err) == 0){
        stop("HOSP failed to run")
      }
      if(err != 0){
        stop("HOSP failed to run")
      }

      file.copy(
        covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,this_index,'hosp','parquet'),
        first_hosp_file
      )
    }

    initial_sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",first_hosp_file))(first_hosp_file) %>%
        filter(time >= min(obs$date), time <= max(obs$date))

#####Aligning location names between observations and hosptial sims
    if(!(obs_nodename %in% names(initial_sim_hosp))){stop(paste("Missing column",obs_nodename,"from hospitalization output"))}
    lhs <- unique(initial_sim_hosp[[obs_nodename]])
    rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]

    if(!file.exists(first_llik_file)){
      if(opt$this_block > 1){
        print(paste("Looking for",first_llik_file,"found",paste(list.files("model_output/llik",recursive=TRUE),collapse=', ')))
        stop("Problem resuming global likelihood after first block")
      }
      print(sprintf("Creating likelihood (%s) from Scratch",first_llik_file))
      global_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
        all_locations,
        initial_sim_hosp,
        obs_nodename,
        config,
        obs,
        data_stats,
        first_llik_file,
        hierarchical_stats,
        defined_priors,
        geodata,
        initial_snpi,
        dplyr::mutate(initial_hpar,parameter=paste(quantity,source,outcome,sep='_'))
      )
      arrow::write_parquet(global_likelihood_data,first_llik_file)
    }
    global_likelihood_data <- arrow::read_parquet(first_llik_file)
    rm(initial_sim_hosp) ### no longer needed

#####Create fist chimeric liklihoo dwith global_likelihood if it does not exist. Load from disk if needed
    if(!file.exists(first_chim_file)){
      if(opt$this_block > 1){
        print(paste("Looking for",first_chim_file,"found",paste(list.files("model_output/llik",recursive=TRUE),collapse=', ')))
        stop("Problem resuming chimeric likelihood after first block")
      }
      print(sprintf("Creating likelihood (%s) from Scratch",first_chim_file))
      ## global_likelihood_data is the same as chimeric before we've done any inference
      arrow::write_parquet(global_likelihood_data,first_chim_file)
    }

    chimeric_likelihood_data <- arrow::read_parquet(first_chim_file)


#####Get the full likelihood (WHY IS THIS A DATA FRAME)
    # Compute total loglik for each sim
    global_likelihood <- global_likelihood_data %>%
      summarise(ll = sum(ll, na.rm = T)) %>%
      mutate(pdeath = deathrate, scenario = scenario)


    ## For logging
    current_index <- 0


#####LOOP NOTES
### current means proposed
### initial means accepted/current

#####Loop over simulations in this block
   for( this_index in seq_len(opt$simulations_per_slot)) {
      print(paste("Running simulation", this_index))

      ## Create filenames
      this_spar_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'spar','parquet')
      this_snpi_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'snpi','parquet')
      this_hosp_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'hosp','parquet')
      this_hpar_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'hpar','parquet')
      this_seed_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'seed','csv')
      this_chim_file <- covidcommon::create_file_name(opt$run_id,chimeric_local_prefix,this_index,'llik','parquet')
      this_llik_file <- covidcommon::create_file_name(opt$run_id,global_local_prefix,this_index,'llik','parquet')

      ## Setup python
      reticulate::py_run_string(paste0("prefix = '", global_local_prefix, "'"))
      reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))

      ## Do perturbations from accepted
      proposed_seeding <- inference::perturb_seeding(initial_seeding,config$seeding$perturbation_sd,
                                                    c(lubridate::ymd(c(config$start_date,config$end_date))))
      proposed_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
      proposed_spar <- initial_spar
      if(!deathrate %in% names(config$outcomes$settings)){
        stop(paste("Deathrate",deathrate,"does not appear in outcomes::settings in the config"))
      }
      proposed_hpar <- inference::perturb_hpar(initial_hpar, config$outcomes$settings[[deathrate]])

      ## Write files that need to be written for other code to read
      write.csv(proposed_seeding,this_seed_file)
      arrow::write_parquet(proposed_snpi,this_snpi_file)
      arrow::write_parquet(proposed_spar,this_spar_file)
      arrow::write_parquet(proposed_hpar,this_hpar_file)

      ## Run SEIR
      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("SEIR failed to run")
      }

      err <- py$onerun_HOSP(this_index)
      err <- ifelse(err == 1,0,1)
      if(length(err) == 0){
        stop("HOSP failed to run")
      }
      if(err != 0){
        stop("HOSP failed to run")
      }

      sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",this_hosp_file))(this_hosp_file) %>%
        filter(time <= max(obs$date))



      lhs <- unique(sim_hosp[[obs_nodename]])
      rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]

      proposed_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(
        all_locations,
        sim_hosp,
        obs_nodename,
        config,
        obs,
        data_stats,
        this_llik_file,
        hierarchical_stats,
        defined_priors,
        geodata,
        proposed_snpi,
        dplyr::mutate(proposed_hpar,parameter=paste(quantity,source,outcome))
      )


      rm(sim_hosp)

      ## UNCOMMENT TO DEBUG
      ## print(global_likelihood_data)
      ## print(chimeric_likelihood_data)
      ## print(proposed_likelihood_data)

      ## Compute total loglik for each sim
      proposed_likelihood <- proposed_likelihood_data %>%
        summarise(ll = sum(ll, na.rm = T)) %>%
        mutate(pdeath = deathrate, scenario = scenario)

      ## For logging
      print(paste("Current likelihood",global_likelihood,"Proposed likelihood",proposed_likelihood))

      if(inference::iterateAccept(global_likelihood, proposed_likelihood, 'll')){
        print("****ACCEPT****")
        current_index <- this_index
        global_likelihood <- proposed_likelihood
	global_likelihood_data <- proposed_likelihood_data
      }
      arrow::write_parquet(global_likelihood_data, this_llik_file)

      seeding_npis_list <- inference::accept_reject_new_seeding_npis(
        seeding_orig = initial_seeding,
        seeding_prop = proposed_seeding,
        snpi_orig = initial_snpi,
        snpi_prop = proposed_snpi,
        hpar_orig = initial_hpar,
        hpar_prop = proposed_hpar,
        orig_lls = chimeric_likelihood_data,
        prop_lls = proposed_likelihood_data
      )
      initial_seeding <- seeding_npis_list$seeding
      initial_snpi <- seeding_npis_list$snpi
      initial_hpar <- seeding_npis_list$hpar
      chimeric_likelihood_data <- seeding_npis_list$ll
      arrow::write_parquet(initial_likelihood_data, this_chim_file)

      print(paste("Current index is ",current_index))
      # print(proposed_likelihood_data)
      # print(chimeric_likelihood_data)

      ###Memory managment
      rm(proposed_snpi)
      rm(proposed_hpar)
      rm(proposed_seeding)
    }

#####Do MCMC end copy. Fail if unsucessfull
      cpy_res <- inference::perform_MCMC_step_copies(current_index,
                                                     opt$this_slot,
                                                     opt$this_block,
                                                     opt$run_id,
                                                     global_local_prefix,
                                                     gf_prefix,
                                                     global_block_prefix)

      if(!prod(unlist(cpy_res))) {stop("File copy failed:", paste(unlist(cpy_res),paste(names(cpy_res),"|")))}


#####Write currently accepted files to disk
    readr::write_csv(initial_seeding,covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block,'seed','csv'))
    arrow::write_parquet(initial_snpi,covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block,'snpi','parquet'))
    arrow::write_parquet(initial_spar,covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block,'spar','parquet'))
    arrow::write_parquet(initial_hpar,covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block,'hpar','parquet'))
    arrow::write_parquet(chimeric_likelihood_data,covidcommon::create_file_name(opt$run_id,chimeric_block_prefix,opt$this_block,'llik','parquet'))
  }
}
