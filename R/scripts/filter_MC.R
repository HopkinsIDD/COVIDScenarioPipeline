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
# suppressMessages(library(flock))
options(warn=1)

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

parser=optparse::OptionParser(option_list=option_list)
opt = optparse::parse_args(parser)

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
  deathrates<- config$hospitalization$parameters$p_death_names
} else if (!(deathrates %in% config$hospitalization$parameters$p_death_names)) {
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
if("hierarchical_stats_geo"%in%names(config$filtering)) {
    hierarchical_stats <- config$filtering$hierarchical_stats_geo    
} else {
    heirarchical_state <- list() ##no stats
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
for(scenario in scenarios) {

  ## One time setup for python
  reticulate::py_run_string(paste0("config_path = '", opt$config,"'"))
  reticulate::py_run_string(paste0("scenario = '", scenario, "'"))
  reticulate::import_from_path("SEIR", path=opt$pipepath)
  reticulate::py_run_file(paste(opt$pipepath,"minimal_interface.py",sep='/'))

  for(deathrate in deathrates) {
    # Data -------------------------------------------------------------------------
    # Load
    first_spar_file <- covidcommon::spar_file_path(config,opt$this_slot, scenario)
    first_snpi_file <- covidcommon::snpi_file_path(config,opt$this_slot, scenario)
    first_hosp_file <- covidcommon::hospitalization_file_path(config,opt$this_slot, scenario, deathrate)
    first_hpar_file <- covidcommon::hpar_file_path(config,opt$this_slot, scenario, deathrate)
    first_seeding_file <- covidcommon::seeding_file_path(config,opt$this_slot)

    # lock <- flock::lock(paste('.lock',gsub('/','-',config$seeding$lambda_file),sep='/'))
    err <- 0
    if(!file.exists(first_seeding_file)){
      print(sprintf("Creating Seeding (%s) from Scratch",first_seeding_file))
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
      suppressMessages(initial_seeding <- readr::read_csv(config$seeding$lambda_file, col_types = list(place = readr::col_character())))
      write.csv(
        initial_seeding,
        file =first_seeding_file
      )
    }
      suppressMessages(initial_seeding <- readr::read_csv(first_seeding_file, col_types = list(place = readr::col_character())))
    # flock::unlock(lock)
    initial_seeding$amount <- as.integer(round(initial_seeding$amount))

    current_index <- 0
      
    # FIX ME : this file won't exist in general
    # TODO CHANGE TO FIRST DRAW OF SEIR CODE
    # lock <- flock::lock(paste('.lock',gsub('/','-',first_snpi_file),sep='/'))
    if((!file.exists(first_snpi_file)) | (!file.exists(first_spar_file))){
      print(sprintf("Creating parameters (%s) from Scratch",first_snpi_file))
      py$onerun_SEIR(opt$this_slot,py$s)
    }
    initial_snpi <- arrow::read_parquet(first_snpi_file)
    initial_spar <- arrow::read_parquet(first_spar_file)
    # flock::unlock(lock)

    if(!file.exists(first_hpar_file)){
      print(sprintf("Creating hospitalization parameters (%s) from config specified file %s",first_hpar_file,config$hospitalization$paths$geoid_params_file))
      file.copy(config$hospitalization$paths$geoid_params_file,first_hpar_file)
    }
    initial_hpar <- arrow::read_parquet(first_hpar_file)
    if(!file.exists(first_hosp_file)){
      print(sprintf("Creating hospitalization (%s) from Scratch",first_hosp_file))
      ## Generate files
      this_index <- opt$this_slot
      # lock <- flock::lock(paste(".lock",paste("SEIR",this_index,scenario,sep='.'),sep='/'))
      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("SEIR failed to run")
      }

      ## Run hospitalization
      err <- system(paste(
        opt$rpath,
        paste(opt$pipepath,"R","scripts","hosp_run.R", sep='/'),
        "-j",opt$jobs,
        "-c",opt$config,
        "-p",opt$pipepath,
        "-s",scenario,
        "-d",deathrate,
        "-n",1,
        "-i",this_index,
        "-g",first_hpar_file
      ))
      if(err != 0){
        stop("Hospitalization failed to run")
      }
    }

    initial_sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",first_hosp_file))(first_hosp_file) %>%
        filter(time >= min(obs$date), time <= max(obs$date)) %>%
        select(-date_inds)

      lhs <- unique(initial_sim_hosp[[obs_nodename]])
      rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]

      if(!dir.exists(config$filtering$likelihood_directory)){
        dir.create(config$filtering$likelihood_directory)
      }
      first_likelihood_file <- paste0(config$filtering$likelihood_directory,"/",sprintf("%09d",opt$this_slot),".chim.parquet")

#################################################PRIOR CODE END
##       if(!file.exists(first_likelihood_file)){
##           print(sprintf("Creating likelihood (%s) from Scratch",first_likelihood_file))




##       ##   initial_likelihood_data <- list()
##       ##   for(location in all_locations) {

##       ##     local_sim_hosp <- dplyr::filter(initial_sim_hosp, !!rlang::sym(obs_nodename) == location)
##       ##     initial_sim_stats <- inference::getStats(
##       ##       local_sim_hosp,
##       ##       "time",
##       ##       "sim_var",
##       ##       stat_list = config$filtering$statistics
##       ##     )


##       ##     # Get observation statistics
##       ##     log_likelihood <- list()
##       ##     for(var in names(data_stats[[location]])) {
##       ##       log_likelihood[[var]] <- inference::logLikStat(
##       ##         obs = data_stats[[location]][[var]]$data_var,
##       ##         sim = initial_sim_stats[[var]]$sim_var,
##       ##         dist = config$filtering$statistics[[var]]$likelihood$dist,
##       ##         param = config$filtering$statistics[[var]]$likelihood$param,
##       ##         add_one = config$filtering$statistics[[var]]$add_one
##       ##       )
##       ##     }
##       ##     # Compute log-likelihoods

##       ##     initial_likelihood_data[[location]] <- dplyr::tibble(
##       ##       ll = sum(unlist(log_likelihood)),
##       ##       filename = first_hosp_file,
##       ##       geoid = location
##       ##     )
##       ##     names(initial_likelihood_data)[names(initial_likelihood_data) == 'geoid'] <- obs_nodename
##       ##   }

##       ##   initial_likelihood_data <- initial_likelihood_data %>% do.call(what=rbind)

##       ##   ##Update initial liklihood data
##       ##   for (stat in names(hierarchical_stats)) {
##       ##       if (hierarchical_stats[[stat]]$module=="seir") {
##       ##           ll_adjs <- inference::calc_hierarchical_penalty(stat=hierarchical_stats[[stat]]$name,
##       ##                                                           infer_frame = initial_snpi,
##       ##                                                           geodata = geodata,
##       ##                                                           geo_group_column = hierarchical_stats[[stat]]$geo_group_col,
##       ##                                                           transform = hierarchical_stats[[stat]]$transform
##       ##                                                           )

##       ##       } else {
##       ##           stop("hospitalization TBI")
##       ##       }
            
##       ##       ##probably a more efficient what to do this, but unclear...
##       ##       initial_likelihood_data<- left_join(initial_likelihood_data, ll_adjs) %>%
##       ##           mutate(lls = ll + likadj) %>%
##       ##           select(-likadj)
            
##       ## }
## #################################################

##           ##function rplacement
##           initial_likelihood_data <- aggregate_and_calc_likelihoods(all_locations,
##                                                                     initial_sim_hosp,
##                                                                     obs_nodename,
##                                                                     config,
##                                                                     data_stats,
##                                                                     first_hosp_file,
##                                                                     hierarchical_stats) 

##           arrow::write_parquet(initial_likelihood_data,first_likelihood_file)
##       }


## ### BEFORE
## ###########################################
##       global_likelihood_data <- list()
##       for(location in all_locations) {

##         local_sim_hosp <- dplyr::filter(initial_sim_hosp, !!rlang::sym(obs_nodename) == location) %>%
##           dplyr::filter(time %in% unique(obs$date[obs$geoid == location]))
##         initial_sim_stats <- inference::getStats(
##           local_sim_hosp,
##           "time",
##           "sim_var",
##           stat_list = config$filtering$statistics
##         )


##           # Get observation statistics
##         log_likelihood <- list()
##         for(var in names(data_stats[[location]])) {
##           log_likelihood[[var]] <- inference::logLikStat(
##             obs = data_stats[[location]][[var]]$data_var,
##             sim = initial_sim_stats[[var]]$sim_var,
##             dist = config$filtering$statistics[[var]]$likelihood$dist,
##             param = config$filtering$statistics[[var]]$likelihood$param,
##             add_one = config$filtering$statistics[[var]]$add_one
##           )
##         }
##         # Compute log-likelihoods

##         global_likelihood_data[[location]] <- dplyr::tibble(
##           ll = sum(unlist(log_likelihood)),
##           filename = first_hosp_file,
##           geoid = location
##         )
##         names(global_likelihood_data)[names(global_likelihood_data) == 'geoid'] <- obs_nodename
##       }
##       rm(initial_sim_hosp) ##STILL EXISTS BELOW

##       global_likelihood_data <- global_likelihood_data %>% do.call(what=rbind)
###########################################PRIOR CODE END
      
      global_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(all_locations,
                                                                          initial_sim_hosp,
                                                                          obs_nodename,
                                                                          config,
                                                                          obs,
                                                                          data_stats,
                                                                          first_hosp_file,
                                                                          hierarchical_stats,
                                                                          geodata,
                                                                          initial_snpi)
      
      
      if(!file.exists(first_likelihood_file)){
          print(sprintf("Creating likelihood (%s) from Scratch",first_likelihood_file))

          arrow::write_parquet(global_likelihood_data,first_likelihood_file)
      }

      rm(initial_sim_hosp)
      # Compute total loglik for each sim
      initial_likelihood <- global_likelihood_data %>%
        summarise(ll = sum(ll, na.rm = T)) %>%
        mutate(pdeath = deathrate, scenario = scenario)
      current_likelihood <- initial_likelihood

### AFTER
      initial_likelihood_data <- arrow::read_parquet(first_likelihood_file)
      ## For logging
      current_index <- 0
      current_likelihood_data <- initial_likelihood_data

    for( index in seq_len(opt$simulations_per_slot)) {
      print(index)
      # Load sims -----------------------------------------------------------

      current_seeding <- inference::perturb_seeding(initial_seeding,config$seeding$perturbation_sd,c(lubridate::ymd(c(config$start_date,config$end_date))))
      current_snpi <- inference::perturb_snpi(initial_snpi, config$interventions$settings)
      current_spar <- initial_spar
      current_hpar <- inference::perturb_hpar(initial_hpar, config$hospitalization)
      this_index <- opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index
      write.csv(
        current_seeding,
        file = covidcommon::seeding_file_path(config,this_index)
      )
      arrow::write_parquet(
        current_snpi,
        covidcommon::snpi_file_path(config,this_index,scenario)
      )
      arrow::write_parquet(
        current_spar,
        covidcommon::spar_file_path(config,this_index,scenario)
      )
      arrow::write_parquet(
        current_hpar,
        covidcommon::hpar_file_path(config,this_index,scenario,deathrate)
      )


      ## Generate files
      # lock <- flock::lock(paste(".lock",paste("SEIR",this_index,scenario,sep='.'),sep='/'))
      err <- py$onerun_SEIR_loadID(this_index, py$s, this_index)
      err <- ifelse(err == 1,0,1)
      if(err != 0){
        stop("SEIR failed to run")
      }

      ## Run hospitalization
      err <- system(paste(
        opt$rpath,
        paste(opt$pipepath,"R","scripts","hosp_run.R", sep='/'),
        "-j",opt$jobs,
        "-c",opt$config,
        "-p",opt$pipepath,
        "-s",scenario,
        "-d",deathrate,
        "-n",1,
        "-i",this_index,
        "-g",covidcommon::hpar_file_path(config,this_index,scenario,deathrate)
      ))
      if(err != 0){
        stop("Hospitalization failed to run")
      }

      file <- covidcommon::hospitalization_file_path(config,this_index,scenario,deathrate)

      sim_hosp <- report.generation:::read_file_of_type(gsub(".*[.]","",file))(file) %>%
        filter(time <= max(obs$date)) %>%
        select(-date_inds)
      # flock::unlock(lock)

     

      lhs <- unique(sim_hosp[[obs_nodename]])
      rhs <- unique(names(data_stats))
      all_locations <- rhs[rhs %in% lhs]

#########################
      
      ## current_likelihood_data <- list()

    
      
      ## for(location in all_locations) {

      ##   local_sim_hosp <- dplyr::filter(sim_hosp, !!rlang::sym(obs_nodename) == location) %>%
      ##     dplyr::filter(time %in% unique(obs$date[obs$geoid == location]))
      ##   sim_stats <- inference::getStats(
      ##     local_sim_hosp,
      ##     "time",
      ##     "sim_var",
      ##     stat_list = config$filtering$statistics
      ##   )


      ##   # Get observation statistics
      ##   log_likelihood <- list()
      ##   for(var in names(data_stats[[location]])) {
      ##   # log_likelihood <- foreach (var = names(data_stats[[location]]), .combine = sum) %do% {
           
      ##     log_likelihood[[var]] <- inference::logLikStat(
      ##       obs = data_stats[[location]][[var]]$data_var,
      ##       sim = sim_stats[[var]]$sim_var,
      ##       dist = config$filtering$statistics[[var]]$likelihood$dist,
      ##       param = config$filtering$statistics[[var]]$likelihood$param,
      ##       add_one = config$filtering$statistics[[var]]$add_one
      ##     )
      ##   # }
      ##   }
      ##    # Compute log-likelihoods

      ##   current_likelihood_data[[location]] <- dplyr::tibble(
      ##     ll = sum(unlist(log_likelihood)),
      ##     filename = file,
      ##     geoid = location
      ##   )
      ##   names(current_likelihood_data)[names(current_likelihood_data) == 'geoid'] <- obs_nodename
      ## }
      ## ##rm(sim_hosp) #Still below

      ## current_likelihood_data <- current_likelihood_data %>% do.call(what=rbind)
#########################



      current_likelihood_data <- inference::aggregate_and_calc_loc_likelihoods(all_locations,
                                                                         sim_hosp,
                                                                         obs_nodename,
                                                                         config,
                                                                         obs,
                                                                         data_stats,
                                                                         file,
                                                                         hierarchical_stats,
                                                                         geodata,
                                                                         current_snpi)
                                      
      
      rm(sim_hosp)

      ## UNCOMMENT TO DEBUG
      ## print(global_likelihood_data)
      ## print(initial_likelihood_data)
      ## print(current_likelihood_data)     
      
      ## Compute total loglik for each sim
      current_likelihood <- current_likelihood_data %>%
        summarise(ll = sum(ll, na.rm = T)) %>%
        mutate(pdeath = deathrate, scenario = scenario)

      ## For logging
      print(paste("Current likelihood",initial_likelihood,"Proposed likelihood",current_likelihood))

      if(inference::iterateAccept(initial_likelihood, current_likelihood, 'll')){
        old_index <- opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + current_index
        current_index <- index
        initial_likelihood <- current_likelihood
        if(opt$clean){
          print("Removing old")
          file.remove(covidcommon::hospitalization_file_path(config,old_index,scenario,deathrate))
          file.remove(covidcommon::simulation_file_path(config,old_index,scenario))
          file.remove(covidcommon::snpi_file_path(config,old_index,scenario))
          file.remove(covidcommon::spar_file_path(config,old_index,scenario))
          file.remove(covidcommon::hpar_file_path(config,old_index,scenario,deathrate))
        }
      } else {
        old_index <- opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + index
        if(opt$clean){
          print("Removing new")
          file.remove(covidcommon::hospitalization_file_path(config,old_index,scenario,deathrate))
          file.remove(covidcommon::simulation_file_path(config,old_index,scenario))
          file.remove(covidcommon::snpi_file_path(config,old_index,scenario))
          file.remove(covidcommon::spar_file_path(config,old_index,scenario))
          file.remove(covidcommon::hpar_file_path(config,old_index,scenario,deathrate))
        }
      }

  
      
      seeding_npis_list <- inference::accept_reject_new_seeding_npis(
        seeding_orig = initial_seeding,
        seeding_prop = current_seeding,
        snpi_orig = initial_snpi,
        snpi_prop = current_snpi,
        hpar_orig = initial_hpar,
        hpar_prop = current_hpar,
        orig_lls = initial_likelihood_data,
        prop_lls = current_likelihood_data
      )
      initial_seeding <- seeding_npis_list$seeding
      initial_snpi <- seeding_npis_list$snpi
      initial_hpar <- seeding_npis_list$hpar
      initial_likelihood_data <- seeding_npis_list$ll
      likelihood_filename <- paste0(config$filtering$likelihood_directory,"/",sprintf("%09d",this_index),".chim.parquet")
      arrow::write_parquet(initial_likelihood_data, likelihood_filename)

      print(paste("Current index is ",current_index))
      # print(current_likelihood_data)
      # print(initial_likelihood_data)
      rm(current_snpi)
      rm(current_hpar)
      rm(current_seeding)
    }

    if(current_index != 0){
      current_file <- covidcommon::hospitalization_file_path(
        config,
        ## GLOBAL ACCEPT/REJECT
        opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + current_index,
        ## NO GLOBAL ACCEPT/REJECT
        # opt$simulations_per_slot * (opt$this_slot - 1) + opt$number_of_simulations + opt$simulations_per_slot,
        scenario,
        deathrate
      )
      target_file <- covidcommon::hospitalization_file_path(config,opt$this_slot,scenario,deathrate)
      target_dir <- gsub('/[^/]*$','',target_file)

      print(paste("Copying",current_file,"to",target_file))
      file.rename(from=current_file,to=target_file)
    }


    readr::write_csv(initial_seeding,first_seeding_file)
    arrow::write_parquet(initial_snpi,first_snpi_file)
    arrow::write_parquet(initial_hpar,first_hpar_file)
    arrow::write_parquet(initial_likelihood_data,first_likelihood_file)
  }
}
