#########################################################################
#########################################################################
#########################################################################
#CHECKING VALUES WITH NO DEFAULTS

is_config_valid <- function(config, validation_list, full_config = config) {
  for (config_name in names(validation_list)) {
    if (class(validation_list)[config_name] == 'list') {
      is_config_valid(config[[config_name]], validation_list[[config_name]], full_config)
    } else {
      validation_list[[config_name]](config[[config_name]], full_config)
    }
  }
  invisible(NULL)
}

validation_list <- list()


#### SPATIAL SETUP PART
##Checking if the following values are present or not.
##If they do not have an assigned default value then the execution will be stopped.
##If they have a default then A statement will be printed and test will continue
## NO Default: Base Path, Modeled States, Year. Nodenames
## With Default: Geodata, Mobility, Popnodes, Statelevel

validation_list$spatial_setup <- list()
validation_list$spatial_setup$base_path <- function(value, full_config) {
  if (is.null(value)) {
    stop("No base path mentioned in the configuration file")
  }
  if (!dir.exists(value)) {
    stop(paste("The base path ", value, "could not be found."))
  }
  invisible(NULL)
}

validation_list$spatial_setup$modeled_states <- function(value, full_config) {
  if (is.null(value)) {
    stop("No States mentioned in the configuration file")
  }
#Here additional checks can be added to see if the states mentioned in the config are valid states or not
  invisible(NULL)
}

validation_list$spatial_setup$census_year <- function(value, full_config) {
  if (is.null(value)) {
    stop("No year mentioned")
  }
  invisible(NULL)
}

validation_list$spatial_setup$nodenames <- function(value, full_config) {
  if (is.null(value)) {
    stop("No Nodenames mentioned") #Should display a better error message than nodenames.
  }
  invisible(NULL)
}

validation_list$spatial_setup$geodata <- function(value, full_config) {
  if (is.null(value)) {
    print("No geodata file mentioned in the configuration file default value assigned")
    value<-'geodata.csv'
  }
  invisible(NULL)
}
  
validation_list$spatial_setup$mobility <- function(value, full_config) {
  if (is.null(value)) {
    print("No mobility file mentioned in the configuration file default value assigned")
    value<-'mobility.csv'
  }
  invisible(NULL)
}

validation_list$spatial_setup$popnodes <- function(value, full_config) {
  if (is.null(value)) {
    print("No popnodes mentioned in the configuration file default value assigned")
  }
  invisible(NULL)
}

validation_list$spatial_setup$state_level <- function(value, full_config) {
  if (is.null(value)) {
    print("No state_level mentioned in the configuration file default value assigned")
  }
  invisible(NULL)
}


###### IMPORTATION PART
validation_list$importation <- list()
validation_list$importation$census_api_key <- function(value, full_config) {
  if (is.null(value) || len(value)=0) {
    if(is.null(Sys.getenv("CENSUS_API_KEY")) || Sys.getenv("CENSUS_API_KEY")=="" ){
      stop("No census api key mentioned")
    }
  }
  invisible(NULL)
}


########SEEDING PART ###########################
#No Default: perturbation_sd, date_sd, method
##With defaults: amount_sd, variant_file (Not sure about the default value: Should it be WILD?)
validation_list$seeding <- list()
validation_list$seeding$perturbation_sd <- function(value, full_config) {
  if (is.null(value)) {
    stop("No perturbation_sd mentioned")
  }
  invisible(NULL)
}

validation_list$seeding$date_sd <- function(value, full_config) {
  if (is.null(value)) {
    stop("No perturbation_sd mentioned")
  }
  invisible(NULL)
}

validation_list$seeding$method <- function(value, full_config) {
  if (is.null(value)) {
    stop("No seeding method mentioned")
  }
  invisible(NULL)
}

validation_list$seeding$amount_sd <- function(value, full_config) {
  if (is.null(value)) {
    print("No amount_sd mentioned in the configuration file default value assigned")
  }
  invisible(NULL)
}

validation_list$seeding$variant_filename <- function(value, full_config) {
  if (is.null(value)) {
    print("No variant_file mentioned in the configuration file default value assigned")
  }
  invisible(NULL)
}

######SEIR MODEL SPECIFICATIONS##################
##NO Defaults: Parameters: sigma,gamma,ROs, transitions, compartments

validation_list$seir <- list()


validation_list$seir$parameters<-list()

validation_list$seir$parameters$sigma <- function(value, full_config) {
  if (is.null(value)) {
    stop("No sigma mentioned")
  }
  invisible(NULL)
}

validation_list$seir$parameters$gamma <- function(value, full_config) {
  if (is.null(value)) {
    stop("No gamma mentioned")
  }
  invisible(NULL)
}

validation_list$seir$parameters$R0s <- function(value, full_config) {
  if (is.null(value)) {
    stop("No R0 mentioned")
  }
  invisible(NULL)
}

validation_list$seir$parameters$R0s <- function(value, full_config) {
  if (is.null(value)) {
    stop("No R0 mentioned")
  }
  invisible(NULL)
}

validation_list$seir$parameters$parallel_structure<-list()

validation_list$seir$parameters$parallel_structure$compartments <- function(value, full_config) {
  if (is.null(value)) {
    stop("No Compartments mentioned")
  }
  invisible(NULL)
}

validation_list$seir$parameters$parallel_structure$transitions <- function(value, full_config) {
  if (is.null(value)) {
    stop("No transitions mentioned")
  }
  invisible(NULL)
}


#########################################################################
#########################################################################
#########################################################################
#CHECKING VALUES WITH DEFAULTS


add_default_values <- function(config, default_list, full_config = config) {
  for (config_name in names(default_list)) {
    if (class(default_list)[config_name] == 'list') {
      config[[config_name]] <- add_default_values(config[[config_name]], default_list[[config_name]], full_config)
    } else if (is.null(config[[config_name]])) {
      config[[config_name]] <- default_list[[config_name]](default_list[[config_name]],full_config)
    }
  }
  return(config)
}


default_list<-list()

################ DEFAULT VALUE FOR SPATIAL SETUP ##################
default_list$spatial_setup<-list()

default_list$spatial_setup$geodata <- function(value, full_config) {
  if (is.null(value)) {
    value<-'geodata.csv'
  }
  if (!file.exists(paste(full_config$spatial_setup$geodata, value, sep = '/'))) {
    stop(paste("The geodata file", value, "could not be found.  Looked in", paste(full_config$spatial_setup$base_path, value, sep = '/')))
  }
  return(value)
}



default_list$spatial_setup$mobility <- function(value, full_config) {
  if (is.null(value)) {
    value<-'mobility.csv'
  }
  if (!file.exists(paste(full_config$spatial_setup$mobility, value, sep = '/'))) {
    stop(paste("The mobility file", value, "could not be found.  Looked in", paste(full_config$spatial_setup$base_path, value, sep = '/')))
  }
  return(value)
}

validation_list$spatial_setup$popnodes <- function(value, full_config) {
  if (is.null(value)) {
    value<-'population'
  }
  return(value)
}

validation_list$spatial_setup$state_level <- function(value, full_config) {
  if (is.null(value)) {
    value<-FALSE
  }
  return(value)
}


################ DEFAULT VALUE FOR SEEDING ##################
default_list$seeding <- list()

default_list$seeding$amount_sd <- function(value, full_config) {
  if (is.null(value)) {
    value<-1
  }
  return(value)
}

validation_list$seeding$variant_filename <- function(value, full_config) {
  if (is.null(value)) {
    value<-'WILD'
  }
  return(value)
}