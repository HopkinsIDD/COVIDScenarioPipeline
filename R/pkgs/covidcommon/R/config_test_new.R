# checkdate <- function(str) {
#   tryCatch(as.Date(str), 
#            warning = function(w) return(FALSE))
# }

validation_list <- list()

validation_list$name<-function(value,full_config,config_name,config_ignore_checks=character(0)){
  if(is.null(value)){
    print("Mention a name for the run")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$dt<- function(value,full_config,config_name){
  if(is.null(value)){
    print("Mention dt")
    return(FALSE)
  }
  if(!is.numeric(value)){
    print("Incorrect value for dt")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$smh_round <-function(value,full_config,config_name){
  if(is.null(value)){
    print("No SMH Round mentioned NA assigned as default") #ASSIGN DEFAULT
  }
  return(TRUE)
}

validation_list$start_date <-function(value,full_config,config_name){
  if(is.null(value)){
    print("No start date mentioned")
    return(FALSE)
  }
  # }else{
  #   if(checkdate(value)==FALSE){
  #     print("Enter a valid start date")
  #     return(FALSE)
  #   }
  # }
  return(TRUE)
}

validation_list$end_date <-function(value,full_config,config_name){
  if(is.null(value)){
    print("No start date mentioned")
    return(FALSE)
  }else{
    if(as.Date(value)<as.Date(full_config$start_date)){
      print("The start date is greater than end date")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$end_date_groundtruth <-function(value,full_config,config_name){
  if(is.null(value)){
    print("No end date groundtruth mentioned")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$nsimulations<- function(value,full_config,config_name){
  if(is.null(value)){
    print("Enter a value for number of simulations")
    return(FALSE)
  }
  if(value<1){
    print("number of simulations has to be greater than or equal to 1")
    return(FALSE)
  }
  return(TRUE)
}
#### SPATIAL SETUP PART
##Checking if the following values are present or not.
##If they do not have an assigned default value then the execution will be stopped.
##If they have a default then A statement will be printed and test will continue
## NO Default: Base Path, Modeled States, Year. Nodenames
## With Default: Geodata, Mobility, Popnodes, Statelevel

validation_list$spatial_setup <- list()
validation_list$spatial_setup$base_path <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No base path mentioned in the configuration file")
    return(FALSE)
  }
  if (!dir.exists(value)) {
    print(paste("The base path ", value, "could not be found."))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$modeled_states <- function(value, full_config,config_name) {
  if(length(value)==0){
    print("No state mentioned in the configuration file")
    return(FALSE)
  }else if (length(value)==1){
    if (list(NULL) %in%(value)) {
      print("No state mentioned in configuration file")
      return(FALSE)
    }
  }else{
    if (list(NULL) %in%(value)) {
      print("NULL is mentioned as a state in configuration file")
      return(FALSE)
    }
  }
  #Here additional checks can be added to see if the states mentioned in the config are valid states or not
  return(TRUE)
}

validation_list$spatial_setup$geodata <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No geodata path mentioned in the configuration file")
    return(FALSE)
  }else{
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned geodata file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$mobility <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No mobility path mentioned in the configuration file")
    return(FALSE)
  }else{
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned mobility file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$census_year <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No year mentioned")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$nodenames <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No Nodenames mentioned") #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$popnodes <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No Population Nodes mentioned") #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

#SINCE NOT NECESSARY written to remove warning
validation_list$spatial_setup$include_in_report <- function(value, full_config,config_name) {
  return(TRUE)
}

validation_list$spatial_setup$setup_name <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No runtype mentioned") #Should display a better error message than nodenames.
    return(FALSE)
  }
  if (length(strsplit(config_copy$spatial_setup$setup_name,split=" ")[[1]])!=1 | length(config_copy$spatial_setup$setup_name)!=1){
    print("Multiple setup_name(run_type) mentioned")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$state_level <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No specifications about state level runs mentioned mentioned")
    return(FALSE)
  }else{
    if(value!=TRUE & value!=FALSE){
      print("Wrong value mentioned should either be TRUE or FALSE")
      return(FALSE)
    }
  }
  return(TRUE)
}



###### IMPORTATION PART
# validation_list$importation <- list()
# validation_list$importation$census_api_key <- function(value, full_config) {
#   if (is.null(value) || len(value)==0) {
#     if(is.null(Sys.getenv("CENSUS_API_KEY")) || Sys.getenv("CENSUS_API_KEY")=="" ){
#       print("No census api key mentioned")
#       return(FALSE)
#     }
#   }
#   return(TRUE)
# }


########SEEDING PART ###########################
#No Default: perturbation_sd, date_sd, method
##With defaults: amount_sd, variant_file (default value: Should it be WILD?)
validation_list$seeding <- list()
validation_list$seeding$perturbation_sd <- function(value, full_config,config_name) {
  if(is.null(full_config$seeding$date_sd)){
    if (is.null(value)) {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$date_sd <- function(value, full_config,config_name) {
  if(is.null(full_config$seeding$pertubation_sd)){
    if (is.null(value)) {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }}
  return(TRUE)
}

validation_list$seeding$amount_sd <- function(value, full_config,config_name){
  if(is.null(value)){
    print("No standard deviation for amount mentioned") #Assign default
  }else{
    if(!(is.numeric(value))){
      print("Wrong format for pertubation amount mentioned")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$method <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No seeding method mentioned")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$variant_filename <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No variant file mentioned in the configuration file")
    return(FALSE)
  }else{
    if (!file.exists(value)) {
      print(paste("The mentioned variant file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$seeding_compartments<- function(value,full_config,config_name){
  if(is.null(value)){
    print("No seeding compartments mentioned")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$seeding_file_type <- function(value, full_config,config_name) {
  if (is.null(value)) {
    print("No specifications about seeding type mentioned")
    return(FALSE)
  }else{
    if(value!='seed' & value!='impa'){
      print("Wrong value mentioned should either be seed or impa")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$pop_seed_file<- function(value,full_config,config_name){
  if("compartments" %in% names(full_config$seir)){
    if(is.null(value)){
      print("Mention population seeding file for a compartment runs")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$folder_path<- function(value,full_config,config_name){
  if(full_config$seeding$method=="FolderDraw" | full_config$seeding$method=="SetInitialConditions" | full_config$seeding$method=="InitialConditionsFolderDraw"){
    if(is.null(value)){
      print("Importation folder path not mentioned")
      return(FALSE)
    }
    if(substr(value, nchar(value), nchar(value))!='/'){
      print("Incorrect folder path: folder path in seeding should have a / in the end")
      return(FALSE)
    }
    if(full_config$seeding$method=="FolderDraw" & !is.null(full_config$filtering)){
      if(is.null(full_config$seeding$lambda_file)){
        print("Lambda File not mentioned even if filtering section present")
        return(FALSE)
      }      
      if(!file.exists(full_config$seeding$lambda_file)){
        print("Lambda File does not exist even if filtering section present")
        return(FALSE)
      }
    }
    
  }
  return(TRUE)
}

validation_list$seeding$lambda_file<- function(value,full_config,config_name){
  if(full_config$seeding$method=="PoissonDistributed" | full_config$seeding$method=="NegativeBinomialDistributed"){
    if(is.null(value)){
      print("Lambda File not mentioned")
      return(FALSE)
    }      
    if(!file.exists(value)){
      print("Lambda File does not exist")
      return(FALSE)
    }
  }
  return(TRUE)  
}

######SEIR MODEL SPECIFICATIONS##################
##NO Defaults: Parameters: sigma,gamma,ROs, transitions, compartments
validation_list$seir$integration_method<-function(value,full_config,config_name){
  if(is.null(value)){
    print("No integration method mentioned default will be assigned")
  }
  return(TRUE)
}
validation_list$seir$parameters<-function(value,full_config,config_name){
  if (is.null(value)) {
    print("No parameters mentioned")
    return(FALSE)
  }
  for (param_name in names(value)){
    if(grepl("sigma",param_name[[1]])==TRUE){
      sig=TRUE
      break
    }
    else{
      sig=FALSE
    }
  }
  if(!sig){
    print("No sigma mentioned")
    return(FALSE)
  }
  if(is.null(value$gamma)){
    print("No gamma mentioned")
    return(FALSE)
  }
  
  #Checks if extra seir parameters are not mentioned depending upon the the 
  #variants present in seeding$seeeding_compartments 
  present_variants<-names(full_config$seeding$seeding_compartments)
  all_variants<-c('WILD','ALPHA','DELTA','OMICRON','VARIANT_X')
  absent_variants<-all_variants[!(all_variants %in% present_variants)]
  parameters<-names(value)
  flag=0 #Checks if a variant which is not present has parameters present
  for (variant in absent_variants){
    if(variant=='ALPHA'){
      if(any(grepl(paste('_',tolower(variant),sep=''),parameters))){
        print(paste('Variant', variant,' not mentioned in compartments but has seir transition value'))
        flag=1
      }      
    }
    else{
      if(any(grepl(tolower(variant),parameters)) |any(grepl(variant,parameters)) ){
        print(paste('Variant', variant,' not mentioned in compartments but has seir transition value'))
        flag=1
      }
    }
  }
  if(flag==1){
    return(FALSE)
  }
  return(TRUE)
  
}

validation_list$seir$compartments<- function(value,full_config,config_name){
  if(is.null(value)){
    print("No compartment information present")
    return(FALSE)
  }else{
    if(is.null(value$infection_stage)){
      print("No infection stage compartments present")
      return(FALSE)
    }
    if(is.null(value$vaccination_stage)){
      print("No vaccination stage compartments present")
      return(FALSE)
    }
    if(is.null(value$variant_type)){
      print("No variants mentioned")
      return(FALSE)
    }
    if(is.null(value$age_strata)){
      print("No age strata mentioned")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seir$transitions<-function(value,full_config,config_name){
  gempyor <- reticulate::import("gempyor")
  gempyor$config$set_file(config_name)
  compartments_obj <- gempyor$compartments$Compartments(gempyor$config["seir"])
  compartments <- compartments_obj$compartments
  transitions <- compartments_obj$transitions
  for (i in 1:length(transitions)){
    if(!(transitions$source[[i]][1] %in% compartments$infection_stage &transitions$destination[[i]][1] %in% compartments$infection_stage)){
      print("Incorrect Source or Destination compartments present")
      return(FALSE)
    }
    
    if(!(transitions$source[[i]][2] %in% compartments$vaccination_stage & transitions$destination[[i]][2] %in% compartments$vaccination_stage)){
      print("Incorrect vaccination mentioned in transitions")
      return(FALSE)
    }
    
    if(!(transitions$source[[i]][3] %in% compartments$variant_type & transitions$destination[[i]][3] %in% compartments$variant_type)){
      print("Incorrect variants mentioned in transitions")
      return(FALSE)
    }
    
    if(!(transitions$source[[i]][4] %in% compartments$age_strata & transitions$destination[[i]][4] %in% compartments$age_strata)){
      print("Incorrect Age Strata mentioned in transitions")
      return(FALSE)
    }
  }
  return(TRUE)
}
# validation_list$seir$parameters$R0s <- function(value, full_config) {
#   if (is.null(value)) {
#     print("No R0 mentioned")
#     return(FALSE)
#   }
#   return(TRUE)
# }

# validation_list$seir$parameters$parallel_structure<-list()
# 
# validation_list$seir$compartments <- function(value, full_config) {
#   if (is.null(value)) {
#     print("No Compartments mentioned")
#     return(FALSE)
#   }
#   return(TRUE)
# }
# 
# validation_list$seir$transitions <- function(value, full_config) {
#   if (is.null(value)) {
#     print("No transitions mentioned")
#     return(FALSE)
#   }
#   return(TRUE)
# }


###OUTCOMES##
validation_list$outcomes<- list()
validation_list$outcomes$method<- function(value,full_config,config_name){
  if(is.null(value)){
    print("No method mentioned in outcomes")
    return(FALSE)
  }
  if(value!="delayframe"){
    print("Only method delayframe is supported at the moment")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$param_from_file<- function(value, full_config,config_name){
  if(is.null(value)){
    print("There is no value mentioned for outcomes param_from_file")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$param_place_file<- function(value, full_config,config_name){
  if(full_config$outcomes$param_from_file){
    if(is.null(value)){
      print("Mention Output Param File")
      return(FALSE)
    }
    if(!file.exists(value)){
      print("Output Param File does not exist")
    }
  }
  return(TRUE)
}

validation_list$outcomes$scenarios<- function(value, full_config,config_name){
  if(is.null(value)){
    print("So outcome scenarios mentioned default assigned") #Assign Default
  }
  return(TRUE)
}

validation_list$outcomes$settings<-function(value, full_config,config_name){
  if(is.null(value)){
    print("No outcome settings mentioned default assigned") #Assign Default
  }
  for (scenario in full_config$outcomes$scenarios){
    if(!(scenario %in% names(value))){
      print(paste0("No details mentioned about scenario ",scenario," in outcomes"))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$outcomes$interventions<-function(value, full_config,config_name){
  if(is.null(value)){
    print("No outcome interventions mentioned")
    return(FALSE)
  }
  return(TRUE)
}

###FILTERING
validation_list$filtering<-list()
validation_list$filtering$simulations_per_slot<-function(value,full_config,config_name){
  if(is.null(value)){
    print("simulations_per_slot undefined in config, can't autodetect parameters")
    return(FALSE)
  }
  if(!is.numeric(value)){
    print("Incorrect value mentioned for simulations_per_slot should be a numeric value")
    return(FALSE)
  }
  if(value<1){
    print("simulations_per_slot should be 1 or greater")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$do_filtering<- function(value,full_config,config_name){
  if(is.null(value)){
    print("Mention do_filtering")
    return(FALSE)
  }
  if(value!=TRUE & value!=FALSE){
    print("Incorrect value mentioned: should be TRUE or FALSE")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$data_path<-function(value,full_config,config_name){
  if(is.null(value)){
    print("Mention correct data path for filtering")
    return(FALSE)
  }
  if(!file.exists(value)){
    print("Mentioned data path does not exist for filtering")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$gt_source<- function(value,config,config_name){
  if(is.null(value)){
    print("No source of ground truth source mentioned default assigned ")#Assign default
  }
  return(TRUE)
}

validation_list$filtering$statistics<- function(value,config,config_name){
  if(is.null(value)){
    print("Mention statistics for filtering")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions<-list()
validation_list$interventions$scenarios<-function(value,config,config_name){
  if(is.null(value)){
    print("Mention intervention scenarios")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings<-function(value,config,config_name){
  if(is.null(value)){
    print("Mention details about intervention scenarios in intervention settings")
    return(FALSE)
  }
  return(TRUE)
}

config_ignore_checks=character(0)

#' @name check_config
#' @title check_config
#' @description Check the config to make sure the fields are valid
#' @param config The config to check
#' @param validation_list A named list of checks, where each element is either a named list of checks, or a function which takes the field value and the config.  The function should return TRUE if the config is valid, or FALSE and emit a warning if the config is invalid.
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @param config_ignore_checks Fields which shouldn't be checked
#' @export
check_config <- function(config, checks = validation_list, original_config = config,
                         config_name,name_prefix = NULL, no_check_fields = config_ignore_checks, index = NULL) {
  config_is_valid <- TRUE
  subconfig_valid <- TRUE
  for (field_name in names(checks)) {
    if (field_name == "::") {
      for (new_index in seq_len(length(config))) {
        if (class(config[[field_name]]) %in% c("NULL", "list")) {
          subconfig_valid <- check_config(
            config[[new_index]], checks[[field_name]],
            original_config, config_name,paste0(name_prefix, ifelse(is.null(name_prefix),
                                                                    "", "::"
            ), c(index, new_index)),
            index = c(index, new_index)
          )
        } else {
          subconfig_valid <- checks[[field_name]](config[[new_index]], original_config,config_name, c(index, new_index))
        }
        config_is_valid <- config_is_valid && subconfig_valid
      }
      next
    }
    if (class(checks[[field_name]]) == "list") {
      if (class(config[[field_name]]) %in% c("NULL", "list")) {
        subconfig_valid <- check_config(
          config[[field_name]], checks[[field_name]],
          original_config, config_name,paste0(name_prefix, ifelse(is.null(name_prefix),
                                                                  "", "::"
          ), field_name)
        )
        config_is_valid <- config_is_valid && subconfig_valid
      } else {
        warning(paste(
          "config field", paste0(name_prefix, ifelse(is.null(name_prefix),
                                                     "", "::"
          ), field_name), "should be a list, but was of type",
          class(config[[field_name]]), "with value", config[[field_name]]
        ))
        config_is_valid <- FALSE
      }
    } else {
      field_valid <- checks[[field_name]](config[[field_name]], original_config, config_name)###
      config_is_valid <- config_is_valid && field_valid
    }
  }
  if ((!all(names(config) %in% names(checks))) && (!all(names(checks) == "::"))) {
    missing_fields <- names(config)[!(names(config) %in% names(checks))]
    missing_fields <- missing_fields[!(missing_fields %in% no_check_fields)]
    for (field in missing_fields) {
      warning(paste(paste0(name_prefix, ifelse(is.null(name_prefix),
                                               "", "::"
      ), field), "is not a known config field, and is unused"))
      config_is_valid <- FALSE
    }
  }
  
  return(config_is_valid)
}


