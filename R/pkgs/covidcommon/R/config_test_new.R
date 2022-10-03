validation_list <- list()

validation_list$name<-function(value,config_name){
  if (mode(value) != "character") {
    print(paste("name should be a single string, but has mode", mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$dt<- function(value,config_name){
  if(mode(value) != "numeric"){
    print(paste("Incorrect value for dt - it should be a numeric ",value," is mentioned"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$smh_round <-function(value,config_name){
  if(mode(value)!="character"){
    print(paste("SMH Round should be a string but is of mode",mode(value))) #ASSIGN DEFAULT
  }
  return(TRUE)
}

validation_list$start_date <-function(value,config_name){
  if(is.na(as.Date(value,optional=TRUE))){
    print(paste("Enter a valid start date ",value," mentioned"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$end_date <-function(value,config_name){
  if(is.na(as.Date(value,optional=TRUE))){
    print(paste("Enter a valid end date ",value," mentioned"))
    return(FALSE)
  }
  full_config = yaml::read_yaml(config_name)
  if(as.Date(value)<as.Date(full_config$start_date)){
    print(paste("The start date",full_config$start_date,"is greater than end date",value))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$start_date_groundtruth <-function(value,config_name){
  if(mode(value) != "character"){
    print("No start date groundtruth mentioned default will be assigned")
  }
  else{
    if(is.na(as.Date(value,optional=TRUE))){
      print(paste("Enter a valid start groundtruth date format ",value," mentioned"))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$end_date_groundtruth <-function(value,config_name){
  if(mode(value)!="character"){
    print("No end date groundtruth mentioned - no limiting ground truth date")
  }
  else{
    if(is.na(as.Date(value,optional=TRUE))){
      print(paste("Enter a valid end groundtruth date format ",value," mentioned"))
      return(FALSE)
    }
  return(TRUE)
  }
}

validation_list$nsimulations<- function(value,config_name){
  if(mode(value) != "numeric"){
    print(paste("number of simulations should be numeric, but has mode ",mode(value)))
    return(FALSE)
  }
  if(value<1){
    print(paste("number of simulations has to be greater than or equal to 1", value,"mentioned"))
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
validation_list$spatial_setup$base_path <- function(value,config_name) {
  if (mode(value) != 'character') {
    print(paste("base path should be a string, but has mode ",mode(value))) 
    return(FALSE)
  }
  if (!dir.exists(value)) {
    print(paste("The base path directory", value, "could not be found."))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$modeled_states <- function(value,config_name) {
  if(mode(value)!="character"){
    print(paste("modeled states should be strings, but has mode",mode(value)))
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
  states<-c("AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","GA","GU",
            "HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN",
            "MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH",
            "OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VA","VI","VT",
            "WA","WI","WV","WY")
  if(!all(value %in% states)){
    print("Invalid state mentioned in modeled states")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$geodata <- function(value, config_name) {
  if (mode(value) != 'character') {
    print(paste("geodata path should be a string, but has mode",mode(value)))
    return(FALSE)
  }else{
    full_config<-yaml::read_yaml(config_name)
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned geodata file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$mobility <- function(value, config_name) {
  if (mode(value) != 'character') {
    print(paste("mobility data path should be a string, but has mode",mode(value)))
    return(FALSE)
  }else{
    full_config<-yaml::read_yaml(config_name)
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned mobility file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$census_year <- function(value, config_name) {
  if (mode(value) != 'numeric') {
    print(paste("year should be a number, but has mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$nodenames <- function(value, config_name) {
  if (mode(value) != 'character') {
    print(paste("nodenames should be a string but has mode",mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$popnodes <- function(value, config_name) {
  if (mode(value) != 'character') {
    print(paste("popnodes should be a string but has mode",mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

#SINCE NOT NECESSARY written to remove warning
validation_list$spatial_setup$include_in_report <- function(value, config_name) {
  return(TRUE)
}

validation_list$spatial_setup$setup_name <- function(value, full_config,config_name) {
  if (mode(value) != 'character') {
    print(paste("setup_name should be a single string, but has mode", mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  if (length(value) != 1) {
    print(paste("setup_name should be a single string, but has length", length(value)))
    return(FALSE)
  }
  if (!(value %in% c("SMH", "FCH"))) {
    print(paste("setup_name should be either 'SMH' or 'FCH', but is ", value))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$state_level <- function(value, config_name) {
  if (mode(value) != "logical") {
    print(paste("state level should be logical but has mode",mode(value)))#ASSIGN DEFAULT
  }
  return(TRUE)
}


########SEEDING PART ###########################
#No Default: perturbation_sd, date_sd, method
##With defaults: amount_sd, variant_file (default value: Should it be WILD?)
validation_list$seeding <- list()
validation_list$seeding$perturbation_sd <- function(value, config_name) {
  full_config = yaml::read_yaml(config_name)
  if(mode(full_config$seeding$date_sd)!="numeric"){
    if (mode(value)!="numeric") {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$date_sd <- function(value, config_name) {
  full_config = yaml::read_yaml(config_name)
  if(mode(full_config$seeding$pertubation_sd)!="numeric"){
    if (mode(value)!="numeric") {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$amount_sd <- function(value, config_name){
  if(mode(value)!="numeric"){
    print(paste("Standard deviation should be a numeric but is of mode",mode(value))) #Assign default
  }
  else if (mode(value)!="numeric"){
    print(paste("The amount_sd should be a numeric value but is of mode",mode(value)))
    return (FALSE)
  }
  return(TRUE)
}

validation_list$seeding$method <- function(value, config_name) {
  if (mode(value)!="character") {
    print(paste("The seeding method should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$variant_filename <- function(value, config_name) {
  if (mode(value)!="character") {
    print(paste("The variant file name should be a string but is of mode",mode(value)))
    return(FALSE)
  }else{
    if (!file.exists(value)) {
      print(paste("The mentioned variant file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$seeding_compartments<- function(value,config_name){
  if(mode(value)!="list"){
    print(paste("Seeding compartments should be a list but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$seeding_file_type <- function(value, config_name) {
  if (mode(value)!="character") {
    print(paste("Seeding file type should be a string but is of mode",mode(value)))
    return(FALSE)
  }else{
    if(value!='seed' & value!='impa'){
      print("Wrong value mentioned should either be seed or impa")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$pop_seed_file<- function(value,config_name){
  full_config = yaml::read_yaml(config_name)
  if("compartments" %in% names(full_config$seir)){
    if(mode(value)!="character"){
      print(paste("Compartments runs should have a pop_seed_file as a string but is of mode",mode(value)))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$folder_path<- function(value,config_name){
  full_config = yaml::read_yaml(config_name)
  if(full_config$seeding$method %in% c("FolderDraw","SetInitialConditions","InitialConditionsFolderDraw")){
    if(mode(value)!="character"){
      print(paste("Folder Path should be a string but is of mode",mode(value)))
      return(FALSE)
    }
    if(substr(value, nchar(value), nchar(value))!='/'){
      print("Incorrect folder path: folder path in seeding should have a / in the end")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$lambda_file<- function(value,config_name){
    if(mode(value)!="character"){
      print(paste("Lambda file should be a string but is of mode",mode(value)))
      return(FALSE)
    }
    full_config<-yaml::read_yaml(config_name)
    if ("filtering" %in% names(full_config)){
      if(!dir.exists(dirname(value))){
        print("The folder where lambda file will be created does not exist")
        return(FALSE)
      }
    }
    else{
      if (full_config$seeding$method %in% c("PoissonDistributed", "NegativeBinomialDistributed")){
        if(!dir.exists(dirname(value))){
          print("The folder where lambda file will be created does not exist")
          return(FALSE)
        }
        if(!file.exists(value)){
          print("Lambda file does not exist")
          return(FALSE)
        }
      }
    }
  return(TRUE)  
}

######SEIR MODEL SPECIFICATIONS##################
##NO Defaults: Parameters: sigma,gamma,ROs, transitions, compartments
validation_list$seir$integration_method<-function(value,config_name){
  if(mode(value)!="character"){
    print(paste("Integration method should be a string but is of mode",mode(value),"default will be assigned"))
  }
  return(TRUE)
}
validation_list$seir$parameters<-function(value,config_name){
  if (mode(value)!="list") {
    print(paste("Parameters should be a list but is of mode",mode(value)))
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
  full_config = yaml::read_yaml(config_name)
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

validation_list$seir$compartments<- function(value,config_name){
  if(mode(value)!="list"){
    print(paste("The seir compartments should be a list but is of type",mode(value)))
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

validation_list$seir$transitions<-function(value,config_name){
  if(mode(value)!="list"){
    print(paste("The seir transitions should be a list but is of mode",mode(value)))
    return (FALSE)
  }
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

###OUTCOMES##
validation_list$outcomes<- list()
validation_list$outcomes$method<- function(value,config_name){
  if(mode(value)!="character"){
    print(paste("Outcome method should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(value!="delayframe"){
    print("Only method delayframe is supported at the moment")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$param_from_file<- function(value, config_name){
  if(mode(value)!="logical"){
    print(paste("Outcomes param_from_file should be logical but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$param_place_file<- function(value, config_name){
  full_config = yaml::read_yaml(config_name)
  if(full_config$outcomes$param_from_file){
    if(mode(value)!="character"){
      print(paste("Outcome param_place_file should be a character but is of mode",mode(value)))
      return(FALSE)
    }
    if(!file.exists(value)){
      print("Output Param File does not exist")
    }
  }
  return(TRUE)
}

validation_list$outcomes$scenarios<- function(value, config_name){
  if(mode(value)!="character"){
    print(paste("Outcomes scenarios should be a string but is of mode",mode(value),"default will be assigned")) #Assign Default
  }
  return(TRUE)
}

validation_list$outcomes$settings<-function(value, config_name){
  if(mode(value)!="list"){
    print(print("Outcome settings should be a list but is of mode",mode(value),"default will be assigned")) #Assign Default
  }
  full_config = yaml::read_yaml(config_name)
  for (scenario in full_config$outcomes$scenarios){
    if(!(scenario %in% names(value))){
      print(paste0("No details mentioned about scenario ",scenario," in outcomes"))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$outcomes$interventions<-function(value, config_name){
  if(mode(value)!="list"){
    print(paste("Outcome interventions should be a list but is of type",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

###FILTERING
validation_list$filtering<-list()
validation_list$filtering$simulations_per_slot<-function(value,config_name){
  if(mode(value)!="numeric"){
    print(paste("simulations_per_slot should be a numeric but is of mode",mode(value)))
    return(FALSE)
  }
  if(value<1){
    print("simulations_per_slot should be 1 or greater")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$do_filtering<- function(value,config_name){
  if(mode(value)!="logical"){
    print(paste("do_filtering should be logical but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$data_path<-function(value,config_name){
  if(mode(value)!="character"){
    print(paste("Data path for filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(!dir.exists(dirname(value))){
    print("Mentioned data path does not exist for filtering")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$gt_source<- function(value,config_name){
  if(mode(value)!="character"){
    print(paste("gt_source in filtering should be a string but is of mode",mode(value),"default will be assigned"))#Assign default
  }
  return(TRUE)
}

validation_list$filtering$statistics<- function(value,config_name){
  if(mode(value)!="list"){
    print(paste("filtering statistics should be a list but is of value",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$priors<- function(value,config_name,index){
  if(mode(value)=="NULL"){
    return(TRUE) #SINCE NOT COMPULSORY TO BE MENTIONED
  }
  for (prior in value){
    if(!(prior$likelihood$dist %in% c('normal','logit'))){
      print("The distribuition of priors can either be normal or logit")
    }
  }
  return(TRUE) 
}

validation_list$interventions<-list()
validation_list$interventions$scenarios<-function(value,config_name){
  if(mode(value)!="character"){
    print(paste("Interventions scenarios should be a list but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings<-function(value,config_name){
  if(mode(value)!="list"){
    print(paste("The intervention settings should be a list but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

config_ignore_checks=character(0)

#' PART OF THE COVIDCOMMON PACKAGE TO CHECK FOR VALIDITY OF CONFIGS. 
#' THIS NEEDS TO BE RUN BY THE MEANS OF A FUNCTION CALL VIA THE DATA FOLDER WHICH HAS THE CONFIG FILE AND THE RELEVANT DATA PATHS
#' FUNCTION CALL SHOULD LOOK LIKE THE FOLLOWING:
#' check_config(config_name=NAME_OF_CONFIG_AS_STRING)
#' 
#' @return A boolean value depending on whether the config is valid(TRUE) or not(FALSE) 
#' @name check_config
#' @title check_config
#' @description Check the config to make sure the fields are valid
#' @param config The config to check
#' @param validation_list A named list of checks, where each element is either a named list of checks, or a function which takes the field value and the config.  The function should return TRUE if the config is valid, or FALSE and emit a warning if the config is invalid.
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @param config_ignore_checks Fields which shouldn't be checked
#' @export
check_config <- function(config_name,config= yaml::read_yaml(config_name), 
                         checks = validation_list,
                         name_prefix = NULL, no_check_fields = config_ignore_checks, index = NULL) {
  config_is_valid <- TRUE
  subconfig_valid <- TRUE
  for (field_name in names(checks)) {
    if (field_name == "::") {
      for (new_index in seq_len(length(config))) {
        if (class(config[[field_name]]) %in% c("NULL", "list")) {
          subconfig_valid <- check_config(
            config_name, config[[new_index]], 
            checks[[field_name]],
            paste0(name_prefix, ifelse(is.null(name_prefix),
                                                                    "", "::"
            ), c(index, new_index)),
            index = c(index, new_index)
          )
        } else {
          subconfig_valid <- checks[[field_name]](config[[new_index]],config_name, c(index, new_index))
        }
        config_is_valid <- config_is_valid && subconfig_valid
      }
      next
    }
    if (class(checks[[field_name]]) == "list") {
      if (class(config[[field_name]]) %in% c("NULL", "list")) {
        subconfig_valid <- check_config(
          config_name, config[[field_name]], 
          checks[[field_name]],
          paste0(name_prefix, ifelse(is.null(name_prefix),
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
      field_valid <- checks[[field_name]](config[[field_name]],  config_name)###
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