distribution_validation_list<-list()

distribution_validation_list$distribution<-function(value,full_config,index){
  print("D")
  print(value)
  if(mode(value)!="character"){
    print(paste("The distribution of the value should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The distribution of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$mean<-function(value,full_config,index){
  print("M")
  print(value)
  if(full_config$distribution %in% c('fixed','uniform')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The mean of the value should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The mean of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$sd<-function(value,full_config,index){
  print("SD")
  print(value)
  if(full_config$distribution %in% c('fixed','uniform')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The sd of the value should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The sd of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$a<-function(value,full_config,index){
  print("A")
  print(value)
  if(full_config$distribution %in% c('fixed','uniform')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The a of the value should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The a of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$b<-function(value,full_config,index){
  print("B")
  print(value)
  if(full_config$distribution %in% c('fixed','uniform')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The b of the value should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The b of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$value<-function(value,full_config,index){
  print("V")
  print(value)
  if(full_config$distribution %in% c('truncnorm','uniform')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The value of the value in should be a numeric but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The mean of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$low<-function(value,full_config,index){
  print("L")
  print(value)
  if(full_config$distribution %in% c('truncnorm','fixed')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The value of the value in should be a numeric but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The mean of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

distribution_validation_list$high<-function(value,full_config,index){
  print("H")
  print(value)
  if(full_config$distribution %in% c('truncnorm','fixed')){
    return(TRUE)
  }
  if(mode(value)!="numeric"){
    print(paste("The value of the value in should be a numeric but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(mode)!=1){
    print(paste("The mean of the value should be a of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}




validation_list <- list()

validation_list$name<-function(value,full_config,index){
  if (mode(value) != "character") {
    print(paste("name should be a single string, but has mode", mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$dt<- function(value,full_config, index){
  if(mode(value) != "numeric"){
    print(paste("Incorrect value for dt - it should be a numeric ",value," is mentioned"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$smh_round <-function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("SMH Round should be a string but is of mode",mode(value))) #ASSIGN DEFAULT
    return(FALSE)
  }
  return(TRUE)
}

validation_list$start_date <-function(value,full_config, index){
  if(is.na(as.Date(value,optional=TRUE))){
    print(paste("Enter a valid start date ",value," mentioned"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$end_date <-function(value,full_config, index){
  if(is.na(as.Date(value,optional=TRUE))){
    print(paste("Enter a valid end date ",value," mentioned"))
    return(FALSE)
  }
  # full_config = yaml::read_yaml(config_name)
  if(as.Date(value)<as.Date(full_config$start_date)){
    print(paste("The start date",full_config$start_date,"is greater than end date",value))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$start_date_groundtruth <-function(value,full_config, index){
  if(mode(value) != "character"){
    print(paste("start date groundtruth should be a date but is of mode",mode(value)))
    return(FALSE)
  }
  else{
    if(is.na(as.Date(value,optional=TRUE))){
      print(paste("Enter a valid start groundtruth date format ",value," mentioned"))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$end_date_groundtruth <-function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("end date should be a date but is of mode",mode(value)))
    return(FALSE)
  }
  else{
    if(is.na(as.Date(value,optional=TRUE))){
      print(paste("Enter a valid end groundtruth date format ",value," mentioned"))
      return(FALSE)
    }
  return(TRUE)
  }
}

validation_list$nsimulations<- function(value,full_config, index){
  if(mode(value) != "numeric"){
    print(paste("number of simulations should be numeric, but has mode ",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print("The number of simulations should be a single value")
    return(FALSE)
  }
  if(value %%1 !=0){
    print("The number of simulations should be an integer value")
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
validation_list$spatial_setup$base_path <- function(value,full_config, index) {
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

validation_list$spatial_setup$modeled_states <- function(value,full_config, index) {
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

validation_list$spatial_setup$geodata <- function(value, full_config, index) {
  if (mode(value) != 'character') {
    print(paste("geodata path should be a string, but has mode",mode(value)))
    return(FALSE)
  }else{
    #full_config<-yaml::read_yaml(config_name)
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned geodata file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$mobility <- function(value, full_config,index) {
  if (mode(value) != 'character') {
    print(paste("mobility data path should be a string, but has mode",mode(value)))
    return(FALSE)
  }else{
    #full_config<-yaml::read_yaml(full_config)
    path=paste(full_config$spatial_setup$base_path,'/',value,sep='')
    if (!file.exists(path)) {
      print(paste("The mentioned mobility file :", value, "could not be found."))
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$spatial_setup$census_year <- function(value, full_config,index) {
  if (mode(value) != 'numeric') {
    print(paste("year should be a number, but has mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$nodenames <- function(value, full_config,index) {
  if (mode(value) != 'character') {
    print(paste("nodenames should be a string but has mode",mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

validation_list$spatial_setup$popnodes <- function(value, full_config,index) {
  if (mode(value) != 'character') {
    print(paste("popnodes should be a string but has mode",mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  return(TRUE)
}

#SINCE NOT NECESSARY written to remove warning
# validation_list$spatial_setup$include_in_report <- function(value, full_config,index) {
#   return(TRUE)
#}

validation_list$spatial_setup$setup_name <- function(value, full_config,index) {
  if (mode(value) != 'character') {
    print(paste("setup_name should be a single string, but has mode", mode(value))) #Should display a better error message than nodenames.
    return(FALSE)
  }
  if (length(value) != 1) {
    print(paste("setup_name should be a single string, but has length", length(value)))
    return(FALSE)
  }
  # if (!(value %in% c("SMH", "FCH"))) {
  #   print(paste("setup_name should be either 'SMH' or 'FCH', but is ", value))
  #   return(FALSE)
  # }
  return(TRUE)
}

validation_list$spatial_setup$state_level <- function(value, full_config,index) {
  if (mode(value) != "logical") {
    print(paste("state level should be logical but has mode",mode(value)))#ASSIGN DEFAULT
    return(FALSE)
  }
  return(TRUE)
}


########SEEDING PART ###########################
#No Default: perturbation_sd, date_sd, method
##With defaults: amount_sd, variant_file (default value: Should it be WILD?)
validation_list$seeding <- list()
validation_list$seeding$perturbation_sd <- function(value, full_config,index) {
  #full_config = yaml::read_yaml(config_name)
#  if(mode(full_config$seeding$date_sd)!="numeric"){
    if (mode(value)!="numeric") {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }
#  }
  return(TRUE)
}

#CHANGED DEPENDENCY OF PERTUBATION_SD and DATE_SD Assign defaults checking on conditions

validation_list$seeding$date_sd <- function(value, full_config,index) {
  #full_config = yaml::read_yaml(config_name)
#  if(mode(full_config$seeding$pertubation_sd)!="numeric"){
    if (mode(value)!="numeric") {
      print("No perturbation_sd or date_sd mentioned")
      return(FALSE)
    }
#  }
  return(TRUE)
}

validation_list$seeding$amount_sd <- function(value, full_config,index){
  if(mode(value)!="numeric"){
    print(paste("Standard deviation should be a numeric but is of mode",mode(value))) #Assign default
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$method <- function(value, full_config,index) {
  if (mode(value)!="character") {
    print(paste("The seeding method should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$variant_filename <- function(value, full_config,index) {
  if (mode(value)!="character") {
    print(paste("The variant file name should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if (!file.exists(value)) {
    print(paste("The mentioned variant file :", value, "could not be found."))
    return(FALSE)
  }
  return(TRUE)
}

# validation_list$seeding$seeding_compartments<- function(value,full_config, index){
#   if(mode(value)!="list"){
#     print(paste("Seeding compartments should be a list but is of mode",mode(value)))
#     return(FALSE)
#   }
#   return(TRUE)
# }

validation_list$seeding$seeding_compartments[["::"]]$source_compartment<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The source comparment should be strings but is of mode",mode(value)))
    return(FALSE)
  }
  if(!(value[1] %in% full_config$seir$compartments$infection_stage)){
    print("The source compartment mentioned is not present in infection stages")
    return(FALSE)
  }
  if(!(value[2] %in% full_config$seir$compartments$vaccination_stage)){
    print("The source compartment mentioned is not present in vaccination stages")
    return(FALSE)
  }
  if(!(value[3] %in% full_config$seir$compartments$variant_type)){
    print("The source compartment mentioned is not present in variant types")
    return(FALSE)
  }
  if(!(value[4] %in% full_config$seir$compartments$age_strata)){
    print("The source compartment mentioned is not present in age stratas")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$seeding_compartments[["::"]]$destination_compartment<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The destination comparment should be strings but is of mode",mode(value)))
    return(FALSE)
  }
  if(!(value[1] %in% full_config$seir$compartments$infection_stage)){
    print("The destination compartment mentioned is not present in infection stages")
    return(FALSE)
  }
  if(!(value[2] %in% full_config$seir$compartments$vaccination_stage)){
    print("The destination compartment mentioned is not present in vaccination stages")
    return(FALSE)
  }
  if(!(value[3] %in% full_config$seir$compartments$variant_type)){
    print("The destination compartment mentioned is not present in variant types")
    return(FALSE)
  }
  if(!(value[4] %in% full_config$seir$compartments$age_strata)){
    print("The destination compartment mentioned is not present in age stratas")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$seeding_file_type <- function(value, full_config,index) {
  if (mode(value)!="character") {
    print(paste("Seeding file type should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(value!='seed' & value!='impa'){
    print("Wrong value mentioned should either be seed or impa")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seeding$pop_seed_file<- function(value,full_config, index){
  #full_config = yaml::read_yaml(config_name)
  if(mode(value)=="NULL"){
    return(TRUE)
  }
  if(value=='pseudo_pop_seed_file'){
    print('A pop_seed_file should be mentioned for compartment runs')
    return(FALSE)
  }
  if("compartments" %in% full_config$seir){
  if(!file.exists(value)){
    print(paste("The pop_seed_file",value,"does not exist"))
    return(FALSE)
  }
  }
  return(TRUE)
}


validation_list$seeding$folder_path<- function(value,full_config, index){
  #full_config = yaml::read_yaml(config_name)
  if(mode(value)=="character"){
    if(substr(value, nchar(value), nchar(value))!='/'){
      print("Incorrect folder path: folder path in seeding should have a / in the end")
      return(FALSE)
    }
  }
  return(TRUE)
}

validation_list$seeding$lambda_file<- function(value,full_config, index){
  if(mode(value)!="character"){
      print(paste("Lambda file should be a string but is of mode",mode(value)))
      return(FALSE)
  }
  if (length(value) != 1) {
    print("lambda file should be a single character, but is of length",length(value))
    return(FALSE)
  }
  #full_config<-yaml::read_yaml(config_name)
  if (value == "unused") {
    if (("filtering" %in% names(full_config)) || (full_config$seeding$method %in% c("PoissonDistributed", "NegativeBinomialDistributed"))) {
      print("The lambda file is required, but 'unused' was passed in. Please provide a filename that is not 'unused'")
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!dir.exists(dirname(value))) {
    print("lambda file should be a path to a file, but could not find directory ",dirname(value), "for value", value)
    return(FALSE)
  }
  if (!file.exists(dirname(value))) {
    if (full_config$seeding$lambda_file %in% c("PoissonDistributed", "NegativeBinomialDistributed")) {
      print("lambda file should be a path to a file, but could not find directory ",dirname(value), "for value", value)
      return(FALSE)
    }
  }
  return(TRUE) 
}

######SEIR MODEL SPECIFICATIONS##################
##NO Defaults: Parameters: sigma,gamma,ROs, transitions, compartments
validation_list$seir<-list()
validation_list$seir$integration_method<-function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("Integration method should be a string but is of mode",mode(value),"default will be assigned"))
  }
  return(TRUE)
}


validation_list$seir$parameters[["::"]]$value$distribution<-function(value,full_config,index){
  if(mode(value)!="character"){
    print(paste("The distribution of parameters in seir should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The distribution of parameters in seir should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$parameters[["::"]]$value$value<-function(value,full_config,index){
  if(mode(value)!="numeric"){
    print(paste("The value of parameter",names(full_config$seir$parameters)[index[1]],
                "in seir should be a number but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The value of parameters in seir should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

# validation_list$seir$parameters<-function(value,full_config, index){
#   if (mode(value)!="list") {
#     print(paste("Parameters should be a list but is of mode",mode(value)))
#     return(FALSE)
#   }
#   for (param_name in names(value)){
#     if(grepl("sigma",param_name[[1]])==TRUE){
#       sig=TRUE
#       break
#     }
#     else{
#       sig=FALSE
#     }
#   }
#   if(!sig){
#     print("No sigma mentioned")
#     return(FALSE)
#   }
#   if(is.null(value$gamma)){
#     print("No gamma mentioned")
#     return(FALSE)
#   }
#   
#   #Checks if extra seir parameters are not mentioned depending upon the the 
#   #variants present in seeding$seeeding_compartments 
#   full_config = yaml::read_yaml(config_name)
#   present_variants<-names(full_config$seeding$seeding_compartments)
#   all_variants<-c('WILD','ALPHA','DELTA','OMICRON','VARIANT_X')
#   absent_variants<-all_variants[!(all_variants %in% present_variants)]
#   parameters<-names(value)
#   flag=0 #Checks if a variant which is not present has parameters present
#   for (variant in absent_variants){
#     if(variant=='ALPHA'){
#       if(any(grepl(paste('_',tolower(variant),sep=''),parameters))){
#         print(paste('Variant', variant,' not mentioned in compartments but has seir transition value'))
#         flag=1
#       }      
#     }
#     else{
#       if(any(grepl(tolower(variant),parameters)) |any(grepl(variant,parameters)) ){
#         print(paste('Variant', variant,' not mentioned in compartments but has seir transition value'))
#         flag=1
#       }
#     }
#   }
#   if(flag==1){
#     return(FALSE)
#   }
#   return(TRUE)
#   
# }


validation_list$seir$compartments$infection_stage<-function(value,full_config, index){
  if(any(value=="unused")){
    print("Not a compartment run")
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The infection stages should be strings but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$compartments$vaccination_stage<-function(value,full_config, index){
  if(any(value=="unused")){
    print("Not a compartment run")
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The vaccination stages should be strings but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$compartments$variant_type<-function(value,full_config, index){
  if(any(value=="unused")){
    print("Not a compartment run")
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The variant types should be string strings but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$compartments$age_strata<-function(value,full_config, index){
  if(any(value=="unused")){
    print("Not a compartment run")
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The age stratas should be strings but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}


validation_list$seir$transitions[["::"]]$source<-function(value,full_config,index){
  if(mode(value)!="list"){
    print("The transitions in seir source cannot be NULL")
    return(FALSE)
  }
  #full_config=yaml::read_yaml(config_name)
  if(!all(value[[1]] %in% full_config$seir$compartments$infection_stage)){
    print("The transitions have to be part of the infection stage compartments")
    return(FALSE)
  }
  if(!all(value[[2]] %in% full_config$seir$compartments$vaccination_stage)){
    print("The transitions have to be part of the vaccination stage compartments")
    return(FALSE)
  }
  if(!all(value[[3]] %in% full_config$seir$compartments$variant_type)){
    print("The transitions have to be part of the variant type compartments")
    return(FALSE)
  }
  if(!all(value[[4]] %in% full_config$seir$compartments$age_strata)){
    print("The transitions have to be part of the age strata compartments")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$transitions[["::"]]$destination<-function(value,full_config,index){
  if(mode(value)!="list"){
    print("The transitions in seir destination cannot be NULL")
    return(FALSE)
  }
  #full_config=yaml::read_yaml(config_name)
  if(!all(value[[1]] %in% full_config$seir$compartments$infection_stage)){
    print("The transitions have to be part of the infection stage compartments")
    return(FALSE)
  }
  if(!all(value[[2]] %in% full_config$seir$compartments$vaccination_stage)){
    print("The transitions have to be part of the vaccination stage compartments")
    return(FALSE)
  }
  if(!all(value[[3]] %in% full_config$seir$compartments$variant_type)){
    print("The transitions have to be part of the variant type compartments")
    return(FALSE)
  }
  if(!all(value[[4]] %in% full_config$seir$compartments$age_strata)){
    print("The transitions have to be part of the age strata compartments")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$seir$transitions[["::"]]$rate<-function(value,full_config,index){
  if(mode(unlist(value))!="character"){
    print("The rate of seir transitions cannot be null")
    return(FALSE)
  }
  #full_config=yaml::read_yaml(config_name)
  if(!all(unlist(strsplit(unlist(value),split="\\*")) %in% c(names(full_config$seir$parameters),1:10))){
    print("The rates mentioned in seir transitions should be defined")
    return(FALSE)
  }
  return(TRUE)
}

# validation_list$seir$transitions<-function(value,full_config, index){
#   if(mode(value)!="list"){
#     print(paste("The seir transitions should be a list but is of mode",mode(value)))
#     return (FALSE)
#   }
#   gempyor <- reticulate::import("gempyor")
#   gempyor$config$set_file(config_name)
#   compartments_obj <- gempyor$compartments$Compartments(gempyor$config["seir"])
#   compartments <- compartments_obj$compartments
#   transitions <- compartments_obj$transitions
#   for (i in 1:length(transitions)){
#     if(!(transitions$source[[i]][1] %in% compartments$infection_stage &transitions$destination[[i]][1] %in% compartments$infection_stage)){
#       print("Incorrect Source or Destination compartments present")
#       return(FALSE)
#     }
# 
#     if(!(transitions$source[[i]][2] %in% compartments$vaccination_stage & transitions$destination[[i]][2] %in% compartments$vaccination_stage)){
#       print("Incorrect vaccination mentioned in transitions")
#       return(FALSE)
#     }
# 
#     if(!(transitions$source[[i]][3] %in% compartments$variant_type & transitions$destination[[i]][3] %in% compartments$variant_type)){
#       print("Incorrect variants mentioned in transitions")
#       return(FALSE)
#     }
# 
#     if(!(transitions$source[[i]][4] %in% compartments$age_strata & transitions$destination[[i]][4] %in% compartments$age_strata)){
#       print("Incorrect Age Strata mentioned in transitions")
#       return(FALSE)
#     }
#   }
#   return(TRUE)
# }

###OUTCOMES##
validation_list$outcomes<- list()
validation_list$outcomes$method<- function(value,full_config, index){
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

validation_list$outcomes$param_from_file<- function(value, full_config,index){
  if(mode(value)!="logical"){
    print(paste("Outcomes param_from_file should be logical but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$param_place_file<- function(value, full_config,index){
  #full_config = yaml::read_yaml(config_name)
  if(mode(value)!="character"){
     print(paste("Outcome param_place_file should be a character but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("Outcome param_place_file should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  if(value=="Unused"){
    return(TRUE)
  }
  if(!file.exists(value)){
    print("Output Param File does not exist")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$scenarios<- function(value, full_config,index){
  if(mode(value)!="character"){
    print(paste("Outcomes scenarios should be a string but is of mode",mode(value),"default will be assigned")) #Assign Default
  }
  return(TRUE)
}


validation_list$outcomes$settings[["::"]][["::"]]$source$incidence$infection_stage<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  if(!(value %in% full_config$seir$compartments$infection_stage)){
    print(paste("The infection stage in outcome settings source incidence of setting",index[1],"should be one
                of the infection stage compartments"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$source$incidence$vaccination_stage<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  if(!(value %in% full_config$seir$compartments$vaccination_stage)){
    print(paste("The vaccination stage in outcome settings source incidence of setting",index[1],"should be one
                of the infection stage compartments"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$source$incidence$variant_type<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  if(!(value %in% full_config$seir$compartments$variant_type)){
    print(paste("The variant type in outcome settings source incidence of setting",index[1],"should be one
                of the infection stage compartments"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$source$incidence$age_strata<-function(value, full_config,index){
  #full_config<-yaml::read_yaml(config_name)
  if(mode(value)!="character"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  if(!(value %in% full_config$seir$compartments$age_strata)){
    print(paste("The age_strata in outcome settings source incidence of setting",index[1],"should be one
                of the infection stage compartments"))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$probability$value$distribution<-function(value, full_config,index){
  if(mode(value)!="character"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$probability$value$value<-function(value, full_config,index){
  if(mode(value)!="numeric"){
    print(paste("The source in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The source in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$delay$value$distribution<-function(value, full_config,index){
  if(mode(value)!="character"){
    print(paste("The value in the delay in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The value in the delay in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$settings[["::"]][["::"]]$delay$value$value<-function(value, full_config,index){
  if(mode(value)!="numeric"){
    print(paste("The value in the delay in different settings in outcomes should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The value in the delay in different settings in outcomes should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

# validation_list$outcomes$settings<-function(value, full_config,index,index){
#   if(mode(value)!="list"){
#     print(print("Outcome settings should be a list but is of mode",mode(value),"default will be assigned")) #Assign Default
#   }
#   full_config = yaml::read_yaml(config_name)
#   for (scenario in full_config$outcomes$scenarios){
#     if(!(scenario %in% names(value))){
#       print(paste0("No details mentioned about scenario ",scenario," in outcomes"))
#       return(FALSE)
#     }
#   }
#   return(TRUE)
# }

validation_list$outcomes$interventions$settings[["::"]]$template<-function(value, full_config,index){
  if(mode(value)!="character"){
    print(paste("The template in different settings in outcomes interventions should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The template in different settings in outcomes interventions should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$outcomes$interventions$settings[["::"]]$scenarios<-function(value, full_config,index){
  if(mode(value)!="character"){
    print(paste("The scenarios in different settings in outcomes interventions should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The scenarios in different settings in outcomes interventions should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

###FILTERING
validation_list$filtering<-list()
validation_list$filtering$simulations_per_slot<-function(value,full_config, index){
  if(mode(value)!="numeric"){
    print(paste("simulations_per_slot should be a numeric but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print("The length of simulations_per_slot should be 1")
    return(FALSE)
  }
  if(value%%1!=0){
    print("The simulations_per_slot should be an integer")
    return(FALSE)
  }
  if(value<1){
    print("simulations_per_slot should be 1 or greater")
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$do_filtering<- function(value,full_config, index){
  if(mode(value)!="logical"){
    print(paste("do_filtering should be logical but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$data_path<-function(value,full_config, index){
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

validation_list$filtering$gt_source<- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("gt_source in filtering should be a string but is of mode",mode(value),"default will be assigned"))#Assign default
  }
  return(TRUE)
}

# validation_list$filtering$statistics<- function(value,full_config, index){
#   if(mode(value)!="list"){
#     print(paste("filtering statistics should be a list but is of value",mode(value)))
#     return(FALSE)
#   }
#   return(TRUE)
# }

validation_list$filtering$statistics[["::"]]$name <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The name of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of name of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$aggregator <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The aggregator of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of aggregator of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$period <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The period of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of period of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$sim_var <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The sim_var of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of sim_var of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  # if(!(value %in% c("incidD","incidC","incidH","incidI"))){
  #   print(paste("The sim_var should be be incidD,incidC,incidH,incidI but is of the value",value))
  #   return(FALSE)
  # }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$data_var <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The data_var of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of data_var of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$remove_na <- function(value,full_config, index) {
  if(mode(value)!="logical"){
    print(paste("The remove_na of statistics in filtering should be a logical operator but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of remove_na of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$add_one <- function(value,full_config, index) {
  if(mode(value)!="logical"){
    print(paste("The add_one of statistics in filtering should be a logical operator but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of add_one of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$statistics[["::"]]$likelihood$dist <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The dist in likelihood of statistics in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of dist in likelihood of statistics in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$hierarchical_stats_geo[["::"]]$name <- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The name in hierarchial_stats_geo in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of name in hierarchial_stats_geo in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$hierarchical_stats_geo[["::"]]$module <- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The module in hierarchial_stats_geo in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of module in hierarchial_stats_geo in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$hierarchical_stats_geo[["::"]]$geo_group_col <- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The geo_group_col in hierarchial_stats_geo in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of geo_group_col in hierarchial_stats_geo in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$hierarchical_stats_geo[["::"]]$transform <- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The transform in hierarchial_stats_geo in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of transform in hierarchial_stats_geo in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$priors[["::"]]$name<- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The name in priors in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of name in priors in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$priors[["::"]]$module<- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The module in priors in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of module in priors in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$filtering$priors[["::"]]$likelihood$dist<- function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("The dist of likelihood in priors in filtering should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of dist of likelihood in priors in filtering should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  if(!(value %in% c("normal","logit_normal"))){
    print(paste("The dist of likelihood in priors in filtering should either be normal or logit_normal but is",value))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions<-list()
validation_list$interventions$scenarios<-function(value,full_config, index){
  if(mode(value)!="character"){
    print(paste("Interventions scenarios should be a list but is of mode",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

# validation_list$interventions$settings<-function(value,full_config, index){
#   if(mode(value)!="list"){
#     print(paste("The intervention settings should be a list but is of mode",mode(value)))
#     return(FALSE)
#   }
#   return(TRUE)
# }

validation_list$interventions$settings[["::"]]$template <- function(value,full_config, index) {
  if(mode(value)!="character"){
    print(paste("The template in interventions settings",
                names(full_config$interventions$settings)[index[1]],
                "should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The length of template in interventions settings",
                names(full_config$interventions$settings)[index[1]],
                " should be of length 1 but is of length",mode(length)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$parameter<-function(value,full_config,index){
  if(full_config$interventions$settings[[index[1]]]$template=='Stacked'){
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The parameter in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The parameter in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$affected_geoids<-function(value,full_config,index){
  if(full_config$interventions$settings[[index[1]]]$template %in% c('Stacked','MultiTimeReduce')){
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The affected geoids in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The affected geoids in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$period_start_date<-function(value,full_config,index){
  if(full_config$interventions$settings[[index[1]]]$template %in% c('Stacked','MultiTimeReduce')){
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The period_start_date in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The period_start_date in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$period_end_date<-function(value,full_config,index){
  if(full_config$interventions$settings[[index[1]]]$template %in% c('Stacked','MultiTimeReduce')){
    return(TRUE)
  }
  if(mode(value)!="character"){
    print(paste("The period_end_date in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The period_end_date in intervention settings",
                names(full_config$interventions$settings)[index[1]],
                "should be of length 1 but is of length",length(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$value<-function(value,full_config,index){
  #print(names(full_config$interventions$settings)[[index[1]]])
  print("+++++++++++++++++++++++++++++++++++++++++++")
  print(value)
  if(full_config$interventions$settings[[index[1]]]$template =='Stacked'){
    return(TRUE)
  }
  print("uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu")
  print(check_config(config=value,checks = distribution_validation_list))
  print("ddddddddddddddddddddddddddddddddddddddddddddd")
  return(check_config(config=value,checks = distribution_validation_list))
}

validation_list$interventions$settings[["::"]]$pertubation<-function(value,full_config,index){
  print("SSSSSSSSSSSSs")
  print(index[[1]])
  stop()
  if(full_config$interventions$settings[[index[1]]]$template =='Stacked'){
    print("LLLLLLLLLLLLLLLLLLLLLL")
    return(TRUE)
  }
  print("JJJJJJJJJJJJJJJJJJJJJ")
  return(check_config(config=value,checks = distribution_validation_list))
}



validation_list$interventions$settings[["::"]]$groups[["::"]]$affected_geoids<- function(value,full_config, index){
  if(mode(value)!="character"){
    stop(paste("The affected geoids groups in every intervention setting should 
                be a string but is of mode",mode(value)))
    return(FALSE)
  }
  if(length(value)!=1){
    print(paste("The affected geoids groups in every intervention setting should 
                be of length 1 but is of length",mode(value)))
    return(FALSE)
  }
  return(TRUE)
}

validation_list$interventions$settings[["::"]]$groups[["::"]]$periods[["::"]]$start_date<-
  function(value,full_config,index){
    if(mode(value)!="character"){
      print("The start date in periods of  groups in every intervention setting should 
      not be NULL")
      return(FALSE)
    }
    if(length(value)!=1){
      print(paste("The start date in periods of groups in every intervention setting should 
                be of length 1 but is of length",mode(value)))
      return(FALSE)
    }
    if(is.na(as.Date(value,optional=TRUE))){
      print("The start date in periods of groups in every intervention setting should be a
            valid date")
      return(FALSE)
    }
    return(TRUE)
  }

validation_list$interventions$settings[["::"]]$groups[["::"]]$periods[["::"]]$end_date<-
  function(value,full_config,index){
    if(mode(value)!="character"){
      print("The end date in periods of  groups in every intervention setting should 
      not be NULL")
      return(FALSE)
    }
    if(length(value)!=1){
      print(paste("The end date in periods of groups in every intervention setting should 
                be of length 1 but is of length",mode(value)))
      return(FALSE)
    }
    if(is.na(as.Date(value,optional=TRUE))){
      print("The end date in periods of groups in every intervention setting should be a
            valid date")
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
check_config <- function(config= yaml::read_yaml(config_name),full_config=config, 
                         checks = validation_list,
                         name_prefix = NULL, no_check_fields = config_ignore_checks, index = NULL) {
  config_is_valid <- TRUE
  subconfig_valid <- TRUE
  for (field_name in names(checks)) {
    #print(field_name)
    if (field_name == "::") {
      for (new_index in seq_len(length(config))) {
        if (class(config[[field_name]]) %in% c("NULL", "list")) {
          subconfig_valid <- check_config(
            config[[new_index]],full_config, 
            checks[[field_name]],
            paste0(name_prefix, ifelse(is.null(name_prefix),
                                                                    "", "::"
            ), c(index, new_index)),
            index = c(index, new_index)
          )
        } else {
          subconfig_valid <- checks[[field_name]](config[[new_index]],full_config, c(index, new_index))
        }
        config_is_valid <- config_is_valid && subconfig_valid
      }
      next
    }
    if (class(checks[[field_name]]) == "list") {
      if (class(config[[field_name]]) %in% c("NULL", "list")) {
        subconfig_valid <- check_config(
          config[[field_name]],full_config, 
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
      field_valid <- checks[[field_name]](config[[field_name]],full_config,index)###
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

config_defaults<-list()

# config_defaults[["name"]]<-function(config,index){return(NULL)}
# config_defaults[["dt"]]<-function(config,index){return(NULL)}
# config_defaults[["smh_round"]]<-function(config,index){return(NULL)}
# config_defaults[["start_date"]]<-function(config,index){return(NULL)}
# config_defaults[["end_date"]]<-function(config,index){return(NULL)}
# config_defaults[["start_date_ground_truth"]]<-function(config,index){return(NULL)}
config_defaults[["end_date_ground_truth"]]<-function(config,index){
  return(config$end_date)
}
# config_defaults[["nsimulations"]]<-function(config,index)<-return(NULL)

config_defaults[["spatial_setup"]]<-list()
config_defaults[["spatial_setup"]][["geodata"]]<-function(config,index){
  return("data/geodata.csv")
}
config_defaults[["spatial_setup"]][["mobility"]]<-function(config,index){
  return("data/mobility.csv")
}
config_defaults[["spatial_setup"]][["popnodes"]]<-function(config,index){
  return("population")
}
config_defaults[["spatial_setup"]][["state_level"]]<-function(config,index){
  return(FALSE)
}

config_defaults[["seir"]][["compartments"]][["infection_stage"]]<-function(config,index){
  return("unused")
}

config_defaults[["seir"]][["compartments"]][["vaccination_stage"]]<-function(config,index){
  return("unused")
}
config_defaults[["seir"]][["compartments"]][["variant_type"]]<-function(config,index){
  return("unused")
}
config_defaults[["seir"]][["compartments"]][["age_strata"]]<-function(config,index){
  return("unused")
}

config_defaults[["seeding"]]<-list()
config_defaults[["seeding"]][["amount_sd"]]<-function(config,index){
  return(1)
}
config_defaults[["seeding"]][["pertubation_sd"]]<-function(config,index){
  if(mode(config$seeding$date_sd)!="NULL"){
    return(config$seeding$date_sd)
  }
  else{
    return(NULL)
  }
}
config_defaults[["seeding"]][["date_sd"]]<-function(config,index){
  if(mode(config$seeding$pertubation_sd)!="NULL"){
    return(config$seeding$pertubation_sd)
  }
  else{
    return(NULL)
  }
}  


config_defaults[["seeding"]][["pop_seed_file"]]<-function(config,index){
  if("compartments" %in% names(config$seir)){
    return("pseudo_pop_seed_file")
  }
  else{
    return(NULL)
  }
}

config_defaults[["seeding"]][["folder_path"]]<-function(config,index){
  if(!(config$seeding$method %in% c("FolderDraw","SetInitialConditions","InitialConditionsFolderDraw"))){
    return(NULL)
  }
  return("importation/minimal/")
}

config_defaults[["seeding"]][["lambda_file"]]<-function(config,index){
  if ('filtering' %in% names(config)) {
    dir.create("data/generated_data")
    return("data/generated_data/seeding.csv")
  }
  if (any(c('PoissonDistributed', 'NegativeBinomialDistributed') %in% names(config$seeding$methdo))) {
    return("data/static_data/seeding.csv")
  }
  return("unused")
}

config_defaults[["outcomes"]][["param_from_file"]]<-function(config_index){
  return(FALSE)
}

config_defaults[["outcomes"]][["param_place_file"]]<-function(config_index){
  if(config$param_from_file){
    return ("unused")
  }
  return(NULL)
}

#' @name complete_config
#' @description Makes default values in the config explicit recursively
#' @param config A config (or section of the config) to apply defaults to
#' @param defaults A named list of defaults, where each element is either a named list of defaults, or a function which takes the full config, and uses it to provide a default for that field
#' @param original_config In case of recursion, the original config this was part of.  This is what is passed to the functions in the defaults
#' @export
complete_config <- function(config, defaults = config_defaults, original_config = config, index = NULL) {
  for (field_name in names(defaults)) {
    if (class(defaults[[field_name]]) == "list") {
      # Special case for array like members
      if (field_name == "::") {
        for (new_index in seq_len(length(config))) {
          config[[new_index]] <- complete_config(
            config[[new_index]], defaults[[field_name]],
            original_config,
            index = c(index, new_index)
          )
        }
        next
      } else if (!(class(config[[field_name]]) %in% c("NULL", "list"))) {
        stop(paste(
          "config field", field_name, "should be a list, but was of type",
          class(config[[field_name]]), "with value", config[[field_name]]
        ))
      } 
      config[[field_name]] <- complete_config(
        config[[field_name]], defaults[[field_name]],
        original_config
      )
    } else if (is.null(config[[field_name]])) {
      config[[field_name]] <- defaults[[field_name]](original_config, index)
    }
  }
  return(config)
}