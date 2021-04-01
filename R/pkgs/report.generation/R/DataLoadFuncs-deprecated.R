#' Depracated function that returns a function to read files of a specific type (or automatically detected type based on extension)
#' @param extension The file extension to read files of
#' @param ... Arguments to pass to the reading function
#' @return A function which will read files with that extension.
#'  - We use readr::read_csv for csv files
#'  - We use arrow::read_parquet for parquet files
#'  - We use a function that detects the extension and calls this function if the auto extension is specified.
#' @export 
#' 
read_file_of_type <- function(extension,...){
    if(extension == 'csv'){
        return(function(x){suppressWarnings(readr::read_csv(x,,col_types = cols(
            .default = col_double(),
            time=col_date(),
            uid=col_character(),
            comp=col_character(),
            geoid=col_character()
        )))})
    }
    if(extension == 'parquet'){
        return(function(x){
            tmp <- arrow::read_parquet(x) 
            if("POSIXct" %in% class(tmp$time)){
                tmp$time <- lubridate::as_date(tz="GMT",tmp$time)
            }
            tmp
        })
    }
    if(extension == 'auto'){
        return(function(filename){
            extension <- gsub("[^.]*\\.","",filename)
            if(extension == 'auto'){stop("read_file_of_type cannot read files with file extension '.auto'")}
            read_file_of_type(extension)(filename)
        })
    }
    if(extension == 'shp'){
        return(sf::st_read)
    }
    stop(paste("read_file_of_type cannot read files of type",extension))
}


##' Depracated function designed to allow generic filtering and summarization
##' of simulations before merging them together when loading outputs
##' from the infection genertion modeling pipeline.
##' 
##' @param scenario_dir the subdirectory containing this scenario
##' @param post_process function that does processing to sims before loading 
##' @param post_process function that does processing after 
##' @param geoid_len in defined, this we want to make geoids all the same length
##' @param padding_char character to add to the front of geoids if fixed length
##' @param ... additional parameters to pass to pre and/or post process
##' 
##' 
##' @return a combined data frame of all hospital simulations with filters applied pre merge.
##' 
##' 
##' @author Justin Lessler
##' 
##' @export
load_scenario_sims_filtered <- function(scenario_dir, 
                                        num_files = NA,
                                        post_process = function(x) {x},
                                        pre_process = function(x){x},
                                        geoid_len = 0,
                                        padding_char = "0",
                                        file_extension = 'auto',
                                        ...) {
    
    require(tidyverse)
    require(foreach)
    
    if (is.na(num_files)) {
        files <- dir(sprintf("model_output/%s", scenario_dir), full.names = TRUE)
    } else {
        files <- c()
        for (scenario in scenario_dir) {
            all_files <- dir(sprintf("model_output/%s", scenario), full.names = TRUE)
            if (length(all_files) == num_files) {
                files <- c(files, all_files) 
            } else if (num_files < length(all_files)) {
                warning(paste0("You are only reading in ", num_files, " out of ", length(all_files), " files in ", scenario,
                               ". Check the num_files argument if this is unexpected.\n"))
                files <- c(files, all_files[seq_len(num_files)])
            } else {
                stop(paste0("There were ", length(all_files), " files in ", scenario, " but num_files is ", num_files, "."))
            }
        }
    }
    if (length(files) == 0) {
        stop(paste0("There were no files in ",getwd(), "/", sprintf("model_output/%s", scenario_dir)))
    }
    
    read_file <- read_file_of_type(file_extension)
    
    if (geoid_len > 0) {
        padfn <- function(x) {x%>% dplyr::mutate(geoid = str_pad(geoid,width =geoid_len,pad=padding_char))}
    } else {
        padfn <- function(x) {x}
    }
    
    rc <- foreach(i = 1:length(files)) %dopar% {
        require(tidyverse)
        
        read_file(files[i]) %>%
            pre_process(...) %>%
            pivot_longer(cols=c(-time, -comp), names_to = "geoid", values_to="N") %>% 
            padfn %>%
            post_process(...) %>%
            mutate(sim_num = i)
    }
    
    rc <- dplyr::bind_rows(rc)
    return(rc)
}
