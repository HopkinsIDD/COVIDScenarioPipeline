
library(tidyverse)

states_of_interest=c("NY","NJ","CT")

regioncode="NYC"
dest=c("JFK","BUF","LGA")
output_dir=file.path("model_output", "importation",regioncode)




##' 
##' Sum importation counts to airport attribution clusters
##' Airport clusters are generated using `do_airport_attribution` for aiports in close proximity to 
##' each other e.g. ORD/MDY, DCA/IAD, SFO/OAK
##'
##' 
##' @title run_distrib_imports 
##'
##' @param states_of_interest single simulation result from importation model
##' @param regioncode Region/project name
##' @param yr Year of county population data
##' @param mean_travel_file Filename of monthly mean travelers into each airport in the region
##' @param states_of_interest States for which to get county populations
##' @param travelers_threshold Minimum monthly average travel volume to be included
##' @param airport_cluster_threshold Distance by which airports can be separated to be in the same cluster, in km; Haversine distance.
##' @param imports_sim_file file name root for each importation simulation. 
##' @param local_dir local data directory
##'
##' @return A data.frame of clustered airports, dates, and nmber of importations
##' 
##' @import doParallel
##' 
##' 
##'
examine_single_dest <- function(dests=NULL, output_dir=file.path("model_output", "importation",regioncode)){
    
    # input data
    input_data <- readr::read_csv(file.path(output_dir,"input_data.csv"))
    input_data_dest <- input_data %>% dplyr::filter(destination %in% dest)  
    
    # travel data
    travel_data <- readr::read_csv(file.path(output_dir,"travel_data_monthly.csv"))
    travel_data_dest <- input_data %>% dplyr::filter(destination %in% dest)  
    

    # Pull importation results and combine them
    import_files <- list.files(output_dir, "imports_sim")

    # get the first one
    imports <- readr::read_csv(file.path(output_dir,paste0("imports_sim", 1, ".csv")))
    if (!is.null(dests)){
        imports <- imports %>% dplyr::filter(destination %in% dest)
    }
    imports <- imports %>% dplyr::group_by(destination, source, t) %>%
        dplyr::summarise(imports = sum(this.sim, na.rm = TRUE))
    n_dest <- length(unique(imports$destination))
    n_source <- length(unique(imports$source))
    n_t <- length(unique(imports$t))
    n_sim <- length(import_files)
    
    #imports_res <- list()
    import_array <- array(0, dim=c(n_source, n_dest, n_t, n_sim), 
                          dimnames = list(sort(unique(imports$source)),
                                          sort(unique(imports$destination)),
                                          sort(unique(imports$t)),
                                          seq_len(n_sim)))
    
    
    for (n in seq_len(length(import_files))){
    
        # import results
        imports <- readr::read_csv(file.path(output_dir,paste0("imports_sim", n, ".csv")))
        
        if (!is.null(dests)){
            imports <- imports %>% dplyr::filter(destination %in% dest)
        }
        
        imports <- imports %>% dplyr::group_by(destination, source, t) %>%
            dplyr::summarise(imports = sum(this.sim, na.rm = TRUE))
        
        #imports_res[[n]] <- imports %>% mutate(sim=n)
        import_array[,,,n] <- reshape2::acast(imports %>% dplyr::group_by(source, destination, t),
                                               source ~ destination ~ t, value.var = "imports", 
                                               fill=0)
    }
    #imports_res <- data.table::rbindlist(imports_res)
    
    #format(object.size(imports_res), "Mb")
    format(object.size(import_array), "Mb")
    
        
    # imports_sum_dest <- imports_res %>% 
    #     dplyr::group_by(destination, t) %>%
    #     dplyr::summarise(sim_mean = mean(imports),
    #               sim_ll = min(imports),
    #               sim_ul = max(imports))
    # 
    # imports_sum <- imports_res %>% 
    #     dplyr::group_by(destination, source, t) %>%
    #     dplyr::summarise(sim_mean = mean(imports),
    #               sim_ll = min(imports),
    #               sim_ul = max(imports))
    
    
    dest_ <- apply(import_array, c(2:4), sum)
    
    
    dest_mean <- apply(dest_, c(1:2), mean)
    
    summarize_dests <- function(data=dest_, ){
        dest_res <- apply(dest_, c(1:2), quantile, prob=0.025)
        dest_res <- dest_res %>% as_tibble() %>% 
            mutate(destination = row.names(dest_res)) %>%
            pivot_longer(-destination, 
                         names_to = "t",
                         values_to = "imports") 
        dest_res <- dest_res %>% mutate(t = as.Date(as.integer(t), origin="1970-01-01"))
        
        return(dest_res)
    }

    
    
    dest_ul <- apply(dest_, c(1:2), quantile, prob=0.975)
    
    
    
    
    
    source_ <- apply(import_array, c(1,3:4), sum)
    
    
    
    
    dest_cum <- apply(dest_, c(4), sum)
    
    
    dest_ll <- apply(import_array, c(2:4), quantile, probs=.025)
    
    dest_cum <- apply(import_array, c(4), sum)
    
    
    
    
    
    ggplot()
    
    
    
    
    
    
    
}











#### DEAL WITH THIS BELOW LATER



##'
##' Get negative binomial estimates for each time and destination
##'
##' @param importation_sim 4D array outputted from the importation model
##' @param cores number of cores to use in parallel. if not parallel, specify 1.
##' 
##' @import doParallel
##'
##' @export
##'
calc_nb_import_pars <- function(importation_sim, cores=4){
    
    # aggregate to just destination
    import_sim_dests <- apply(importation_sim, 2:4, sum, na.rm=TRUE)
    
    dests <- dimnames(import_sim_dests)[[1]]
    t <- dimnames(import_sim_dests)[[2]]
    n_t <- length(t)
    n_dest <- length(dests)
    
    # Make the blank parameter data.frame
    #import_pars_df <- tidyr::expand_grid(destination=dests, t=t, size=1, mu=0)
    
    
    # Set up the cluster for parallelization
    library(doParallel)
    print(paste0("Making a cluster of ", cores," for parallelization."))
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    # Suppress errors for this loop
    options(show.error.messages = FALSE)
    
    import_pars_df_all <- foreach(t_=seq_len(length(t)), 
                                  .packages = "fitdistrplus",
                                  .combine = "rbind") %dopar% {
                                      
                                      import_pars_df <- data.frame(destination=dests, t=t_, size=1, mu=0)
                                      
                                      for (d_ in seq_len(length(dests))){
                                          
                                          pars_ <- tryCatch ( {
                                              fitdistrplus::fitdist(import_sim_dests[d_, t_, ], distr="nbinom", method="mle")$estimate
                                          },
                                          #warning = function(w) { },
                                          error = function(e) {
                                              c(1,0)
                                          })
                                          
                                          import_pars_df[d_, 3:4] <- pars_
                                          
                                      }
                                      import_pars_df
                                  }
    
    # Un-suppress errors
    options(show.error.messages = TRUE)
    
    parallel::stopCluster(cl)
    
    
    # write_csv(import_pars_df, file.path("output",project_name, sprintf("covid_importation_nb_params_%s_batch_v%s.csv", batch, version)))
    return(import_pars_df_all)
}



