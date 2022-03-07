# Preamble ---------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(foreach)
library(doParallel)
library(itertools)


option_list = list(
  optparse::make_option(c("-t", "--test"), action = "store", default = 2, type='integer', help = "Test id to compile"),
  optparse::make_option(c("-i", "--do_int"), action = "store", default = FALSE, type='logical', help = "Postprocess intermediate trajectories"),
  optparse::make_option(c("-r", "--redo"), action = "store", default = FALSE, type='logical', help = "Postprocess intermediate trajectories"),
  optparse::make_option(c("-j", "--jobs"), action = "store", default = parallel::detectCores(), type = 'integer', help = "Number of jobs to run in parallel")
)

parser <- optparse::OptionParser(option_list=option_list)
opt <- optparse::parse_args(parser)
do_int <- opt$do_int
redo <- opt$redo

# Load test inforamtion
all_tests <- yaml::read_yaml("all_tests.yml")
test <- all_tests[[opt$test]]
test_name <- names(all_tests)
runid <- test$runid
scenario <- test$scenario
setup <- test$setup
out_dir <- "outputs"
config <- covidcommon::load_config(test$config_file)

out_dir <- "notebooks/outputs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = T)


# Functions --------------------------------------------------------------------

errorStats <- function(data, sim) {
  tmp <- inner_join(data, sim, by = c("date" = "time", "var" = "var", "geoid" = "geoid")) 
  error_stats <- tmp %>% 
    group_by(var, geoid) %>% 
    summarise(bias = sum(value - N)/length(value),
              mae = sum(abs(value - N))/length(value),
              n = length(value))
  
  return(error_stats)
}


errorParamStats <- function(truth, sample, param = "npis") {
  
  if (param == "npis") {
    tmp <- inner_join(
      truth %>% rename(truth = reduction),
      sample %>% mutate(geoid = as.character(geoid)) %>% 
        select(geoid, npi_name, reduction, it, sim) %>% 
        rename(sample = reduction), 
      by = c("geoid", "npi_name")
    ) %>% 
      rename(var = npi_name)
    
  } else if (param == "outcomes") {
    tmp <- inner_join(
      truth %>% rename(truth = value),
      sample %>% mutate(geoid = as.character(geoid)) %>% 
        select(geoid, quantity, outcome, source, it, sim, value) %>% 
        rename(sample = value)
    ) %>% 
      mutate(var = str_c(outcome, source, quantity, sep = "-"))
  } else {
    stop("Parameter type not recognized")
  }
  
  error_stats <- tmp %>% 
    group_by(var, it, sim, geoid) %>% 
    summarise(
      bias = sum(sample - truth)/length(sample),
      mae = sum(abs(sample - truth))/length(sample),
      n = length(sample)
    )
  
  return(error_stats)
}

prodMinusOne <- function(x) {
  prod(1-x) 
}

# Simulations -----------------------------------------------------------------

outfile_sims <- glue::glue("{out_dir}/sim_stats_{setup}_{scenario}_{runid}.csv")

if (!file.exists(outfile_sims) | redo) {
  
  hosp_files <- dir(glue::glue("model_output/hosp/{setup}/{scenario}/med/{runid}/"), 
                    full.names = T, 
                    recursive = T,
                    pattern = "parquet") %>% 
    .[str_detect(., "final")] 
  
  sims <- map_df(hosp_files, 
                 function(x) {
                   arrow::read_parquet(x) %>% 
                     inference::compute_cumulative_counts() %>% 
                     inference::compute_totals() %>% 
                     mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)")),
                            geoid = as.character(geoid)) 
                 }) 
  
  
  sim_stats <- sims %>% 
    gather(var, value, -time, -geoid, -sim) %>% 
    group_by(time, geoid, var) %>% 
    summarise(q025 = quantile(value, .025),
              q25 = quantile(value, .25),
              q75 = quantile(value, .75),
              q975 = quantile(value, .975))
  
  write_csv(sim_stats, path = outfile_sims)
  
  outfile_all_sim_errors <- glue::glue("{out_dir}/sims_all_errors_{setup}_{scenario}_{runid}.csv")
  
  if (do_int & (!file.exists(outfile_all_sim_errors) | redo)) {
    
    # Which variables were used in fitting
    vars <- map_chr(config$filtering$statistics, "sim_var")
    
    vars <- c(vars, "incidI")
    
    # Load data to compute errors
    data <- read_csv(config$filtering$data_path) %>% 
      rename(geoid = FIPS) %>% 
      pivot_longer(cols = c(-date, -geoid), values_to = "N", names_to = "var") %>%
      mutate(geoid = factor(geoid)) %>% 
      filter(var %in% vars)
    
    # All output files to compute changes in MAE and Bias
    all_sim_files <- dir(glue::glue("model_output/hosp/{setup}/{scenario}/med/{runid}/"), 
                         full.names = T, 
                         recursive = T,
                         pattern = "parquet") %>% 
      .[str_detect(., "intermediate")] %>% 
      .[str_detect(., "global")] %>% 
      .[str_detect(., "[0-9]+\\.[0-9]+\\.[0-9]+")]
    
    cl <- parallel::makeCluster(opt$jobs)
    doParallel::registerDoParallel(cl)
    
    all_sims_errors <- foreach(files = ichunk(all_sim_files, round(length(all_sim_files)/16)),
                               .combine = rbind,
                               .inorder = F,
                               .packages = c("tidyverse", "glue", "arrow"),
                               .export = "runid") %dopar% 
      {
        map_df(files, 
               function(x) {
                 arrow::read_parquet(x) %>% 
                   select(time, geoid, one_of(vars)) %>% 
                   gather(var, value, -time, -geoid) %>% 
                   mutate(time = as.Date(time)) %>% 
                   errorStats(data, .) %>% 
                   mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
                          sim = as.numeric(str_extract(x, glue::glue("(?<=/0)[0-9]+(?=\\.)")))) %>% 
                   select(-contains("X"))
               })
      }
    parallel::stopCluster(cl)
    
    write_csv(all_sims_errors, path = outfile_all_sim_errors)
  }
}

cat("---- DONE simulations \n")
# NPIs --------------------------------------------------------------------

outfile_npis <- glue::glue("{out_dir}/npis_{setup}_{scenario}_{runid}.csv")

if (!file.exists(outfile_npis) | redo) {
  
  npi_files <- dir(glue::glue("model_output/snpi/{setup}/{scenario}/med/{runid}/"), 
                   full.names = T, 
                   recursive = T,
                   pattern = "parquet") %>% 
    .[str_detect(., "final")] 
  
  npis <- map_df(npi_files, 
                 function(x) {
                   arrow::read_parquet(x) %>% 
                     mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)")),
                            geoid = as.character(geoid)) 
                 }) 
  
  write_csv(npis, path = outfile_npis)
  
  outfile_all_npis <- glue::glue("{out_dir}/npis_all_{setup}_{scenario}_{runid}.csv")
  outfile_all_npis_errors <- glue::glue("{out_dir}/npis_all_errors_{setup}_{scenario}_{runid}.csv")
  
  if (do_int & (!file.exists(outfile_all_npis) | redo)) {
    # Check progression
    all_npi_files <- dir(glue::glue("model_output/snpi/{setup}/{scenario}/med/{runid}/"), 
                         full.names = T, 
                         recursive = T,
                         pattern = "parquet") %>% 
      .[str_detect(., "intermediate")] %>% 
      .[str_detect(., "global")] %>% 
      .[str_detect(., "[0-9]+\\.[0-9]+\\.[0-9]+")]
    
    cl <- parallel::makeCluster(opt$jobs)
    doParallel::registerDoParallel(cl)
    
    # All npi values
    npis <- foreach(files = ichunk(all_npi_files, round(length(all_npi_files)/16)),
                    .combine = rbind,
                    .inorder = F,
                    .packages = c("tidyverse", "glue", "arrow"),
                    .export = "runid") %dopar% 
      {
        map_df(files, 
               function(x) {
                 arrow::read_parquet(x) %>% 
                   mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
                          sim = as.numeric(str_extract(x, glue::glue("(?<=/0)[0-9]+(?=\\.)"))),
                          geoid = as.character(geoid)) %>% 
                   select(-contains("X"))
               })
      }
    
    write_csv(npis, path = outfile_all_npis)
    parallel::stopCluster(cl)
    
    # Compute errors
    npis_generation <- read_csv(glue::glue("data/generated/generate_testInference_{test$runid}_snpi.csv"))
    
    npis_generation <- npis_generation %>% 
      select(geoid, npi_name, reduction) %>% 
      rbind(
        npis_generation %>% 
          group_by(geoid) %>% 
          summarise(reduction = 1-prodMinusOne(reduction)) %>% 
          mutate(npi_name = str_c("reff", geoid)) %>% 
          select(geoid, npi_name, reduction) %>% 
          ungroup()
      )
    
    npis <- npis %>% 
      rbind(
        npis %>% 
          group_by(geoid, it, sim, parameter, start_date, end_date) %>% 
          summarise(reduction = 1-prodMinusOne(reduction)) %>% 
          mutate(npi_name = str_c("reff", geoid)) %>% 
          select(one_of(colnames(npis))) %>% 
          ungroup()
      )
    
    npi_errors <- errorParamStats(npis_generation, npis)
    
    write_csv(npi_errors, path = outfile_all_npis_errors)
  }
}

cat("---- DONE NPIs \n")
# Seeding -----------------------------------------------------------------

outfile_seeding <- glue::glue("{out_dir}/seedings_{setup}_{scenario}_{runid}.csv")

if (!file.exists(outfile_seeding) | redo) { 
  seeding_files <- dir(glue::glue("model_output/seed/{setup}/{scenario}/med/{runid}/"), 
                       full.names = T, 
                       recursive = T,
                       pattern = "csv") %>% 
    .[str_detect(., "final")] 
  
  seedings <- map_df(seeding_files, 
                     function(x) {
                       read_csv(x) %>% 
                         mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)"))) %>% 
                         select(-contains("X"))
                     })
  
  write_csv(seedings, path = outfile_seeding)
  
  outfile_all_seedings <- glue::glue("{out_dir}/seedings_all_{setup}_{scenario}_{runid}.csv")
  
  if (do_int &  (!file.exists(outfile_all_seedings) | redo)) {
    # Check progression
    seeding_files <- dir(glue::glue("model_output/seed/{setup}/{scenario}/med/{runid}/"), 
                         full.names = T, 
                         recursive = T,
                         pattern = "csv") %>% 
      .[str_detect(., "intermediate")] %>% 
      .[str_detect(., "global")]
    
    cl <- parallel::makeCluster(opt$jobs)
    doParallel::registerDoParallel(cl)
    
    seedings <- foreach(files = ichunk(seeding_files, round(length(seeding_files)/16)),
                        .combine = rbind,
                        .inorder = F,
                        .packages = c("tidyverse", "glue"),
                        .export = "runid") %dopar% 
      {
        map_df(files, 
               function(x) {
                 read_csv(x) %>% 
                   mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
                          sim = as.numeric(str_extract(x, glue::glue("(?<=/0)[0-9]+(?=\\.)"))),
                          place = as.character(place)) %>% 
                   select(-contains("X"))
               })
      }
    parallel::stopCluster(cl)
    
    write_csv(seedings, path = outfile_all_seedings)
  }
}

cat("---- DONE seeding \n")
# Hpars -------------------------------------------------------------------

outfile_hpar <- glue::glue("{out_dir}/hpar_{setup}_{scenario}_{runid}.csv")

if (!file.exists(outfile_hpar) | redo) { 
  hpar_files <- dir(glue::glue("model_output/hpar/{setup}/{scenario}/med/{runid}/"), 
                    full.names = T, 
                    recursive = T,
                    pattern = "parquet") %>% 
    .[str_detect(., "final")] 
  
  hpar <- map_df(hpar_files, 
                 function(x) {
                   arrow::read_parquet(x) %>% 
                     mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)")),
                            geoid = as.character(geoid)) 
                 }) 
  write_csv(hpar, path = outfile_hpar)
  
  outfile_all_hpar <- glue::glue("{out_dir}/hpar_all_{setup}_{scenario}_{runid}.csv")
  outfile_all_hpar_errors <- glue::glue("{out_dir}/hpar_all_errors_{setup}_{scenario}_{runid}.csv")
  
  if (do_int & (!file.exists(outfile_all_hpar) | redo)) {
    # Check progression
    all_hpar_files <- dir(glue::glue("model_output/hpar/{setup}/{scenario}/med/{runid}/"), 
                          full.names = T, 
                          recursive = T,
                          pattern = "parquet") %>% 
      .[str_detect(., "intermediate")] %>% 
      .[str_detect(., "global")] %>% 
      .[str_detect(., "[0-9]+\\.[0-9]+\\.[0-9]+")]
    
    cl <- parallel::makeCluster(opt$jobs)
    doParallel::registerDoParallel(cl)
    
    # Get fitted params
    hpar_fitted <- pmap_df(list(x = config$outcomes$settings$med, 
                                y = names(config$outcomes$settings$med)),
                           function(x, y) {
                             if ("perturbation" %in% names(x$probability)) {
                               tibble::tibble(outcome = y, source = x$source)
                             }
                           })%>% 
      mutate(os = str_c(outcome, source, sep = "-")) 
    
    # All npi values
    hpars <- foreach(files = ichunk(all_hpar_files, round(length(all_hpar_files)/16)),
                    .combine = rbind,
                    .inorder = F,
                    .packages = c("tidyverse", "glue", "arrow"),
                    .export = "runid") %dopar% 
      {
        map_df(files, 
               function(x) {
                 arrow::read_parquet(x) %>% 
                   mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
                          sim = as.numeric(str_extract(x, glue::glue("(?<=/0)[0-9]+(?=\\.)"))),
                          geoid = as.character(geoid)) %>% 
                   select(-contains("X")) %>% 
                   mutate(os = str_c(outcome, source, sep = "-")) %>% 
                   filter(os %in% hpar_fitted$os)
               })
      }
    
    write_csv(hpars, path = outfile_all_hpar)
    parallel::stopCluster(cl)
    
    # Compute errors
    hpar_generation <- read_csv(glue::glue("data/generated/generate_testInference_{test$runid}_hpar.csv"))
    
    hpar_errors <- errorParamStats(hpar_generation, hpars, param = "outcomes")
    
    write_csv(hpar_errors, path = outfile_all_hpar_errors)
  }
}

cat("---- DONE hpars \n")
# Logliks ----------------------------------------------------------------------

outfile_globliks <- glue::glue("{out_dir}/glob_lliks_{setup}_{scenario}_{runid}.csv")
outfile_liks <- glue::glue("{out_dir}/lliks_{setup}_{scenario}_{runid}.csv")

if(!file.exists(outfile_globliks) | redo) {
  
  # glob_lik_files <- dir(glue::glue("model_output/llik/{setup}/{scenario}/med/{runid}/"), 
  #                       full.names = T, 
  #                       recursive = T,
  #                       pattern = "csv") 
  # 
  # glob_liks <- map_df(glob_lik_files, 
  #                     function(x) {
  #                       read_csv(x) %>% 
  #                         mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
  #                                sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)"))) 
  #                     }) 
  # 
  # 
  # final_glob_liks <- glob_liks %>% group_by(sim) %>% arrange(desc(it)) %>% slice(1)
  # 
  # write_csv(glob_liks, outfile_globliks)
  
  lik_files <- dir(glue::glue("model_output/llik/{setup}/{scenario}/med/{runid}/"), 
                   full.names = T, 
                   recursive = T,
                   pattern = "parquet") %>% 
    .[str_detect(., "final")] 
  
  liks <- map_df(lik_files, 
                 function(x) {
                   arrow::read_parquet(x) %>% 
                     mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)")),
                            geoid = as.character(geoid)) 
                 }) 
  
  write_csv(liks, outfile_liks)
  
  outfile_all_liks <- glue::glue("{out_dir}/lliks_all_{setup}_{scenario}_{runid}.csv")
  
  
  if (do_int & (!file.exists(outfile_all_liks) | redo)) {
    # Check progression
    all_lik_files <- dir(glue::glue("model_output/llik/{setup}/{scenario}/med/{runid}/"), 
                         full.names = T, 
                         recursive = T,
                         pattern = "parquet") %>% 
      .[str_detect(., "intermediate")] %>% 
      .[str_detect(., "global")] %>% 
      .[str_detect(., "[0-9]+\\.[0-9]+\\.[0-9]+")]
    
    cl <- parallel::makeCluster(opt$jobs)
    doParallel::registerDoParallel(cl)
    
    all_liks <- foreach(files = ichunk(all_lik_files, round(length(all_lik_files)/16)),
                        .combine = rbind,
                        .inorder = F,
                        .packages = c("tidyverse", "glue", "arrow"),
                        .export = "runid") %dopar% 
      {
        map_df(files, 
               function(x) {
                 arrow::read_parquet(x) %>% 
                   mutate(it = as.numeric(str_extract(x, glue::glue("(?<=0)[0-9]+(?=\\.{runid})"))),
                          sim = as.numeric(str_extract(x, glue::glue("(?<=/0)[0-9]+(?=\\.)"))),
                          geoid = as.character(geoid))
               })
      }
    parallel::stopCluster(cl)
    
    write_csv(all_liks, path = outfile_all_liks)
  }
}

cat("---- DONE liks \n")
# SEIR params ------------------------------------------------------------------

# seir_files <- dir(glue::glue("model_output/spar//{setup}/{scenario}/med/{runid}/"), 
#                   full.names = T, 
#                   recursive = T,
#                   pattern = "parquet") %>% 
#   .[str_detect(., "final")] 
# 
# spars <- map_df(seir_files, 
#                 function(x) {
#                   arrow::read_parquet(x) %>% 
#                     mutate(sim = as.numeric(str_extract(x, "(?<=0)[0-9]+(?=\\.)"))) 
#                 }) 
# 
# cat("---- DONE seir pars \n")
