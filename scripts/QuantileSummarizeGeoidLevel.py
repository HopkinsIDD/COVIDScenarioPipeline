import click
import confuse
import multiprocessing
import os.path
import pathlib
import pickle

import numpy as np
import pandas as pd
import pyarrow.parquet as pq

from SEIR.utils import config
from SEIR.profile import profile_options

def load_hosp_sims(hosp_dir, deathrate, nsims, start_date, end_date):
    dfs = []
    for sim_id in range(1, nsims+1):
        sim_id_str = str(sim_id).zfill(9)
        file_basename = f"{hosp_dir}/{deathrate}_death-{sim_id_str}.hosp"
        if os.path.exists(f"{file_basename}.csv"):
            df = pd.read_csv(f"{file_basename}.csv", parse_dates=["time"])
        elif os.path.exists(f"{file_basename}.parquet"):
            df = pq.read_table(f"{file_basename}.parquet").to_pandas()
        else:
            raise Exception(f"Hospital simulation not found at {file_basename}.[parquet|csv]")

        # Per file filtering code
        # post_proc <- function(x,geodata,opt) {


        #   x%>%
        #     group_by(geoid) %>%
        #       mutate(cum_infections=cumsum(incidI)) %>%
        #       mutate(cum_death=cumsum(incidD)) %>%
        #       ungroup()%>%
        #       filter(time>=opt$start_date& time<=opt$end_date)%>%
        #       rename(infections=incidI, death=incidD, hosp=incidH)
        # }

        # grouped_df = df.groupby(["geoid"])
        # grouped_df["cum_infections"] = np.cumsum(df.incidI)
        # grouped_df["cum_death"] = np.cumsum(df.incidD)
        # print(grouped_df)

        df.rename(columns={"incidI": "infections", "incidD": "deaths", "incidH": "hosp"}, inplace=True)
        cum_df = df[["geoid","time","infections","deaths"]].groupby(['geoid', 'time']).sum().groupby(level=0).cumsum().reset_index()
        cum_df.rename(columns={"infections": "cum_infections", "deaths": "cum_deaths"}, inplace=True)
        df = df.join(cum_df[["cum_infections","cum_deaths"]])
        df = df[(df.time > start_date) & (df.time <= end_date)]
        df = df.assign(sim_num=sim_id)
        print(df)
        dfs.append(df)

    return pd.concat(dfs)
#   files <- dir(sprintf("hospitalization/model_output/%s", scenario_dir),full.names = TRUE)
#   files <- files[grepl(name_filter,gsub('^.*[/]','',files))]
#   if(length(files) == 0){stop(paste0("There were no files in ",getwd(),"/",sprintf("hospitalization/model_output/%s", scenario_dir)," matching name filter |",name_filter,"|"))}

#   if(is.null(num_files) | is.na(num_files) ){
#     num_files <- length(files)
#   }
#   if ( num_files <= length(files) ){
#     files <- files[seq_len(num_files)]
#     warning(paste("You are only reading in", num_files, "files. Check the num_files argument if this is unexpected."))
#   }

#   read_file <- read_file_of_type(file_extension)

#   rc<- foreach (i = 1:length(files)) %dopar% {
#     require(tidyverse)
#     file <- files[i]


#     read_file(files[i]) %>%
#       padfn %>%
#       post_process(...) %>%
#       mutate(sim_num = i)
#   }

#   rc<- dplyr::bind_rows(rc)

#   warning("Finished loading")
#   return(rc)

# }

@click.command()
@click.option("-c", "--config", "config_file", envvar="CONFIG_PATH", type=click.Path(exists=True), required=True,
              help="configuration file for this simulation")
@click.option("-d", "--name_filter", "deathrate", type=str, required=True,
              help="filename filter, usually deaths")
@click.option("-s", "--scenario", "scenarios", type=str, default=[], multiple=True,
              help="override the scenario(s) run for this simulation [supports multiple scenarios: `-s Wuhan -s None`]")
@click.option("-n", "--nsim", type=click.IntRange(min=1),
              help="override the # of simulation runs in the config file")
@click.option("-j", "--jobs", type=click.IntRange(min=1),
              default=multiprocessing.cpu_count(), show_default=True,
              help="the parallelization factor")
@click.option("-o", "--outfile", type=str, required=True)
@profile_options
def main (config_file, deathrate, scenarios, nsim, jobs, outfile):
    config.set_file(config_file)

    start_date = config["start_date"].as_date()
    end_date = config["end_date"].as_date()

    geodata_file = pathlib.Path(config["spatial_setup"]["base_path"].get()) / config["spatial_setup"]["geodata"].get()
    nodenames_key = config["spatial_setup"]["nodenames"]
    geodata = pd.read_csv(geodata_file, converters={nodenames_key: lambda x: str(x)}) # geoids and populations



    ## First parse the options to the scripts
    # suppressMessages({
    #     library(optparse, quietly=TRUE)
    #     library(tidyverse, quietly=TRUE)
    # })

    # ##List of specified options
    # option_list <- list(
    #     make_option("--name_filter", type="character", default="", help="filename filter, usually deaths"),
    #     make_option("--nfiles", type="numeric", default=NA, help="number of files to load, default is all"),
    #     make_option("--ncores", type="numeric", default=6, help="number of cores to use in data load, default =6"),
    #     make_option(c("--outfile","-o"), type="character", default=NULL, help="file to saver output"),
    #     make_option("--start_date", type="character", default="2020-01-01", help="earliest date to include"),
    #     make_option("--end_date",  type="character", default="2022-01-01", help="latest date to include")
    # )

    # opt_parser <- OptionParser(option_list = option_list, usage="%prog [options] [one or more scenarios]")

    # ## Paerse the
    # arguments <- parse_args(opt_parser, positional_arguments=c(1,Inf))
    # opt <- arguments$options
    # scenarios <- arguments$args

    # if(is.null(opt$outfile)) {
    #     stop("outfile must be specified")
    # }

    ## Register the parallel backend
    # doParallel::registerDoParallel(opt$ncores)

    ##Run over scenarios and death rates as appropriate. Note that
    ##Final results will average accross whatever is included
    # res_geoid <-list()
    setup_name = config["spatial_setup"]["setup_name"]
    dfs = []
    for scenario in scenarios:
        hosp_dir = f"hospitalization/model_output/{setup_name}_{scenario}"
        df = load_hosp_sims(hosp_dir, deathrate, nsim, start_date, end_date)
        df = df.assign(scenario=scenario)
        dfs.append(df)

    res_geoid = pd.concat(dfs)
    print(res_geoid)

    to_save_geo = inner_join()

# ##deregister backend
# doParallel::stopImplicitCluster()



# ## Extract quantiles
# tmp_col <- function(x, col) {
#     x%>%
#         group_by(time,geoid) %>%
#         summarize(x=list(enframe(quantile(!!sym(col), probs=c(0.01, 0.025,
#                                                               seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
#                                  "quantile",col))) %>%
#         unnest(x)
# }


# to_save_geo <- inner_join(tmp_col(res_geoid,"hosp_curr"),
#                          tmp_col(res_geoid,"cum_death"))%>%
#     inner_join(tmp_col(res_geoid,"death"))%>%
#     inner_join(tmp_col(res_geoid,"infections"))%>%
#     inner_join(tmp_col(res_geoid,"cum_infections"))%>%
#     inner_join(tmp_col(res_geoid,"hosp"))

    to_save_geo.to_csv(outfile)


if __name__ == "__main__":
    main()



