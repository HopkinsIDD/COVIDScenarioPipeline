import gempyor
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import datetime
import glob, os, sys
from pathlib import Path

# import seaborn as sns
import matplotlib._color_data as mcd
import pyarrow.parquet as pq
import click

import dask.dataframe as dd
import matplotlib.dates as mdates
import matplotlib.cbook as cbook
from matplotlib.backends.backend_pdf import PdfPages

channelid_cspproduction = "C011YTUBJ7R"
channelid_chadi = "UFV770AE8"  # to debug
channelid_debug = "C04MAQWLEAW"


class RunInfo:
    def __init__(self, run_id, config_path=None, folder_path=None):
        self.run_id = run_id
        self.config_path = config_path
        self.folder_path = folder_path


def get_all_filenames(file_type, all_runs, finals_only=False, intermediates_only=False, ignore_chimeric=True) -> dict:
    """
    return dictionanary for each run name
    """
    if file_type == "seed":
        ext = "csv"
    else:

        ext = "parquet"
    files = {}
    for run_name, run_info in all_runs.items():
        l = []
        for f in Path(str(run_info.folder_path)).rglob(f"*.{ext}"):
            f = str(f)
            if file_type in f:
                if (
                    (finals_only and "final" in f)
                    or (intermediates_only and "intermediate" in f)
                    or (not finals_only and not intermediates_only)
                ):
                    if not (ignore_chimeric and "chimeric" in f):
                        l.append(str(f))
        files[run_name] = l
    return files


def slack_multiple_files_deprecated(slack_token, message, fileList, channel):
    import logging
    from slack_sdk import WebClient

    client = WebClient(slack_token)
    logging.basicConfig(level=logging.DEBUG)

    logging.basicConfig(level=logging.DEBUG)
    for file in fileList:
        upload = client.files_upload(file=file, filename=file)
        message = message + "<" + upload["file"]["permalink"] + "| >"
    outP = client.chat_postMessage(channel=channel, text=message)


def slack_multiple_files_v2(slack_token, message, fileList, channel):
    # file_uploads=[
    #    {
    #        "file": "pplot_llik_FCH_R3_highVE_pesImm_2022_Jan22_USA-20230130T163847_inference_med.pdf",
    #        "title": "Log-likelihood plot",
    #    },
    #    {
    #        "file": "slurm-11598936_237.out",
    #        "title": "random log file",
    #    },
    # ],

    import logging
    from slack_sdk import WebClient

    client = WebClient(slack_token)
    logging.basicConfig(level=logging.DEBUG)

    file_uploads = [{"file": f, "title": f.split["."][0]} for f in file_List]
    response = client.files_upload_v2(
        file_uploads=file_uploads,
        channel=channel,
        initial_comment=message,
    )


@click.command()
@click.option(
    "-c",
    "--config",
    "config_path",
    envvar="CONFIG_PATH",
    type=click.Path(exists=True),
    required=True,
    help="configuration file for this simulation",
)
@click.option(
    "-I",
    "--run-id",
    "run_id",
    envvar="COVID_RUN_INDEX",
    type=str,
    show_default=True,
    help="run index",
)
@click.option(
    "-j",
    "--job-name",
    "job_name",
    envvar="JOB_NAME",
    default="x",
    type=str,
    show_default=True,
    help="unique identifier for the run",
)
@click.option(  # slurm only option
    "-f",
    "--fs-results-path",
    "fs_results_path",
    envvar="FS_RESULTS_PATH",
    type=click.Path(exists=True),
    default=".",
    show_default=True,
    help="The file system folder to use load the simulations from",
)
@click.option(  # slurm only option
    "-s",
    "--slack-token",
    "slack_token",
    envvar="SLACK_TOKEN",
    type=str,
    help="Slack token",
)
@click.option(
    "-m",
    "--max_files",
    type=click.IntRange(min=1),
    default=90000,
    help="Maximum number of files to load for aggregate plot, e.g quantiles",
)
@click.option(
    "-M",
    "--max_files_deep",
    type=click.IntRange(min=1),
    default=30,
    help="Maximum number of files to load for in depth plot and individual sim plot",
)
def generate_pdf(config_path, run_id, job_name, fs_results_path, slack_token, max_files, max_files_deep):
    print("Generating plots")
    print(f">> config {config_path} for run_id {run_id}")
    print(f">> job name {job_name}, path {fs_results_path}")
    print(f">> max files (normal, deeep): {max_files}, {max_files_deep}")
    all_runs = {
        run_id: RunInfo(run_id, config_path),
    }

    # In[4]:

    for run_name, run_info in all_runs.items():
        run_id = run_info.run_id
        config_filepath = run_info.config_path
        run_info.gempyor_simulator = gempyor.InferenceSimulator(
            config_path=config_filepath,
            run_id=run_id,
            # prefix=f"USA/inference/med/{run_id}/global/intermediate/000000001.",
            first_sim_index=1,
            scenario="inference",  # NPIs scenario to use
            deathrate="med",  # Outcome scenario to use
            stoch_traj_flag=False,
            spatial_path_prefix="./",  # prefix where to find the folder indicated in spatial_setup$
        )
        run_info.folder_path = f"{fs_results_path}/model_output"

    node_names = run_info.gempyor_simulator.s.spatset.nodenames

    # In[5]:

    gempyor.config.set_file(run_info.config_path)
    gt = pd.read_csv(gempyor.config["filtering"]["data_path"].get())
    gt
    statistics = {}
    # Ingoring agreegation and all, assuming by week
    for stat in gempyor.config["filtering"]["statistics"]:
        statistics[gempyor.config["filtering"]["statistics"][stat]["sim_var"].get()] = gempyor.config["filtering"][
            "statistics"
        ][stat]["data_var"].get()
    statistics

    # ## Analyze llik files

    # In[6]:

    llik_filenames = get_all_filenames("llik", all_runs, intermediates_only=True)

    # In[7]:

    resultST = {}

    for run_name, run_info in all_runs.items():
        resultST[run_name] = []
        file_list = llik_filenames[run_name][:max_files]
        for filename in file_list:
            slot = int(filename.split("/")[-1].split(".")[0])
            block = int(filename.split("/")[-1].split(".")[1])
            sim_str = filename.split("/")[-1].split(".")[2]  # not necessarily a sim number now
            if sim_str.isdigit():
                sim = int(sim_str)
                if block == 1 and (sim == 1 or sim % 5 == 0):  ## first block, only one
                    df_raw = pq.read_table(filename).to_pandas()
                    df_raw["slot"] = slot
                    df_raw["sim"] = sim
                    df_raw["ID"] = run_name
                    df_raw = df_raw.drop("filename", axis=1)
                    # df_csv = df_csv.groupby(['slot','sim', 'ID', 'geoid']).sum().reset_index()
                    # df_csv = df_csv[['ll','sim', 'slot', 'ID','geoid']]
                    resultST[run_name].append(df_raw)
    full_df = pd.concat(resultST[run_name])
    full_df

    # In[22]:

    full_df.groupby(["sim", "slot"]).sum()

    # In[23]:

    fig, axes = plt.subplots(len(node_names) + 1, 4, figsize=(4 * 4, len(node_names) * 3), sharex=True)

    colors = ["b", "r", "y", "c"]
    icl = 0

    idp = 0
    all_nn = (
        full_df.groupby(["sim", "slot"])
        .sum()
        .reset_index()[["sim", "slot", "ll", "accept", "accept_avg", "accept_prob"]]
    )
    for ift, feature in enumerate(["ll", "accept", "accept_avg", "accept_prob"]):
        lls = all_nn.pivot(index="sim", columns="slot", values=feature)
        if feature == "accept":
            lls = lls.cumsum()
            feature = "accepts, cumulative"
        axes[idp, ift].fill_between(
            lls.index, lls.quantile(0.025, axis=1), lls.quantile(0.975, axis=1), alpha=0.1, color=colors[icl]
        )
        axes[idp, ift].fill_between(
            lls.index, lls.quantile(0.25, axis=1), lls.quantile(0.75, axis=1), alpha=0.1, color=colors[icl]
        )
        axes[idp, ift].plot(lls.index, lls.median(axis=1), marker="o", label=run_id, color=colors[icl])
        axes[idp, ift].plot(lls.index, lls.iloc[:, 0:max_files_deep], color="k", lw=0.3)
        axes[idp, ift].set_title(f"National, {feature}")
        axes[idp, ift].grid()

    for idp, nn in enumerate(node_names):
        idp = idp + 1
        all_nn = full_df[full_df["geoid"] == nn][["sim", "slot", "ll", "accept", "accept_avg", "accept_prob"]]
        for ift, feature in enumerate(["ll", "accept", "accept_avg", "accept_prob"]):
            lls = all_nn.pivot(index="sim", columns="slot", values=feature)
            if feature == "accept":
                lls = lls.cumsum()
                feature = "accepts, cumulative"
            axes[idp, ift].fill_between(
                lls.index, lls.quantile(0.025, axis=1), lls.quantile(0.975, axis=1), alpha=0.1, color=colors[icl]
            )
            axes[idp, ift].fill_between(
                lls.index, lls.quantile(0.25, axis=1), lls.quantile(0.75, axis=1), alpha=0.1, color=colors[icl]
            )
            axes[idp, ift].plot(lls.index, lls.median(axis=1), marker="o", label=run_id, color=colors[icl])
            axes[idp, ift].plot(lls.index, lls.iloc[:, 0:max_files_deep], color="k", lw=0.3)
            axes[idp, ift].set_title(f"{nn}, {feature}")
            axes[idp, ift].grid()
            if idp == len(node_names) - 1:
                axes[idp, ift].set_xlabel("sims")
            # ax.ticklabel_format(style='sci', scilimits=(-1,2), axis='y')
    fig.tight_layout()
    plt.savefig(f"pplot_llik_{run_id}_{job_name}.pdf")

    # In[9]:

    flist = []
    for f in Path(str(".")).rglob(f"./pplot*.pdf"):
        flist.append(str(f))


    channel = 

    # slack_multiple_files(
    #    slack_token=slack_token,
    #    message=f"FlepiMoP run `{run_id}` (job `{job_name}`) has successfully completed 🎉🤖. \n \nPlease find below a little analysis of the llik files, and I'll try to be more helpful in the future.",
    #    fileList=flist,
    #    channel=channelid_chadi,
    # )

    channel=channelid_debug

    slack_multiple_files_v2(
        slack_token=slack_token,
        message=f"FlepiMoP run `{run_id}` (job `{job_name}`) has successfully completed 🎉🤖. \n \nPlease find below a little analysis of the llik files, and I'll try to be more helpful in the future.",
        fileList=flist,
        channel=channelid_chadi,
    )


if __name__ == "__main__":
    generate_pdf()
