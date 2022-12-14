import numpy as np
import pandas as pd
import sys, os, click, glob, tqdm, shutil


@click.command()
@click.option(
    "-u",
    "--run_id",
    "run_id",
    required=True,
    envvar="COVID_RUN_INDEX",
    help="run_id_to convert from",
)
@click.option(
    "-f",
    "--input_folder",
    "input_folder",
    required=True,
    help="Folder where the initial run is synced. There should be a model_ouput in it",
)
def cli(run_id, input_folder):
    return run_id, input_folder


# https://stackoverflow.com/questions/2793789/create-destination-path-for-shutil-copy-files
def copy_path(*, src, dst, dir_mode=0o777, follow_symlinks: bool = True):
    """
    Copy a source filesystem path to a destination path, creating parent
    directories if they don't exist.

    Args:
        src: The source filesystem path to copy. This must exist on the
            filesystem.

        dst: The destination to copy to. If the parent directories for this
            path do not exist, we will create them.

        dir_mode: The Unix permissions to set for any newly created
            directories.

        follow_symlinks: Whether to follow symlinks during the copy.

    Returns:
        Returns the destination path.
    """
    try:
        return shutil.copy2(src=src, dst=dst, follow_symlinks=follow_symlinks)
    except FileNotFoundError as exc:
        if exc.filename == dst and exc.filename2 is None:
            parent = os.path.dirname(dst)
            os.makedirs(name=parent, mode=dir_mode, exist_ok=True)
            return shutil.copy2(
                src=src,
                dst=dst,
                follow_symlinks=follow_symlinks,
            )
        raise


def detect_old_run_id(fp):
    return fp.split("/inference/med/")[-1].split("/")[0]


if __name__ == "__main__":
    # standalone_mode: so click doesn't exit, see
    # https://stackoverflow.com/questions/60319832/how-to-continue-execution-of-python-script-after-evaluating-a-click-cli-function
    run_id, input_folder = cli(standalone_mode=False)
    print(run_id, input_folder)
    if not os.path.isdir(f"{input_folder}/model_output"):
        raise ValueError(
            f"Could not find folder {input_folder}/model_output, are you sure -f option is set correctly ?"
        )
    files = glob.glob(input_folder + "/**/*.parquet", recursive=True)
    os.makedirs("model_output/cont", exist_ok=True)

    fn = files[0]
    old_run_id = detect_old_run_id(fn)
    new_name = (
        fn.replace("seir", "cont").replace(f"{input_folder}/model_output", "model_output").replace(old_run_id, run_id)
    )

    print(f"detected old_run_id: {old_run_id} which will be replaced by user provided run_id: {run_id}")
    empty_str = "°" * len(input_folder)
    print(f"file: \n OLD NAME: {fn}\n NEW NAME: {empty_str}{new_name}")
    for fn in tqdm.tqdm(files):
        old_run_id = detect_old_run_id(fn)
        new_name = (
            fn.replace("seir", "cont")
            .replace(f"{input_folder}/model_output", "model_output")
            .replace(old_run_id, run_id)
        )
        copy_path(src=fn, dst=new_name)

    # os.makedirs(outdir, exist_ok=True)
