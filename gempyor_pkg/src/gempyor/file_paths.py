import os
import datetime


def create_file_name(run_id, prefix, index, ftype, extension, create_directory=True):
    if create_directory:
        os.makedirs(create_dir_name(run_id, prefix, type), exist_ok=True)
    return "model_output/%s/%s%09d.%s.%s.%s" % (
        ftype,
        prefix,
        index,
        run_id,
        ftype,
        extension,
    )

def create_file_name_without_extension(
    run_id, prefix, index, ftype, create_directory=True
):
    if create_directory:
        os.makedirs(create_dir_name(run_id, prefix, ftype), exist_ok=True)
    return "model_output/%s/%s%09d.%s.%s" % (ftype, prefix, index, run_id, ftype)


def run_id():
    return datetime.datetime.strftime(datetime.datetime.now(), "%Y.%m.%d.%H:%M:%S.%Z")


def create_dir_name(run_id, prefix, ftype):
    return os.path.dirname(
        create_file_name_without_extension(
            run_id, prefix, 1, ftype, create_directory=False
        )
    )
