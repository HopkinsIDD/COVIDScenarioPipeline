import os
import datetime


def create_file_name(run_id, prefix, index, type, extension, create_directory=True):
    if create_directory:
        os.makedirs(create_dir_name(run_id, prefix, type), exist_ok=True)
    return "model_output/%s/%s%09d.%s.%s.%s" % (
        type,
        prefix,
        index,
        run_id,
        type,
        extension,
    )


def create_file_name_without_extension(
    run_id, prefix, index, type, create_directory=True
):
    if create_directory:
        os.makedirs(create_dir_name(run_id, prefix, type), exist_ok=True)
    return "model_output/%s/%s%09d.%s.%s" % (type, prefix, index, run_id, type)


def run_id():
    return datetime.datetime.strftime(datetime.datetime.now(), "%Y.%m.%d.%H:%M:%S.%Z")


def create_dir_name(run_id, prefix, type):
    return os.path.dirname(
        create_file_name_without_extension(
            run_id, prefix, 1, type, create_directory=False
        )
    )
