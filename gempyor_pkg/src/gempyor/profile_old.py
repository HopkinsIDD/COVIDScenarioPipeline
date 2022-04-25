import cProfile
import io
import pstats
from functools import update_wrapper

import click


def profile_options(f):
    """Adds options to click methods for profiling the command"""

    @click.option(
        "--profile",
        is_flag=True,
        help="Enables profiling for the command.  By default prints to console sorted by cumulative time.",
    )
    @click.option(
        "--profile-output",
        type=click.Path(),
        help="Enables profiling for the command and writes the output to a file as well.",
    )
    @click.option(
        "--profile-sort-by",
        type=click.Choice(["tottime", "cumtime"]),
        default="tottime",
        show_default=True,
        help="Sort the profile stats by total time or cumulative time",
    )
    @click.option(
        "--profile-num-stats",
        type=int,
        default=200,
        show_default=True,
        help="The number of lines of profile stats to print",
    )
    @click.pass_context
    def new_func(ctx, *args, **kwargs):
        profile = kwargs.pop("profile")
        profile_output = kwargs.pop("profile_output")
        sortby = kwargs.pop("profile_sort_by")
        num_stats = kwargs.pop("profile_num_stats")
        profile = True if profile_output else profile
        if profile:
            pr = cProfile.Profile()
            pr.enable()
        result = ctx.invoke(f, *args, **kwargs)
        if profile:
            pr.disable()
            s = io.StringIO()
            ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
            ps.print_stats(num_stats)
            print(s.getvalue())
            if profile_output:
                ps.dump_stats(profile_output)
        return result

    return update_wrapper(new_func, f)
