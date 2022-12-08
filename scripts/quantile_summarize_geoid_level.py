#!/opt/spark/bin/spark-submit
"""
A faster and distributed version of QuantileSummarizeGeoidLevel.R, which uses Apache Spark as its execution engine

This script can be executed from the command line in the container by running Apache Spark locally:
% /opt/spark/bin/spark-submit --driver-memory 1g --executor-memory 8g \
        quantile_summarize_geoid_level.py USA_None -c config.yml -o usa_none.csv
"""

import itertools
import pathlib
import re

import click
import confuse
import numpy as np
from pyspark.sql import functions as F, SparkSession, SQLContext, Window

PROBS = np.concatenate([[0.01, 0.025], np.arange(start=0.05, stop=0.95, step=0.05), [0.975, 0.99]])
METRICS = ["hosp_curr", "cum_death", "death", "infections", "cum_infections", "hosp"]

spark = SparkSession.builder.appName("quantile report").getOrCreate()
sc = spark.sparkContext
sc.setLogLevel("WARN")
sqlContext = SQLContext(sc)


@click.command()
@click.option(
    "-c",
    "--config",
    "config_file",
    envvar="CONFIG_PATH",
    type=click.Path(exists=True),
    required=True,
    help="configuration file for this simulation",
)
@click.option(
    "-d",
    "--name_filter",
    type=str,
    default=".*",
    metavar="REGEX",
    help="only process files matching this filter",
)
@click.option(
    "-o",
    "--output",
    type=pathlib.Path,
    required=True,
    help="write output to this directory",
)
@click.option(
    "--start_date",
    type=click.DateTime(formats=["%Y-%m-%d"]),
    default="2020-01-01",
    show_default=True,
    help="earliest date to include",
)
@click.option(
    "--end_date",
    type=click.DateTime(formats=["%Y-%m-%d"]),
    default="2022-01-01",
    show_default=True,
    help="latest date to include",
)
@click.argument("scenarios", type=str, nargs=-1, required=True)
def process(config_file, scenarios, output, start_date, end_date, name_filter):
    config = confuse.Configuration("COVIDScenarioPipeline")
    config.set_file(config_file)

    input_paths = (f"{config['spatial_setup']['setup_name'].get()}_{scenario}" for scenario in scenarios)
    paths = itertools.chain(
        *(pathlib.Path("hospitalization/model_output").glob(p + "/**/*.parquet") for p in input_paths)
    )
    paths = (str(p) for p in paths if p.is_file())
    paths = filter(lambda p: re.search(name_filter, p), paths)
    paths = list(paths)
    if not paths:
        raise click.BadParameter("no files found in input path")

    df = sqlContext.read.parquet(*paths)
    df = (
        df.withColumnRenamed("incidI", "infections")
        .withColumnRenamed("incidD", "death")
        .withColumnRenamed("incidH", "hosp")
    )
    df = df.filter((df.time > start_date.date()) & (df.time <= end_date.date()))
    df = df.withColumn(
        "cum_infections",
        F.sum(df.infections).over(Window.partitionBy(df.geoid).orderBy(df.time, df.uid)),
    )
    df = df.withColumn(
        "cum_death",
        F.sum(df.death).over(Window.partitionBy(df.geoid).orderBy(df.time, df.uid)),
    )

    # construct gnarly spark sql statement to compute quantiles via percentile_approx()
    df.registerTempTable("df")
    metric_probs = [
        (metric, prob, f"{metric}__{str(round(prob, 3)).replace('.', '_')}")
        for metric, prob in itertools.product(METRICS, PROBS)
    ]
    agg_sql = ", ".join(f"percentile_approx({metric}, {prob}, 100) AS {name}" for metric, prob, name in metric_probs)
    rollup_df = sqlContext.sql(
        f"""\
SELECT geoid, time, {agg_sql} FROM df
GROUP BY geoid, time
"""
    )

    # melt dataframe from column-wise to row-wise
    exprs = [
        F.struct(
            F.lit(metric).alias("metric"),
            F.lit(str(prob)).alias("quantile"),
            F.col(name).alias("value"),
        )
        for metric, prob, name in metric_probs
    ]
    final_df = (
        rollup_df.select("geoid", "time", F.explode(F.array(exprs)).alias("q"))
        .select(
            "geoid",
            "time",
            F.col("q.quantile").alias("quantile"),
            F.col("q.metric").alias("metric"),
            F.col("q.value").alias("value"),
        )
        .groupBy(["geoid", "time", "quantile"])
        .pivot("metric")
        .sum("value")
    )

    final_df.write.option("header", "true").csv(str(output))


process()
