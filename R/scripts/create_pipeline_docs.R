# This script is meant to run from the COVIDScenarioPipeline directory

# paths are relative to the created doc directory
pipeline_runners = list(
  "../R/scripts/create_filter.R",
  "../R/scripts/importation.R",
  "../R/scripts/hosp_run.R"
  )

dir.create("doc")
setwd("doc")
for (runner in pipeline_runners) {
  knitr::spin(runner)
}