library(magrittr)
options(error=recover)
infection_stage_changer <- function(x){ return(x) }
vaccination_stage_changer <- function(x){ return(ifelse(grepl('previousinfection', x), "2dose", x)) }
variant_type_changer <- function(x){
  x <- ifelse(grepl("WILD", x), "DELTA", x)
  x <- ifelse(grepl("ALPHA", x), "DELTA", x)
  return(x)
}
age_strata_changer <- function(x){ return(x) }
seeding_file <- "~/tmp/seeding_territories.csv"
new_start <- lubridate::ymd("2022-03-20")
initial_conditions_source <- c(
  source_infection_stage = "S",
  source_vaccination_stage = "unvaccinated",
  source_variant_type = "DELTA",
  source_age_strata = "age0to17")
seeding <- readr::read_csv(seeding_file)
seeding <- seeding %>%
  dplyr::filter(date > new_start) %>%
  dplyr::mutate(
    source_infection_stage = infection_stage_changer(source_infection_stage),
    source_vaccination_stage = vaccination_stage_changer(source_vaccination_stage),
    source_variant_type = variant_type_changer(source_variant_type),
    source_age_strata = age_strata_changer(source_age_strata),
    destination_infection_stage = infection_stage_changer(destination_infection_stage),
    destination_vaccination_stage = vaccination_stage_changer(destination_vaccination_stage),
    destination_variant_type = variant_type_changer(destination_variant_type),
    destination_age_strata = age_strata_changer(destination_age_strata))
all_files <- list.files()[grepl('seir', list.files())]
for (file in all_files) {
  print(file)
  table <- arrow::read_parquet(file)
  table <- table %>%
    dplyr::filter(date == new_start, mc_value_type == 'prevalence') %>%
    dplyr::mutate(
      mc_infection_stage = infection_stage_changer(mc_infection_stage),
      mc_vaccination_stage = vaccination_stage_changer(mc_vaccination_stage),
      mc_variant_type = variant_type_changer(mc_variant_type),
      mc_age_strata = age_strata_changer(mc_age_strata),
    ) %>%
    dplyr::select(-mc_name, -mc_value_type) %>%
    dplyr::group_by(mc_infection_stage, mc_vaccination_stage, mc_variant_type, mc_age_strata, date) %>%
    dplyr::summarize_all(~sum(.))
  table <- table %>% tidyr::pivot_longer(names(table)[-1:-5], names_to = 'place', values_to = 'amount')
  names(table) <- gsub('mc_', 'destination_', names(table))
  for (col_name in names(initial_conditions_source)) {
    table[[col_name]] <- initial_conditions_source[[col_name]]
  }
  table[["no_perturb"]] <- TRUE
  table <- table[, names(seeding)] %>% dplyr::bind_rows(seeding)
  readr::write_csv(x = table, file = gsub('parquet', 'csv', gsub('seir', 'seed', file)))
}
