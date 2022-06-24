#' N.B.: Everything in this file will probably eventually go to gempyor, but for now it's here:
name_changing_functions <- list(
  mc_infection_stage = function(x){ return(x) },
  mc_vaccination_stage = function(x){ return(ifelse(grepl('previousinfection', x), "2dose", x)) },
  mc_variant_type = function(x){
    x <- ifelse(grepl("WILD", x), "DELTA", x)
    x <- ifelse(grepl("ALPHA", x), "DELTA", x)
    return(x)
  },
  mc_age_strata = function(x){ return(x) }
)

create_cont_file_from_seir <- function(cont_filename, seir_filename, config_start_date, name_changing_functions) {

  table <- arrow::read_parquet(seir_file)
  if (is.null(config_start_date)) {
    config_start_date <- min(table$date)
  }
  dplyr::filter(date == config_start_date, mc_value_type == 'prevalence')
  for(fun_name in names(name_changing_functions)) {
    table[[fun_name]] <- name_changing_functions[[fun_name]](table[[fun_name]])
  }
  table[["mc_name"]] <- apply(table[names(name_changing_functions)], 1, paste, collapse = "_")
  columns_to_group_by <- c(names(table)[grepl("^mc_", names(table))], "date")
  table %>%
    dplyr::group_by(!!!rlang::syms(columns_to_group_by)) %>%
    dplyr::summarize_all(~sum(.))
  arrow::write_write_parquet(x = table, sink = cont_filename)

  invisible(NULL)
}

create_cont_file_from_default <- function(cont_filename) {
  stop("Not supporting creating a continuation file except during a resume.")
}
