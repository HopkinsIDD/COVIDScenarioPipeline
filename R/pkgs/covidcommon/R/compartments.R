parse_compartment_names <- function(seir_config) {
  compartment_frame <- tidyr::expand_grid(!!!seir_config$compartments)
  compartment_names <- character(0)
  for (component in compartment_frame) {
    if (any(grepl("_", component, fixed = TRUE))) {
      stop(paste("_", "is a reserved character, and cannot appear in compartment component names"))
    }
    compartment_names <- paste(compartment_names, component, sep = "_")
  }
  compartment_names <- stringr::str_sub(compartment_names, 2)
  return(compartment_frame)
}

assert <- function(bool, msg) {
  if (!bool) {
    stop(paste("Assertion failed:", msg))
  }
}

check_transition_element <- function(
  config_piece,
  compartment_names,
  problem_dimension_length = ncol(compartment_names),
  problem_dimension = NULL,
  allow_combinations = FALSE,
  allow_extra_categories = FALSE
) {

  if (!is.null(problem_dimension)) {
    if (length(problem_dimension) != problem_dimension_length) {
      stop(paste(
        "Problem dimension passed in as",
        "(",
        paste(problem_dimension, collapse = ", "),
        ")",
        "with length",
        length(problem_dimension),
        "but should have length",
        problem_dimension_length,
        "based on number of compartments"
      ))
    }
  }
  if (!(length(config_piece) %in% c(1, problem_dimension_length))) {
    stop(paste(
      "config piece",
      "{",
      paste(unlist(sapply(config_piece, paste, collapse = ", ")), collapse = "|"),
      "}",
      "has incorrect number of categories:",
      "found",
      length(config_piece),
      "expected",
      problem_dimension_length
    ))
  }
  for (dimension in seq_len(problem_dimension_length)) {
    if ((!allow_combinations) & (!all(config_piece[[dimension]] %in% compartment_names[[dimension]]))) {
      stop(paste(
        "config piece",
        "{",
        paste(unlist(sapply(config_piece, paste, collapse = ", ")), collapse = "|"),
        "}",
        "has incorrect category(ies):",
        "found",
        config_piece[[dimension]][!(config_piece[[dimension]] %in% compartment_names[[dimension]])],
        "expected to be one of",
        paste(unique(compartment_names[[dimension]]), collapse = ", ")
      ))
    }
    if (allow_combinations) {
      current <- config_piece[[dimension]]
      for (piece in compartment_names[[dimension]]) {
        current <- gsub(piece, "", fixed = TRUE, current)
      }
      if (!all(grepl("^[1234567890*+^ ]*$", current))) {
        stop(paste(
          "config piece",
          "{",
          paste(unlist(sapply(config_piece, paste, collapse = ", ")), collapse = "|"),
          "}",
          "has incorrect category(ies):",
          "found",
          config_piece[[dimension]][!(config_piece[[dimension]] %in% compartment_names[[dimension]])],
          "expected to be composed of arithmatic operations on numbers and elements of",
          paste(unique(compartment_names[[dimension]]), collapse = ", ")
        ))
      }
    }
    if (!is.null(problem_dimension)) {
      if (((!allow_extra_categories) || (problem_dimension[[dimension]] > 1)) & (!(length(config_piece[[dimension]]) %in% c(1, problem_dimension[[dimension]])))) {
        stop(paste(
          "config piece",
          "{",
          paste(unlist(sapply(config_piece, paste, collapse = ", ")), collapse = "|"),
          "}",
          "has incorrect category(ies):",
          "found",
          config_piece[[dimension]],
          "with length",
          length(config_piece[[dimension]]),
          "expected to be either 1 or ",
          problem_dimension[[dimension]]
        ))
      }
    }
  }
}

check_transition_elements <- function(config_piece, compartment_names, parameter_names, problem_dimension) {
  parameter_names <- lapply(
    problem_dimension,
    function(x) {
      return(parameter_names)
    }
  )
  check_transition_element(
    config_piece[["destination"]],
    compartment_names = compartment_names,
    problem_dimension = problem_dimension
  )
  check_transition_element(
    config_piece[["destination"]],
    compartment_names = compartment_names,
    problem_dimension = problem_dimension
  )
  for (elem in config_piece[["proportion_exponent"]]) {
    check_transition_element(
      elem,
      compartment_names = parameter_names,
      problem_dimension_length = ncol(compartment_names),
      problem_dimension = problem_dimension,
      allow_combinations = TRUE
    )
  }
  for (elem in config_piece[["proportional_to"]]) {
    if (!(identical(elem, "source"))) {
      check_transition_element(
        elem,
        compartment_names = compartment_names,
        problem_dimension = problem_dimension,
        allow_extra_categories = TRUE
      )
    }
  }
  check_transition_element(
    config_piece[["rate"]],
    compartment_names = parameter_names,
    problem_dimension_length = ncol(compartment_names),
    problem_dimension = problem_dimension,
    allow_combinations = TRUE
  )
}

expand_transition_elements <- function(config_piece, problem_dimension) {

  for (proportion_idx in seq_len(length(config_piece[["proportional_to"]]))) {
    if (any(class(config_piece[["proportional_to"]][[proportion_idx]]) != "list")) {
      config_piece[["proportional_to"]][[proportion_idx]] <-
        as.list(config_piece[["proportional_to"]][[proportion_idx]])
    }
    if (identical(list("source"), config_piece[["proportional_to"]][[proportion_idx]])) {
      config_piece[["proportional_to"]][[proportion_idx]] <- config_piece[["source"]]
    }
    if (any(class(config_piece[["proportion_exponent"]][[proportion_idx]]) != "list")) {
      config_piece[["proportion_exponent"]][[proportion_idx]] <-
        as.list(config_piece[["proportion_exponent"]][[proportion_idx]])
    }
  }
  rm(proportion_idx)
  if (any(class(config_piece[["source"]]) != "list")) {
    config_piece[["source"]] <- as.list(config_piece[["source"]])
  }
  if (any(class(config_piece[["destination"]]) != "list")) {
    config_piece[["destination"]] <- as.list(config_piece[["destination"]])
  }
  if (any(class(config_piece[["rate"]]) != "list")) {
    config_piece[["rate"]] <- as.list(config_piece[["rate"]])
  }
  for (dimension_idx in seq_len(length(problem_dimension))) {
    if (length(config_piece[["source"]][[dimension_idx]]) == 1) {
      config_piece[["source"]][[dimension_idx]] <-
        rep(times = problem_dimension[[dimension_idx]], config_piece[["source"]][[dimension_idx]])
    }
    if (length(config_piece[["destination"]][[dimension_idx]]) == 1) {
      config_piece[["destination"]][[dimension_idx]] <-
        rep(times = problem_dimension[[dimension_idx]], config_piece[["destination"]][[dimension_idx]])
    }
    if (length(config_piece[["rate"]][[dimension_idx]]) == 1) {
      config_piece[["rate"]][[dimension_idx]] <-
        rep(times = problem_dimension[[dimension_idx]], config_piece[["rate"]][[dimension_idx]])
    }
    for (proportion_idx in seq_len(length(config_piece[["proportional_to"]]))) {
      if (
      (problem_dimension[[dimension_idx]] > 1) &
        (length(config_piece[["proportional_to"]][[proportion_idx]][[dimension_idx]]) == 1)
      ) {
        config_piece[["proportional_to"]][[proportion_idx]][[dimension_idx]] <-
          rep(
            times = problem_dimension[[dimension_idx]],
            config_piece[["proportional_to"]][[proportion_idx]][[dimension_idx]]
          )
      }
      if (length(config_piece[["proportion_exponent"]][[proportion_idx]][[dimension_idx]]) == 1) {
        config_piece[["proportion_exponent"]][[proportion_idx]][[dimension_idx]] <-
          rep(
            times = problem_dimension[[dimension_idx]],
            config_piece[["proportion_exponent"]][[proportion_idx]][[dimension_idx]]
          )
        }
    }
  }
  return(config_piece)
}

parse_transition_name <- function(seir_config, single_transition_config) {

  compartment_names <- parse_compartment_names(seir_config)
  parameter_names <- names(seir_config$parameters)

  check_transition_element(single_transition_config$source, compartment_names)
  check_transition_element(single_transition_config$destination, compartment_names)
  source_dimension <- sapply(single_transition_config$source, length)
  destination_dimension <- sapply(single_transition_config$destination, length)
  problem_dimension <- pmax(
    source_dimension,
    destination_dimension
  )
  # problem_dimension_length <- ncol(compartment_names)
  check_transition_elements(
    single_transition_config,
    compartment_names = compartment_names,
    parameter_names = parameter_names,
    problem_dimension = problem_dimension
  )

  single_transition_config <- expand_transition_elements(single_transition_config, problem_dimension)

  all_data <- list()
  for (i in seq_len(prod(problem_dimension))) {
    array_index <- arrayInd(i, problem_dimension)
    source_compartment <- mapply(
      s = single_transition_config[["source"]],
      i = array_index,
      function(s, i) {
        return(s[[i]])
      }
    )
    destination_compartment <- mapply(
      s = single_transition_config[["destination"]],
      i = array_index,
      function(s, i) {
        return(s[[i]])
      }
    )
    rate <- mapply(
      s = single_transition_config[["rate"]],
      i = array_index,
      function(s, i) {
        return(s[[i]])
      }
    )
    proportion_exponent <- lapply(
      single_transition_config[["proportion_exponent"]],
      function(x) {
        mapply(
          s = x,
          i = array_index,
          d = problem_dimension,
          function(s, d, i) {
            if (d == 1) {
              return(s)
            }
            return(s[[i]])
          }
        )
      }
    )
    proportional_to <- lapply(
      single_transition_config[["proportional_to"]],
      function(x) {
        mapply(
          s = x,
          i = array_index,
          d = problem_dimension,
          function(s, d, i) {
            if (d == 1) {
              return(s)
            }
            return(s[[i]])
          }
        )
      }
    )
    all_data[[i]] <- tibble::tibble(
      source = format_source(setNames(source_compartment, paste("source", names(compartment_names), sep = "."))),
      destination = format_destination(
        setNames(destination_compartment, paste("destination", names(compartment_names), sep = "."))
      ),
      rate = paste(format_rate(rate), collapse = " * "),
      proportional_to = paste(format_proportional_to(proportional_to), collapse = "*"),
      proportion_exponent = paste(format_proportion_exponent(proportion_exponent), collapse = "*")
    )

  }
  all_data <- do.call(what = rbind, all_data)
  return(all_data)
}

parse_all_transitions <- function(seir_config) {
  return(do.call(what = rbind, lapply(seir_config$transitions, parse_transition_name, seir_config = seir_config)))
}

format_proportional_to <- function(proportional_to) {
  rc <- paste(
    sapply(
      proportional_to,
      function(y) {
        paste(
          sapply(
            y,
            function(z) {
              paste0(
                ifelse(length(z) == 1, "", "("),
                paste(z, collapse = '+'),
                ifelse(length(z)==1, "", ")")
              )
            }
          ),
          collapse = "_"
        )
      }
    )
  )
  rc <- sapply(rc, gsub, pattern = "(.)[* 1]*$", replacement = "\\1")
}

format_proportion_exponent <- function(proportion_exponent) {
  rc <- sapply(proportion_exponent, paste, collapse = "*")
  rc <- sapply(rc, gsub, pattern = "(.)[* 1]*$", replacement = "\\1")
  return(rc)
}

format_source <- function(source) {
  return(paste(source, collapse = "_"))
}

format_destination <- function(destination) {
  return(paste(destination, collapse = "_"))
}

format_rate <- function(rate) {
  rc <- paste(rate, collapse = " * ")
  rc <- sapply(rc, gsub, pattern = "([^_])[* 1]*$", replacement = "\\1")
  rc <- unname(sapply(rc, gsub, pattern = "([^_ ]) *[*] *1 *[*] *([^ _])", replacement = "\\1 * \\2"))
  return(rc)
}

plot_graph <- function(config) {
  node_df <- parse_compartment_names(config$seir)
  node_df$name <- apply(node_df, 1, format_source)
  edge_df <- parse_all_transitions(config$seir)
  edge_df$from <- edge_df$source # apply(edge_df[, grepl("^source", names(edge_df))], 1, paste, collapse = "_")
  edge_df$to <- edge_df$destination # apply(edge_df[, grepl("^destination", names(edge_df))], 1, paste, collapse = "_")
  g <- tidygraph::tbl_graph(node_df, edge_df)
  plt <- g %>%
    ggraph::ggraph() +
    ggraph::geom_node_label(ggplot2::aes(label = name), size = 4 / ggplot2::.pt) +
    ggraph::geom_edge_link(
      ggplot2::aes(
        label = paste0(
          rate, "*", "\n",
          mapply(
            pt = proportional_to,
            pe = proportion_exponent,
            function(pt, pe) {
              paste(
                "(", pt, ")",
                sapply(
                  pe,
                  function(x) {
                    ifelse(identical(x, "1"), "", paste("^", "(", x, ")"))
                  }
                ),
                collapse = "*\n"
              )
            }
          )
        ),
        label_size = 2 / ggplot2::.pt
      ),
      arrow = grid::arrow()
    )

  return(plt)
}
