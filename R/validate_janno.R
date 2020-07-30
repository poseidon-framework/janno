validate_janno <- function(input_janno) {
  cli::cli_alert_info(input_janno)
  # does it exist?
  if ( !checkmate::test_file_exists(input_janno, access = "r", extension = "janno") ) {
    cli::cli_alert_danger("The janno file does not exist")
    return(1)
  }
  # does it contain tab separated columns?
  if ( !is_tab_separated_file(input_janno) ) {
    return(1)
  }
  # read file
  character_janno <- readr::read_tsv(input_janno, col_types = readr::cols(.default = "c"))
  # are the necessary columns present?
  if (!has_necessary_columns(character_janno)) {
    return(1)
  }
  # from here onwards check conditions become less mandatory
  everything_fine_flag <- TRUE
  # check values in janno file
  # loop through each column
  for (cur_col in colnames(character_janno)) {
    # get column background information
    expected_type <- hash::values(janno_column_name_column_type, cur_col)
    check_function <- type_string_to_check_function(expected_type)
    mandatory <- cur_col %in% janno_mandatory_columns
    no_dupli <- cur_col %in% janno_unique_columns
    with_choices <- cur_col %in% janno_choice_columns
    if (with_choices) {
      expected_choices <- unlist(strsplit(
        hash::values(janno_column_name_choices, cur_col), 
        ";"
      ))
    }
    with_range <- cur_col %in% janno_range_columns
    if (with_range) {
      expected_range <- c(
        hash::values(janno_column_name_range_lower, cur_col),
        hash::values(janno_column_name_range_upper, cur_col)
      )
    }
    # column wise checks
    if (no_dupli) {
      if ( !no_duplicates(character_janno, cur_col) ) {
        everything_fine_flag <- FALSE
      }
    }
    # loop through each cell: cell wise checks
    for (cur_row in 1:nrow(character_janno)) {
      cur_cell <- character_janno[[cur_col]][cur_row]
      ## general checks ##
      if ( !positioned_feedback(cur_cell, is_not_empty, position_in_table_string(cur_col, cur_row)) ) {
        everything_fine_flag <- FALSE
        next
        # special case: n/a
      } else if (is_n_a(cur_cell)) {
        if (mandatory) {
          cli::cli_alert_danger("n/a in a mandatory column")
          cat("  in", position_in_table_string(cur_col, cur_row), "\n")
          everything_fine_flag <- FALSE
          next
        } else {
          next
        }
      }
      ## column type checks ##
      # with defined set of choices
      if (with_choices) {
        if ( 
          !positioned_feedback(
            cur_cell, check_function, 
            position_in_table_string(cur_col, cur_row), 
            choices = expected_choices
          )
        ) {
          everything_fine_flag <- FALSE
        }
        # with range
      } else if (with_range) {
        if ( 
          !positioned_feedback(
            cur_cell, check_function, 
            position_in_table_string(cur_col, cur_row), 
            expected_range = expected_range
          ) 
        ) {
          everything_fine_flag <- FALSE
        }
        # without anything
      } else {
        if ( 
          !positioned_feedback(
            cur_cell, check_function, 
            position_in_table_string(cur_col, cur_row)
          ) 
        ) {
          everything_fine_flag <- FALSE
        }
      }
    }
  }
  # final output
  if ( everything_fine_flag ) {
    return(0)
  } else {
    return(2)
  }
}

is_n_a <- function(x) {
  x == "n/a"
}

position_in_table_string <- function(cur_col, cur_row) {
  paste0("[", cur_row, " | ", cur_col, "]")
}

positioned_feedback <- function(x, check_function, position_string, ...) {
  check_result <- check_function(x, ...)
  if ( !check_result ) {
    cat("  in ")
    cat(position_string)
    cat(": ")
    cat(x)
    cat("\n")
  }
  return(check_result)
}

is_not_empty <- function(x) {
  check <- !(is.na(x) | x == "")
  if ( !check ) {
    cli::cli_alert_danger("Empty cells are not allowed, please fill with n/a")
  }
  return(check)
}

no_duplicates <- function(x, column) {
  check <- length(unique(x[[column]])) == length(x[[column]])
  if ( !check ) {
    cli::cli_alert_danger(paste(
      "Duplicates are not allowed in column", column
    ))
  }
  return(check)
}

has_necessary_columns <- function(x, columns = janno_column_names) {
  check <- all(columns %in% colnames(x))
  if ( !check ) {
    cli::cli_alert_danger(paste(
      "The janno file lacks the following columns: ", 
      paste(columns[!(columns %in% colnames(x))], collapse = ", ")
    ))
  }
  return(check)
}

is_tab_separated_file <- function(x) {
  input_janno_linewise <- readr::read_lines(x, n_max = 50)
  check <- all(grepl(".*\\t.*", input_janno_linewise))
  if ( !check ) {
    cli::cli_alert_danger("The janno file is not a valid tab separated file with")
  }
  return(check)
}

type_string_to_check_function <- function(x) {
  switch(
    x,
    "String" = is_valid_string,
    "String choice" = is_valid_string_choice,
    "String list" = is_valid_string_list,
    "Char choice" = is_valid_string_choice,
    "Integer" = is_valid_integer, 
    "Integer list" = is_valid_integer_list,
    "Float" = is_valid_float,
    NA
  )
}

is_valid_string <- function(x) {
  check <- checkmate::test_string(x, min.chars = 1)
  if ( !check ) {
    cli::cli_alert_danger("Not a valid string")
  }
  return(check)
}

is_valid_string_choice <- function(x, choices) {
  check <- checkmate::test_choice(x, choices)
  if ( !check ) {
    cli::cli_alert_danger(paste("Value not in", paste(choices, collapse = ", ")))
  }
  return(check)
}

is_valid_string_list <- function(x) {
  check_0 <- is_valid_string(x)
  if ( check_0 ) {
    check_1 <- !grepl(".*;+?\\s+.*|.*\\s+;+?.*", x)
    if( !check_1 ) {
      cli::cli_alert_danger("Superfluous white space around separator ;")
    }
    return(check_1)
  }
  return(check_0)
}

is_valid_integer <- function(x, expected_range = c(-Inf, Inf)) {
  check_0 <- grepl("^[0-9-]+$", x) && !grepl("\\.", x) && !is.na(suppressWarnings(as.integer(x)))
  if ( !check_0 ) {
    cli::cli_alert_danger("Value not a valid integer number")
  } else {
    check_1 <- checkmate::test_integer(
      as.integer(x), lower = expected_range[1], upper = expected_range[2]
    )
    if ( !check_1 ) {
      cli::cli_alert_danger(
        paste("Value not in range", expected_range[1], "to", expected_range[2])
      )
      return(check_1)
    }
  }
  return(check_0)
}

is_valid_integer_list <- function(x, expected_range = c(-Inf, Inf)) {
  supposed_integers <- unlist(strsplit(x, split = ";"))
  check_0 <- all(grepl("^[0-9-]+$", supposed_integers)) &&
    all(!grepl("\\.", supposed_integers)) &&
    !any(is.na(suppressWarnings(as.integer(supposed_integers))))
  if ( !check_0 ) {
    cli::cli_alert_danger("One or multiple values not valid integer numbers")
  } else {
    check_1 <- checkmate::test_integer(
      as.integer(supposed_integers), lower = expected_range[1], upper = expected_range[2]
    )
    if ( !check_1 ) {
      cli::cli_alert_danger(
        paste("One or multiple values not in range", expected_range[1], "to", expected_range[2])
      )
      return(check_1)
    }
  }
  return(check_0)
}

is_valid_float <- function(x, expected_range = c(-Inf, Inf)) {
  check_0 <- grepl("^[0-9\\.-]+$", x) && !is.na(suppressWarnings(as.double(x)))
  if ( !check_0 ) {
    cli::cli_alert_danger("Value not a valid double number")
  } else {
    check_1 <- checkmate::test_numeric(
      as.double(x), lower = expected_range[1], upper = expected_range[2]
    )
    if ( !check_1 ) {
      cli::cli_alert_danger(paste("Value not in range", expected_range[1], "to", expected_range[2]))
      return(check_1)
    }
  }
  return(check_0)
}
