validate_janno <- function(input_janno) {
  cli::cli_alert_info(basename(input_janno))
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
    # get basic column background information
    expected_type <- hash::values(janno_column_name_data_type, cur_col)
    multi <- cur_col %in% janno_multi_value_columns
    mandatory <- cur_col %in% janno_mandatory_columns
    no_dupli <- cur_col %in% janno_unique_columns
    with_choices <- cur_col %in% janno_choice_columns
    with_range <- cur_col %in% janno_range_columns
    bonus <- cur_col %in% janno_bonus_columns
    # get derived column background information
    type_check_function <- type_string_to_type_check_function(expected_type)
    if (with_choices) {
      expected_choices <- unlist(strsplit(
        hash::values(janno_column_name_choices, cur_col), 
        ";"
      ))
    } else {
      expected_choices <- NA
    }
    if (with_range) {
      expected_range <- c(
        hash::values(janno_column_name_range_lower, cur_col),
        hash::values(janno_column_name_range_upper, cur_col)
      )
    } else {
      expected_range <- c(-Inf, Inf)
    }
    # column wise checks
    if (!bonus & all(character_janno[[cur_col]] == "n/a")) {
      cli::cli_alert_warning(paste0(
        "Only n/a values in Column [", 
        cur_col, 
        "]"
      ))
    }
    if (no_dupli) {
      if ( !no_duplicates(character_janno, cur_col) ) {
        everything_fine_flag <- FALSE
      }
    }
    # cell wise checks: loop through each cell
    for (cur_row in 1:nrow(character_janno)) {
      #print(paste(cur_col, cur_row))
      cur_cell <- character_janno[[cur_col]][cur_row]
      # general cell wise checks
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
      # leading or trailing whitespace
      positioned_feedback(
        cur_cell, 
        has_no_leading_or_trailing_whitespace, 
        position_in_table_string(cur_col, cur_row)
      )
      # specific column type checks
      if ( 
        !positioned_feedback(
          cur_cell, type_check_function,
          position_in_table_string(cur_col, cur_row),
          multi = multi,
          expected_choices = expected_choices,
          expected_range = expected_range
        )
      ) {
        everything_fine_flag <- FALSE
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

has_no_leading_or_trailing_whitespace <- function(x) {
  check <- any(grepl("(*UCP)^\\s+", x, perl = T) | grepl("(*UCP)\\s+$", x, perl = T))
  if ( check ) {
    cli::cli_alert_warning("Cell has leading or trailing whitespaces")
  }
  return(!check)
}

is_n_a <- function(x) {
  x == "n/a"
}

position_in_table_string <- function(cur_col, cur_row) {
  paste0("[", cur_row, " | ", cur_col, "]")
}

positioned_feedback <- function(x, type_check_function, position_string, ...) {
  check_result <- type_check_function(x, ...)
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

type_string_to_type_check_function <- function(x) {
  switch(
    x,
    "String" = is_valid_string,
    "Char" = is_valid_string,
    "Integer" = is_valid_integer, 
    "Float" = is_valid_float,
    NA
  )
}

is_valid_string <- function(x, multi = FALSE, expected_choices = NA, ...) {
  # is string?
  check_1 <- checkmate::test_string(x, min.chars = 1)
  if ( !check_1 ) {
    cli::cli_alert_danger("Not a valid string")
    return(check_1)
  }
  # is multi?
  if (multi) {
    # is without superfluous white space?
    check_2 <- !grepl(".*;+?\\s+.*|.*\\s+;+?.*", x)
    if( !check_2 ) {
      cli::cli_alert_danger("Superfluous white space around separator ;")
      return(check_2)
    }
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is choices?
  if (length(expected_choices) > 1 || !is.na(expected_choices)) {
    check_3 <- all(sapply(x, function(y) { checkmate::test_choice(y, expected_choices) }))
    if ( !check_3 ) {
      cli::cli_alert_danger(paste("At least one value not in", paste(expected_choices, collapse = ", ")))
      return(check_3)
    }
  }
  return(TRUE)
}

is_valid_integer <- function(x, multi = FALSE, expected_range = c(-Inf, Inf), ...) {
  # is multi?
  if (multi) {
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is integer?
  check_1 <- all(grepl("^[0-9-]+$", x)) &&
    all(!grepl("\\.", x)) &&
    !any(is.na(suppressWarnings(as.integer(x))))
  if ( !check_1 ) {
    cli::cli_alert_danger("One or multiple values are not valid integer numbers")
    return(check_1)
  }
  # is in range?
  check_2 <- checkmate::test_integer(
    as.integer(x), lower = expected_range[1], upper = expected_range[2]
  )
  if ( !check_2 ) {
    cli::cli_alert_danger(
      paste("One or multiple values not in range", expected_range[1], "to", expected_range[2])
    )
    return(check_2)
  }
  return(TRUE)
}

is_valid_float <- function(x, multi = FALSE, expected_range = c(-Inf, Inf), ...) {
  # is multi?
  if (multi) {
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is integer?
  check_1 <- all(grepl("^[0-9\\.e-]+$", x)) &&
    !any(is.na(suppressWarnings(as.double(x))))
  if ( !check_1 ) {
    cli::cli_alert_danger("One or multiple values are not valid floating point numbers")
    return(check_1)
  }
  # is in range?
  check_2 <- checkmate::test_numeric(
    as.double(x), lower = expected_range[1], upper = expected_range[2]
  )
  if ( !check_2 ) {
    cli::cli_alert_danger(
      paste("One or multiple values not in range", expected_range[1], "to", expected_range[2])
    )
    return(check_2)
  }
  return(TRUE)
}
