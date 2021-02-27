#' @rdname janno
#' @export
validate_janno <- function(path) {
  message("Validating .janno files...")
  # input checks and search for janno files
  janno_file_paths <- get_janno_file_paths(path)
  # validate files
  lapply(janno_file_paths, validate_one_janno) %>% dplyr::bind_rows()
}

validate_one_janno <- function(path) {
  # the issues are stored in a data.frame:
  issues <- tibble::tibble(
    row = integer(),
    column = character(),
    value = character(),
    issue = character()
  )
  # does it exist?
  if ( !check_if_file_exists(path) ) {
    stop("Input file does not exist")
  }
  # does it contain tab separated columns?
  if ( !is_tab_separated_file(path) ) {
    stop("Input file is not a valid tab separated file")
  }
  # read file
  character_janno <- readr::read_tsv(path, col_types = readr::cols(.default = "c"))
  # are the necessary columns present?
  column_check <- has_necessary_columns(character_janno)
  if (!is.na(column_check)) {
    stop(column_check)
  }
  # column wise check: loop through each column
  for (cur_col in colnames(character_janno)) {
    # get necessary information to check column
    cur_constraints <- get_column_constraints(cur_col)
    # column wise checks
    # if (!cur_constraints$bonus) {
    #   if (only_na_in_column(character_janno, cur_constraints)) {
    #     issues <- issues %>% append_issue(
    #       column = cur_col,
    #       issue = "Only n/a values in column"
    #     )
    #   }
    # }
    if (cur_constraints$no_dupli) {
      if (has_duplicates(character_janno, cur_constraints)) {
        issues <- issues %>% append_issue(
          column = cur_col,
          issue = "Duplicates are not allowed in this column"
        )
      }
    }
    # cell wise checks: loop through each cell
    for (cur_row in 1:nrow(character_janno)) {
      cur_cell <- character_janno[[cur_col]][cur_row]
      # empty cell
      if (is_empty(cur_cell)) {
        issues <- issues %>% append_issue(
          row = cur_row,
          column = cur_col,
          value = cur_cell,
          issue = "Empty cells are not allowed, please fill with n/a"
        )
        next
      }
      # n/a in mandatory column
      if (cur_constraints$mandatory) {
        if (is_n_a(cur_cell)) {
          issues <- issues %>% append_issue(
            row = cur_row,
            column = cur_col,
            value = cur_cell,
            issue = "n/a in a mandatory column"
          )
          next
        }
      }
      if (!is_n_a(cur_cell)) {
        # type check
        type_check <- cur_constraints$type_check_function(cur_cell, cur_constraints)
        if (!is.na(type_check)) {
          issues <- issues %>% append_issue(
            row = cur_row,
            column = cur_col,
            value = cur_cell,
            issue = type_check
          )
          next
        }
        # leading or trailing whitespaces
        if (has_leading_or_trailing_whitespace(cur_cell)) {
          issues <- issues %>% append_issue(
            row = cur_row,
            column = cur_col,
            value = cur_cell,
            issue = "Cell has leading or trailing whitespaces"
          )
          next
        }
      }
    }
  }
  # final output
  source_janno_short <- get_last_two_elements_of_path(path)
  issues %>%
    tibble::add_column(source_file = source_janno_short, .before = 1)
}

check_if_file_exists <- function(x) {
  checkmate::test_file_exists(x, access = "r", extension = "janno")
}

get_column_constraints <- function(cur_col) {
  constraints <- list(
    column              = cur_col,
    expected_type       = hash::values(janno_column_name_data_type, cur_col),
    multi               = cur_col %in% janno_multi_value_columns,
    mandatory           = cur_col %in% janno_mandatory_columns,
    no_dupli            = cur_col %in% janno_unique_columns,
    with_choices        = cur_col %in% janno_choice_columns,
    with_range          = cur_col %in% janno_range_columns,
    bonus               = cur_col %in% janno_bonus_columns,
    type_check_function = type_string_to_type_check_function(
      hash::values(janno_column_name_data_type, cur_col)
    ),
    expected_choices    = NA,
    expected_range      = c(-Inf, Inf)
  )
  if (constraints$with_choices) {
    constraints$expected_choices <- unlist(strsplit(
      hash::values(janno_column_name_choices, cur_col), 
      ";"
    ))
  } 
  if (constraints$with_range) {
    constraints$expected_range <- c(
      hash::values(janno_column_name_range_lower, cur_col),
      hash::values(janno_column_name_range_upper, cur_col)
    )
  }
  return(constraints)
}

append_issue <- function(x, row = NA, column = NA, value = NA, issue = NA) {
  rbind(
    x,
    tibble::tibble(row = row, column = column, value = value, issue = issue)
  )
}

has_leading_or_trailing_whitespace <- function(x) {
  any(grepl("(*UCP)^\\s+", x, perl = T) | grepl("(*UCP)\\s+$", x, perl = T))
}

is_n_a <- function(x) {
  x == "n/a"
}

is_empty <- function(x) {
  (is.na(x) | x == "")
}

only_na_in_column <- function(x, co) {
  all(x[[co$column]] == "n/a")
}

has_duplicates <- function(x, co) {
  length(unique(x[[co$column]])) != length(x[[co$column]])
}

has_necessary_columns <- function(x, columns = janno_mandatory_columns) {
  check <- all(columns %in% colnames(x))
  if ( !check ) {
    return(paste(
      "The janno file lacks the following columns: ", 
      paste(columns[!(columns %in% colnames(x))], collapse = ", ")
    ))
  }
  return(NA)
}

is_tab_separated_file <- function(x) {
  x_linewise <- readr::read_lines(x, n_max = 50)
  all(grepl(".*\\t.*", x_linewise))
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

is_valid_string <- function(x, co) {
  # is string?
  check_1 <- checkmate::test_string(x, min.chars = 1)
  if ( !check_1 ) {
    return("Not a valid string")
  }
  # is multi?
  if (co$multi) {
    # is without superfluous white space?
    check_2 <- !grepl(".*;+?\\s+.*|.*\\s+;+?.*", x)
    if( !check_2 ) {
      return("Superfluous white space around separator ;")
    }
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is choices?
  if (length(co$expected_choices) > 1 || !is.na(co$expected_choices)) {
    check_3 <- all(sapply(x, function(y) { checkmate::test_choice(y, co$expected_choices) }))
    if ( !check_3 ) {
      return(paste("At least one value not in", paste(co$expected_choices, collapse = ", ")))
    }
  }
  return(NA)
}

is_valid_integer <- function(x, co) {
  # is multi?
  if (co$multi) {
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is integer?
  check_1 <- all(grepl("^[0-9-]+$", x)) &&
    all(!grepl("\\.", x)) &&
    !any(is.na(suppressWarnings(as.integer(x))))
  if ( !check_1 ) {
    return("One or multiple values are not valid integer numbers")
  }
  # is in range?
  check_2 <- checkmate::test_integer(
    as.integer(x), lower = co$expected_range[1], upper = co$expected_range[2]
  )
  if ( !check_2 ) {
    return(
      paste("One or multiple values not in range", co$expected_range[1], "to", co$expected_range[2])
    )
  }
  return(NA)
}

is_valid_float <- function(x, co) {
  # is multi?
  if (co$multi) {
    # split to true multi
    x <- unlist(strsplit(x, ";"))
  }
  # is integer?
  check_1 <- all(grepl("^[0-9\\.e-]+$", x)) &&
    !any(is.na(suppressWarnings(as.double(x))))
  if ( !check_1 ) {
    return("One or multiple values are not valid floating point numbers")
  }
  # is in range?
  check_2 <- checkmate::test_numeric(
    as.double(x), lower = co$expected_range[1], upper = co$expected_range[2]
  )
  if ( !check_2 ) {
    return(
      paste("One or multiple values not in range", co$expected_range[1], "to", co$expected_range[2])
    )
  }
  return(NA)
}
