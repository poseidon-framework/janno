#' @rdname cli_modules
#' @export
validate_module <- function(input_janno_file_or_packages) {
  type <- janno_or_package(input_janno_file_or_packages)
  validate_start_message(input_janno_file_or_packages, type)
  for (i in 1:length(input_janno_file_or_packages)) {
    if (type[i] == "janno") {
      validate_janno(input_janno_file_or_packages[i])
    } else if (type[i] == "package") {
      validate_package(input_janno_file_or_packages[i])
    }
  }
}

validate_start_message <- function(input_janno_file_or_packages, type) {
  cat(
    "\n",
    "*******************************************************\n",
    "validate => Validates janno files and poseidon packages\n",
    "*******************************************************\n",
    "\n",
    paste(type, input_janno_file_or_packages, sep = "\t=> ", collapse = "\n"),
    "\n\n",
    sep = ""
  )
}

janno_or_package <- function(input_janno_file_or_packages) {
  ifelse(grepl(".janno", input_janno_file_or_packages), "janno", "package")
}

validate_janno <- function(input_janno) {
  cli::cli_alert_info(input_janno)
  # does it exist?
  if (file.exists(input_janno)) {
    cli::cli_alert_success("The janno file exists")
  } else {
    cli::cli_alert_danger("The janno file does not exist")
    return(1)
  }
  # does it contain tab separated columns?
  input_janno_linewise <- readr::read_lines(input_janno, n_max = 50)
  if (all(grepl(".*\\t.*\\t.*\\t.*", input_janno_linewise))) {
    cli::cli_alert_success("The janno file seems to be a valid tab separated file")
  } else {
    cli::cli_alert_danger("The janno file is nota a valid tab separated file with")
    return(1)
  }
  # read file
  character_janno <- readr::read_tsv(input_janno, col_types = readr::cols(.default = "c"))
  # are the necessary columns present
  if (all(janno_column_names %in% colnames(character_janno))) {
    cli::cli_alert_success("The janno file has all necessary columns")
  } else {
    cli::cli_alert_danger(paste(
      "The janno file lacks the following columns: ", 
      paste(janno_column_names[!(janno_column_names %in% colnames(character_janno))], collapse = ", ")
    ))
    return(1)
  }
  # do the columns have the right type
  cli::cli_alert_info("Cell content check")
  # loop through each column
  for (cur_col in colnames(character_janno)) {
    # get column background information
    expected_type <- hash::values(janno_column_name_column_type, cur_col)
    check_function <- type_string_to_check_function(expected_type)
    mandatory <- cur_col %in% janno_mandatory_columns
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
    # loop through each cell
    for (cur_row in 1:nrow(character_janno)) {
      cur_cell <- character_janno[[cur_col]][cur_row]
      ## general checks ##
      # special case: NA or ""
      if (is.na(cur_cell) | cur_cell == "") {
        cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Empty cells are not allowed, please fill with n/a"))
        next
      # special case: n/a
      } else if (cur_cell == "n/a") {
        if (mandatory) {
          cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> n/a in a mandatory column"))
          next
        } else {
          next
        }
      }
      ## column type checks ##
      # with defined set of choices
      if (with_choices) {
        check_function(cur_cell, cur_col, cur_row, expected_choices)
      # with range
      } else if (with_range) {
        check_function(cur_cell, cur_col, cur_row, expected_range)
      # without anything
      } else {
        check_function(cur_cell, cur_col, cur_row)
      }
    }
  }
}

type_string_to_check_function <- function(x) {
  switch(
    x,
    "String" = is_valid_string,
    "String choice" = is_valid_string_choice,
    "String list" = is_valid_string_list,
    "Char choice" = is_valid_char_choice,
    "Integer" = is_valid_integer, 
    "Integer list" = is_valid_integer_list,
    "Float" = is_valid_float,
    NA
  )
}

is_valid_string <- function(x, cur_col, cur_row) {
  
}

is_valid_string_choice <- function(x, cur_col, cur_row, choices) {
  if (!(x %in% choices)) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not in", paste(choices, collapse = ", ")))
  }
}

is_valid_string_list <- function(x, cur_col, cur_row) {
  if ( grepl(",", x) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> The separator for string lists is ; and not ,"))
  }
  if( grepl(".*;?\\s+.*|.*\\s+;?.*", x) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Superfluous white space around separator ;"))
  }
}

is_valid_char_choice <- function(x, cur_col, cur_row, choices) {
  if (!(x %in% choices)) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not in", paste(choices, collapse = ", ")))
  }
}

is_valid_integer <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  if ( !grepl("^[0-9-]+$", x) | is.na(suppressWarnings(as.integer(x))) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not a valid integer number"))
  } else {
    x_integer <- as.integer(x)
    if ( !are_in_range(x_integer, expected_range) ) {
      cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not in range", expected_range[1], "to", expected_range[2]))
    }
  }
}

are_in_range <- function(x, expected_range) {
  all(x >= expected_range[1] & x <= expected_range[2])
}

is_valid_integer_list <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  if ( grepl(",", x) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> The separator for integer lists is ; and not ,"))
  } else if ( !grepl("^[0-9;-]+$", x) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Contains symbols that do not belong here"))
  } else if( grepl(".*;?\\s+.*|.*\\s+;?.*", x) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Superfluous white space around separator ;"))
  } else {
    x_integer <- as.integer(unlist(strsplit(x, ";")))
    if ( !are_in_range(x_integer, expected_range) ) {
      cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> One or multiple values not in range", expected_range[1], "to", expected_range[2]))
    }
  }
}

is_valid_float <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  if ( !grepl("^[0-9\\.-]+$", x) | is.na(suppressWarnings(as.numeric(x))) ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not a valid floating point number"))
  } else {
    x_numeric <- as.numeric(x)
    if ( !are_in_range(x_numeric, expected_range) ) {
      cli::cli_alert_danger(paste(cur_row, ":", cur_col, "=> Value not in range", expected_range[1], "to", expected_range[2]))
    }
  }
}

validate_package <- function(input_package) {
  cli::cli_alert_info(input_package)
  # does it exist?
  if (dir.exists(input_package)) {
    cli::cli_alert_success("The directory exists")
  } else {
    cli::cli_alert_danger("The directory does not exist")
    return(1)
  }
  # does it contain the necessary files once?
  necessary_files <- list.files(input_package, pattern = ".janno|.bed|.bim|.fam")
  extensions_necessary_files <- tools::file_ext(necessary_files)
  if (all(extensions_necessary_files == c("bed", "bim", "fam", "janno"))) {
    cli::cli_alert_success("The package contains the necessary files .bed, .bim, .fam and .janno exactly once")
  } else {
    cli::cli_alert_danger("Necessary files (.bed, .bim, .fam and .janno) are missing or multiple of these are present")
    return(1)
  }
  # are other files present?
  all_files <- list.files(input_package)
  if (any(!all_files %in% necessary_files)) {
    cli::cli_alert_warning(paste("There are other files present as well:", paste(all_files[!all_files %in% necessary_files], collapse = ", ")))
  }
  # check .janno file
  validate_janno(list.files(input_package, pattern = ".janno", full.names = T))
  cat("\n")
}








