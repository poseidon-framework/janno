#' @rdname cli_modules
#' @export
validate_module <- function(input_janno_file_or_packages) {
  # input check and prep
  checkmate::assert_character(input_janno_file_or_packages, any.missing = FALSE, all.missing = FALSE, min.len = 1)
  validate_janno_or_package(input_janno_file_or_packages) -> type
  if ("janno" %in% type) {
    checkmate::assert_file_exists(input_janno_file_or_packages[type == "janno"], access = "r", extension = "janno")
  } else if ("package" %in% type) {
    checkmate::assert_directory_exists(input_janno_file_or_packages[type == "package"])
  }
  # start message
  validate_start_message(input_janno_file_or_packages, type)
  # select validation submodule
  for (i in 1:length(input_janno_file_or_packages)) {
    if (type[i] == "janno") {
      validate_janno(input_janno_file_or_packages[i])
    } else if (type[i] == "package") {
      validate_package(input_janno_file_or_packages[i])
    }
  }
}

validate_start_message <- function(input_janno_file_or_packages, type) {
  cli::cli_h1("validate => Validates janno files and poseidon packages")
}

validate_janno_or_package <- function(input_janno_file_or_packages) {
  ifelse(grepl(".janno", input_janno_file_or_packages), "janno", "package")
}

#### janno file validation ####

validate_janno <- function(input_janno) {
  cli::cli_rule(left = input_janno)
  # does it exist?
  if ( !checkmate::test_file_exists(input_janno) ) {
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
    unique <- cur_col %in% janno_unique_columns
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
    if (cur_col %in% unique) {
      if (no_duplicates(character_janno, cur_col)) {
        everything_fine_flag <- FALSE
      }
    }
    # loop through each cell: cell wise checks
    for (cur_row in 1:nrow(character_janno)) {
      cur_cell <- character_janno[[cur_col]][cur_row]
      ## general checks ##
      if (is_empty(cur_cell, cur_col, cur_row)) {
        everything_fine_flag <- FALSE
        next
      # special case: n/a
      } else if (is_na(cur_cell)) {
        if (mandatory) {
          cli::cli_alert_danger(paste(cur_row, ":", cur_col, "n/a in a mandatory column"))
          everything_fine_flag <- FALSE
          next
        } else {
          next
        }
      }
      ## column type checks ##
      # with defined set of choices
      if (with_choices) {
        if ( !check_function(cur_cell, cur_col, cur_row, expected_choices) ) {
          everything_fine_flag <- FALSE
        }
      # with range
      } else if (with_range) {
        if ( !check_function(cur_cell, cur_col, cur_row, expected_range) ) {
          everything_fine_flag <- FALSE
        }
      # without anything
      } else {
        if ( !check_function(cur_cell, cur_col, cur_row) ) {
          everything_fine_flag <- FALSE
        }
      }
    }
  }
}

is_na <- function(x) {
  x == "n/a"
}

is_empty <- function(x, cur_col, cur_row) {
  check <- is.na(x) | x == ""
  if ( !check ) {
    cli::cli_alert_danger(paste(
      cur_row, ":", cur_col, "Empty cells are not allowed, please fill with n/a"
    ))
  }
  return(check)
}

no_duplicates <- function(x, column) {
  check <- unique(x[[column]])
  if ( !check ) {
    cli::cli_alert_danger(paste(
      "Duplicates are not allowed in column ", column
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
    cli::cli_alert_danger("The janno file is nota a valid tab separated file with")
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

is_valid_string <- function(x, cur_col, cur_row) {
  check <- checkmate::test_string(min.chars = 1)
  if ( !check ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Not a valid string"))
  }
  return(check)
}

is_valid_string_choice <- function(x, cur_col, cur_row, choices) {
  check <- checkmate::test_choice(choices)
  if (!check ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Value not in", paste(choices, collapse = ", ")))
  }
  return(check)
}

is_valid_string_list <- function(x, cur_col, cur_row) {
  check_0 <- checkmate::test_string(min.chars = 1)
  if ( !check_0 ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Not a valid string"))
  }
  check_1 <- !grepl(".*;?\\s+.*|.*\\s+;?.*", x)
  if( !check2 ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Superfluous white space around separator ;"))
  }
  return(all(c(check_0, check_1)))
}

is_valid_integer <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  check_0 <- checkmate::test_integer(x)
  if ( !check_0 ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Value not a valid integer number"))
  } else {
    check_1 <- checkmate::test_integer(x, lower = expected_range[1], upper = expected_range[2])
    if ( !check_1 ) {
      cli::cli_alert_danger(paste(
        cur_row, ":", cur_col, "Value not in range", expected_range[1], "to", expected_range[2]
      ))
    }
  }
  return(all(check_0, check_1))
}

is_valid_integer_list <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  supposed_integers <- unlist(strsplit(x, split = ";"))
  check_0 <- checkmate::test_integer(supposed_integers)
  if ( !check_0 ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "One or multiple values not valid integer numbers"))
  } else {
    check_1 <- checkmate::test_integer(x, lower = expected_range[1], upper = expected_range[2])
    if ( !check_1 ) {
      cli::cli_alert_danger(paste(
        cur_row, ":", cur_col, "One or multiple values not in range", expected_range[1], "to", expected_range[2]
      ))
    }
  }
  return(all(check_0, check_1))
}

is_valid_float <- function(x, cur_col, cur_row, expected_range = c(-Inf, Inf)) {
  check_0 <- checkmate::test_double(x)
  if ( !check_0 ) {
    cli::cli_alert_danger(paste(cur_row, ":", cur_col, "Value not a valid double number"))
  } else {
    check_1 <- checkmate::test_double(x, lower = expected_range[1], upper = expected_range[2])
    if ( !check_1 ) {
      cli::cli_alert_danger(paste(
        cur_row, ":", cur_col, "Value not in range", expected_range[1], "to", expected_range[2]
      ))
    }
  }
}

#### validate poseidon package ####

validate_package <- function(input_package) {
  cli::cli_rule(left = input_package)
  # does it exist?
  if (dir.exists(input_package)) {
    cli::cli_alert_success("The directory exists")
  } else {
    cli::cli_alert_danger("The directory does not exist")
    return(1)
  }
  # validate POSEIDON.yml
  validate_POSEIDON_yml(list.files(input_package, pattern = "POSEIDON\\.yml", full.names = T)) -> read_success
  if ( is.logical(read_success) && !read_success ) {
    return(1)
  }
  # does it contain the other necessary files once?
  necessary_files <- list.files(input_package, pattern = "\\.janno|\\.bed|\\.bim|\\.fam")
  extensions_necessary_files <- tools::file_ext(necessary_files)
  if (all(extensions_necessary_files == c("bed", "bim", "fam", "janno"))) {
    cli::cli_alert_success(
      "The package contains the necessary files POSEIDON.yml, .bed, .bim, .fam and .janno exactly once"
    )
  } else {
    cli::cli_alert_danger(
      "Necessary files (.bed, .bim, .fam and .janno) are missing or multiple of these are present"
    )
    return(1)
  }
  # are other files present?
  all_files <- list.files(input_package)
  if (any(!all_files %in% necessary_files)) {
    cli::cli_alert_warning(paste(
      "There are other files present as well:", 
      paste(all_files[!all_files %in% necessary_files], collapse = ", ")
    ))
  }
  # check .janno file
  validate_janno(list.files(input_package, pattern = ".janno", full.names = T))
}

#### validate POSEIDON.yml files ####

validate_POSEIDON_yml <- function(input_POSEIDON_yml) {
  # check if file can be read by yaml::read_yaml
  pyml <- tryCatch(
    yaml::read_yaml(input_POSEIDON_yml), 
    error = function(e) {
      cli::cli_alert_danger(paste(
        "Can't read POSEIDON.yml file. More info:\n",
        e
      ))
      return(FALSE)
    }
  )
  if ( is.logical(pyml) && !pyml ) {
    return(pyml)
  }
  has_the_necessary_elements(names(pyml))
  is_valid_poseidon_version(pyml$poseidonVersion)
  is_valid_string(pyml$title)
  is_valid_string(pyml$description)
  is_valid_string(pyml$contributor$name)
  is_valid_email(pyml$contributor$email)
}

is_valid_email <- function(x) {
  if ( !grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE) ) {
    cli::cli_alert_danger("Not a valid email adress for the contributor")
  }
}

is_valid_poseidon_version <- function(x) {
  if ( !grepl("[2]{1}\\.[0-9]+\\.[0-9]+", x) ) {
    cli::cli_alert_danger("Not a valid POSEIDON v.2 version number")
  }
}

has_the_necessary_elements <- function(
  x,
  mandatory_elements = c(
    "poseidonVersion", "title", "contributor", 
    "lastModified", "genotypeData", "jannoFile"
  ),
  optional_elements = c("description", "bibFile")
) {
  if ( !all(mandatory_elements %in% x) ) {
    cli::cli_alert_danger(paste(
      "The following mandatory elements of the POSEIDON.yml are missing:",
      paste(mandatory_elements[!mandatory_elements %in% x], collapse = ", ")
    ))
  }
  if ( !all(optional_elements %in% x) ) {
    cli::cli_alert_info(paste(
      "The following optional elements of the POSEIDON.yml are missing:",
      paste(optional_elements[!optional_elements %in% x], collapse = ", ")
    ))
  }
}














