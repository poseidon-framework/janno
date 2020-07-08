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
  cat(input_janno, "\n")
  # does it exist?
  if (file.exists(input_janno)) {
    cat("=> The janno file exists\n")
  } else {
    stop("The janno file does not exist")
  }
  # does it contain tab separated columns?
  input_janno_linewise <- readr::read_lines(input_janno, n_max = 50)
  if (all(grepl(".*\\t.*\\t.*\\t.*", input_janno_linewise))) {
    cat("=> The janno file seems to be a valid tab separated file\n")
  } else {
    stop("The janno file can't be a valid .tsv file with at least 4 columns")
  }
  # read file
  character_janno <- readr::read_tsv(input_janno, col_types = readr::cols(.default = "c"))
  # are the necessary columns present
  if (all(janno_column_names %in% colnames(character_janno))) {
    cat("=> The janno file has all necessary columns\n")
  } else {
    stop(
      "The janno file lacks the following columns: ", 
      paste(janno_column_names[!(janno_column_names %in% colnames(character_janno))], collapse = ", ")
    )
  }
  # do the columns have the right type
  cat("=> Cell content check\n")
  # loop through each column
  for (cur_col in colnames(character_janno)) {
    # get column background information
    expected_type <- hash::values(janno_column_name_column_type, cur_col)
    check_function <- type_string_to_check_function(expected_type)
    with_choices <- cur_col %in% janno_choice_columns
    if (with_choices) {
      expected_choices <- unlist(strsplit(
        hash::values(janno_column_name_choices, cur_col),
        ","
      ))
    }
    mandatory <- cur_col %in% janno_mandatory_columns
    # loop through each cell
    for (cur_row in 1:nrow(character_janno)) {
      cur_cell <- character_janno[[cur_col]][cur_row]
      ## general checks ##
      # special case: NA or ""
      if (is.na(cur_cell) | cur_cell == "") {
        cat("/!\\ ->", cur_col, ":", cur_row, "=> Empty cells are not allowed, please fill with n/a")
        cat("\n")
      # special case: n/a
      } else if (cur_cell == "n/a") {
        if (mandatory) {
          cat("/!\\ ->", cur_col, ":", cur_row, "=> n/a in a mandatory column")
          cat("\n")
        } else {
          next
        }
      }
      ## column type checks ##
      # normal case: values
      if (with_choices) {
      # with defined set of choices
        check_function(cur_cell, cur_col, cur_row, expected_choices)
      # without
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
    "Non-negative Integer list" = is_valid_non_negative_integer_list,
    "Float" = is_valid_float,
    NA
  )
}

is_valid_string <- function(x, cur_col, cur_row) {
  
}

is_valid_string_choice <- function(x, cur_col, cur_row, choices) {
  if (!(x %in% choices)) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Value not in", paste(choices, collapse = ", "))
    cat("\n")
  }
}

is_valid_string_list <- function(x, cur_col, cur_row) {
  if ( grepl(",", x) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> The separator for string lists is ; and not ,")
    cat("\n")
  }
  if( grepl(".*;?\\s+.*|.*\\s+;?.*", x) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Superfluous white space around separator ;")
    cat("\n")
  }
}

is_valid_char_choice <- function(x, cur_col, cur_row, choices) {
  if (!(x %in% choices)) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Value not in", paste(choices, collapse = ", "))
    cat("\n")
  }
}

is_valid_integer <- function(x, cur_col, cur_row) {
  if ( !grepl("^[0-9]+$", x) | is.na(suppressWarnings(as.integer(x))) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Value not a valid integer number")
    cat("\n")
  }
}

is_valid_non_negative_integer_list <- function(x, cur_col, cur_row) {
  if ( grepl(",", x) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> The separator for integer lists is ; and not ,")
    cat("\n")
  }
  if ( !grepl("^[0-9;]+$", x) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Not a valid non-negative integer list")
    cat("\n")
  }
  if( grepl(".*;?\\s+.*|.*\\s+;?.*", x) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Superfluous white space around separator ;")
    cat("\n")
  }
}

is_valid_float <- function(x, cur_col, cur_row) {
  if ( !grepl("^[0-9\\.-]+$", x) | is.na(suppressWarnings(as.numeric(x))) ) {
    cat("/!\\ ->", cur_col, ":", cur_row, "=> Value not a valid floating point number")
    cat("\n")
  }
}

validate_package <- function(input_package) {
  cat(input_package, "\n")
  # does it exist?
  if (dir.exists(input_package)) {
    cat("=> The directory exists\n")
  } else {
    stop("The directory does not exist")
  }
  # does it contain the necessary files once?
  necessary_files <- list.files(input_package, pattern = ".janno|.bed|.bim|.fam")
  extensions_necessary_files <- tools::file_ext(necessary_files)
  if (all(extensions_necessary_files == c("bed", "bim", "fam", "janno"))) {
    cat("=> The package contains the necessary files .bed, .bim, .fam and .janno exactly once\n")
  } else {
    stop("Necessary files (.bed, .bim, .fam and .janno) are missing or multiple of these are present")
  }
  # are other files present?
  all_files <- list.files(input_package)
  if (any(!all_files %in% necessary_files)) {
    cat("=> There are other files present as well:", paste(all_files[!all_files %in% necessary_files], collapse = ", "), "\n")
  }
  # check .janno file
  validate_janno(list.files(input_package, pattern = ".janno", full.names = T))
  cat("\n")
}








