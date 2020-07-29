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
  result <- c()
  for (i in 1:length(input_janno_file_or_packages)) {
    if (type[i] == "janno") {
      result[i] <- validate_janno(input_janno_file_or_packages[i])
      if (result[i] == 0) {
        cli::col_green("Everything seems to be alright with this janno file.")
        cat("\n")
      } else if (result[i] == 1) {
        cli::col_red("This is not a valid janno file.")
        cat("\n")
      } else if (result[i] == 2) {
        cli::col_yellow("This seems to be a valid janno file, but some things are fishy.")
        cat("\n")
      }
    } else if (type[i] == "package") {
      result[i] <- validate_package(input_janno_file_or_packages[i])
      if (result[i] == 0) {
        cli::col_green("Everything seems to be alright with this package.")
        cat("\n")
      } else if (result[i] == 1) {
        cli::col_red("This is not a valid package.")
        cat("\n")
      } else if (result[i] == 2) {
        cli::col_yellow("This seems to be a valid package, but some things are fishy.")
        cat("\n")
      }
    }
  }
  if ( length(input_janno_file_or_packages) > 1) {
    cat("-----\n")
    general_result <- 1
    if (all(result == 0)) {
      general_result <- 0
      cli::col_green("Everything seems to be alright with these packages or janno files.")
    } else if (any(result == 1)) {
      general_result <- 1
      cli::col_red("There are non-valid packages or janno files in this set.")
    } else if (any(result == 2)) {
      general_result <- 2
      cli::col_yellow("These seem to be valid packages or janno files, but some things are fishy.")
    }
    return(general_result)
  }
  return(result)
}

validate_start_message <- function(input_janno_file_or_packages, type) {
  cli::cli_h1("validate => Validates janno files and poseidon packages")
}

validate_janno_or_package <- function(input_janno_file_or_packages) {
  ifelse(grepl(".janno", input_janno_file_or_packages), "janno", "package")
}
