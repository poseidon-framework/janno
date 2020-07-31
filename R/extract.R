#' @rdname cli_modules
#' @export
extract_module <- function(filter_file, input_package, output_directory, log_directory = tempdir()) {
  # input check and prep
  checkmate::assert_file_exists(input_file, access = "r")
  checkmate::assert_directory_exists(input_package, access = "r")
  checkmate::assert_directory_exists(log_directory, access = "rw")
  # validate input package
  validation_result <- validate_module(input_package)
  if (validation_result == 1) {
    cli::cli_alert_danger("Can't extract from broken package.")
    return(1)
  } else if (validation_result == 2) {
    cli::cli_alert_info("Extraction will be attempted anyway.")
  }
  # create output directory
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  checkmate::assert_directory_exists(output_directory, access = "rw")
  # start message
  extract_start_message(filter_file, input_package, output_directory, log_directory)
  # construct extraction command
  # extract(...)
}

extract_start_message <- function(filter_file, input_package, output_directory, log_directory) {
  cli::cli_h1("extract => Extracts a subset of individuals from a poseidon package")
  cli::cli_alert(paste0("Filter file:\t", filter_file))
  cli::cli_alert(paste0("Input package:\t", input_package))
  cli::cli_alert(paste0("Output directory:\t", output_directory))
  cli::cli_alert(paste0("Log file directory:\t", log_directory))
}
