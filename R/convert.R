#' @rdname cli_modules
#' @export
convert_module <- function(output_format, input_package, output_directory, log_directory = tempdir()) {
  # input check and prep
  checkmate::assert_choice(output_format, choices = c("eigenstrat"))
  checkmate::assert_directory_exists(input_package, access = "r")
  checkmate::assert_directory_exists(log_directory, access = "rw")
  # validate input package
  validation_result <- validate_module(input_package)
  if (validation_result == 1) {
    cli::cli_alert_danger("Can't convert broken package.")
    return(1)
  } else if (validation_result == 2) {
    cli::cli_alert_info("Conversion will be attempted anyway.")
  }
  # create output directory
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  checkmate::assert_directory_exists(output_directory, access = "rw")
  # start message
  convert_start_message(output_format, input_package, output_directory, log_directory)
  # select conversion submodule
  if (output_format == "eigenstrat") {
    convert_ped2eig(input_package, output_directory, log_directory)
  } else {
    stop("Unknown output format.")
  }
}

convert_start_message <- function(output_format, input_package, output_directory, log_directory) {
  cli::cli_h1("convert => Converts data in poseidon packages")
  cli::cli_alert(paste0("Output format:\t", output_format))
  cli::cli_alert(paste0("Input package:\t", input_package))
  cli::cli_alert(paste0("Output directory:\t", output_directory))
  cli::cli_alert(paste0("Log file directory:\t", log_directory))
}
