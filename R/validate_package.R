validate_package <- function(input_package) {
  cat("*****\n")
  cli::cli_alert(input_package)
  # does it exist?
  if ( !checkmate::test_directory_exists(input_package) ) {
    cli::cli_alert_danger("The package directory does not exist")
    return(1)
  }
  # validate POSEIDON.yml
  POSEIDON_yml_file <- list.files(input_package, pattern = "POSEIDON\\.yml", full.names = T)
  cli::cli_alert_info(POSEIDON_yml_file)
  if ( !checkmate::test_string(POSEIDON_yml_file, min.chars = 1) ) {
    cli::cli_alert_danger(
      "Can't find POSEIDON.yml file"
    )
    return(1)
  }
  if ( !can_POSEIDON_yml_be_read(POSEIDON_yml_file) ) {
    return(1)
  }
  pyml <- yaml::read_yaml(POSEIDON_yml_file)
  if ( !has_POSEIDON_yml_the_necessary_elements(names(pyml)) ) {
    return(1)
  }
  # flag for less important conditions
  everything_fine_flag <- TRUE
  if ( !validate_POSEIDON_yml(pyml) ) {
    everything_fine_flag <- FALSE
  }
  # does it contain the other necessary files exactly once?
  janno_file <- list.files(input_package, pattern = "\\.janno")
  bed_file <- list.files(input_package, pattern = "\\.bed")
  bim_file <- list.files(input_package, pattern = "\\.bim")
  fam_file <- list.files(input_package, pattern = "\\.fam")
  if ( 
    !checkmate::test_string(janno_file, min.chars = 1) | 
    !checkmate::test_string(bed_file, min.chars = 1) | 
    !checkmate::test_string(bim_file, min.chars = 1) | 
    !checkmate::test_string(fam_file, min.chars = 1) 
  ) {
    cli::cli_alert_danger(
      "One or multiple of the necessary files (.bed, .bim, .fam and .janno) are missing duplicate multiple times"
    )
    return(1)
  }
  # are other files present?
  all_files <- list.files(input_package)
  additional_files <- all_files[
    !all_files %in% c(basename(POSEIDON_yml_file), janno_file, bed_file, bim_file, fam_file)
    ]
  if (length(additional_files) > 0) {
    cli::cli_alert_warning(paste(
      "There are supplementary files present in this package:", 
      paste(additional_files, collapse = ", ")
    ))
  }
  # check .janno file
  error_code <- validate_janno(list.files(input_package, pattern = ".janno", full.names = T))
  # final output
  cat("*****\n")
  if (error_code == 0) {
    return(0)
  } else if (error_code == 2 || !everything_fine_flag) {
    return(2)
  }
}
