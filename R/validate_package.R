validate_package <- function(input_package) {
  # flag for less important conditions
  everything_fine <- TRUE
  # beginn package check
  cli::cli_alert_info(input_package)
  # does it exist?
  if ( !checkmate::test_directory_exists(input_package) ) {
    cli::cli_alert_danger("The package directory does not exist")
    return(1)
  }
  # collect file paths
  POSEIDON_yml_file <- list.files(input_package, pattern = "POSEIDON.yml", full.names = T)
  janno_file <- list.files(input_package, pattern = "\\.janno", full.names = T)
  bed_file <- list.files(input_package, pattern = "\\.bed", full.names = T)
  bim_file <- list.files(input_package, pattern = "\\.bim", full.names = T)
  fam_file <- list.files(input_package, pattern = "\\.fam", full.names = T)
  README_file <- list.files(input_package, pattern = "README.txt", full.names = T)
  CHANGELOG_file <- list.files(input_package, pattern = "CHANGELOG.txt", full.names = T)
  bib_file <- list.files(input_package, pattern = "LITERATURE.bib", full.names = T)
  # does it contain the necessary files exactly once?
  if ( 
    !checkmate::test_string(POSEIDON_yml_file, min.chars = 1) | 
    !checkmate::test_string(janno_file, min.chars = 1) | 
    !checkmate::test_string(bed_file, min.chars = 1) | 
    !checkmate::test_string(bim_file, min.chars = 1) | 
    !checkmate::test_string(fam_file, min.chars = 1)
  ) {
    cli::cli_alert_danger(paste(
      "One or multiple of the necessary files",
      "(POSEIDON.yml, .janno, .bed, .bim, .fam)",
      "are missing or occur multiple times"
    ))
    return(1)
  }
  # are other files present?
  all_files <- list.files(input_package)
  additional_files <- all_files[
    !all_files %in% basename(
      c(POSEIDON_yml_file, janno_file, bed_file, bim_file, fam_file, README_file, CHANGELOG_file, bib_file))
  ]
  if (length(additional_files) > 0) {
    cli::cli_alert_warning(paste(
      "There are supplementary files present in this package:", 
      paste(additional_files, collapse = ", ")
    ))
  }
  # validate POSEIDON.yml
  cli::cli_alert_info(POSEIDON_yml_file)
  if ( !can_POSEIDON_yml_be_read(POSEIDON_yml_file) ) {
    return(1)
  }
  pyml <- yaml::read_yaml(POSEIDON_yml_file)
  if ( !has_POSEIDON_yml_the_necessary_elements(names(pyml)) ) {
    return(1)
  }
  if ( !validate_POSEIDON_yml(pyml, input_package) ) {
    everything_fine <- FALSE
  }
  # check .janno file
  janno_error_code <- validate_janno(list.files(input_package, pattern = "\\.janno", full.names = T))
  if (janno_error_code == 2) {
    everything_fine <- FALSE
  }
  # check data interactions
  ## .fam <-> .janno
  if (janno_error_code == 2) {
    cli::cli_alert_warning(paste(
      "There seem to be some issues with the janno file.",
      ".janno + .fam and .janno + .bib check may fail because the janno file is broken."
    ))
  }
  if ( !validate_fam_janno_interaction(fam_file, janno_file) ) {
    return(1)
  }
  ## .bib <-> .janno
  if ( length(list.files(input_package, pattern = "\\.bib")) != 0 ) {
    if ( !validate_bib_janno_interaction(bib_file, janno_file) ) {
      everything_fine <- FALSE
    }
  }
  # final output (serious errors already ended returned the error code 1)
  if (everything_fine) {
    return(0)
  } else {
    return(2)
  }
}
