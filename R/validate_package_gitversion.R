#' validate_packages_gitversion
#'
#' Special version of the package validation for the reduced representation
#' of the packages on github.
#'
#' @param input_packages Character vector. Paths to poseidon packages
#'
#' @export
validate_packages_gitversion <- function(package_search_path) {
  input_packages <- dirname(list.files(
    package_search_path, "POSEIDON.yml", recursive = T, full.names = T
  ))
  error_codes <- sapply(input_packages, function(x) {
    cat("\n")
    validate_package_gitversion(x)
  })
  if (any(error_codes == 1)) {
    cat("\n")
    stop(paste(
      "There are broken packages in this selection:",
      paste(basename(input_packages[error_codes == 1]), collapse = ", ")
    ))
  }
}

validate_package_gitversion <- function(input_package) {
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
  README_file <- list.files(input_package, pattern = "README.txt", full.names = T)
  CHANGELOG_file <- list.files(input_package, pattern = "CHANGELOG.txt", full.names = T)
  bib_file <- list.files(input_package, pattern = "LITERATURE.bib", full.names = T)
  # does it contain the necessary files exactly once?
  if ( 
    !checkmate::test_string(POSEIDON_yml_file, min.chars = 1) | 
    !checkmate::test_string(janno_file, min.chars = 1)
  ) {
    cli::cli_alert_danger(paste(
      "One or multiple of the necessary files",
      "(POSEIDON.yml, .janno)",
      "are missing or occur multiple times"
    ))
    return(1)
  }
  # are other files present?
  all_files <- list.files(input_package)
  additional_files <- all_files[
    !all_files %in% basename(
      c(POSEIDON_yml_file, janno_file, README_file, CHANGELOG_file, bib_file)
    )
  ]
  if (length(additional_files) > 0) {
    cli::cli_alert_warning(paste(
      "There are supplementary files present in this package:", 
      paste(additional_files, collapse = ", ")
    ))
  }
  # check POSEIDON.yml
  if ( !validate_POSEIDON_yml(POSEIDON_yml_file, input_package, ignore_genotype_files = TRUE) ) {
    everything_fine <- FALSE
  }
  # check .janno file
  janno_error_code <- validate_janno(list.files(input_package, pattern = "\\.janno", full.names = T))
  if (janno_error_code == 1) {
    return(1)
  } else if (janno_error_code == 2) {
    everything_fine <- FALSE
  }
  # check .bib file
  if ( length(bib_file) != 0 ) {
    bib_file_fine <- TRUE
    if ( !validate_bibtex(bib_file) ) {
      bib_file_fine <- FALSE
      everything_fine <- FALSE
    }
  }
  # check data interactions
  cli::cli_alert_info("Cross-file relationships")
  ## .bib <-> .janno
  if ( length(bib_file) != 0 && bib_file_fine ) {
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
