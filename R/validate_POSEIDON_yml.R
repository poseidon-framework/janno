can_POSEIDON_yml_be_read <- function(x) {
  tryCatch(
    yaml::read_yaml(x), 
    error = function(e) {
      cli::cli_alert_danger(paste("Can't read POSEIDON.yml file. More info:\n", e))
      return(FALSE)
    }
  )
  return(TRUE)
}

validate_POSEIDON_yml <- function(x) {
  return(
    positioned_feedback(x$poseidonVersion, is_valid_poseidon_version, "poseidonVersion") &
    positioned_feedback(x$title, is_valid_string, "title") &
    positioned_feedback(x$contributor$name, is_valid_string, "contributor > name") &
    positioned_feedback(x$contributor$email, is_valid_email, "contributor > email") &
    positioned_feedback(x$lastModified, is_valid_date, "lastModified") &
    positioned_feedback(x$bibFile, is_valid_string, "bibFile") &
    positioned_feedback(x$genotypeData$format, is_valid_string, "genotypeData > format") &
    positioned_feedback(x$genotypeData$genoFile, is_valid_string, "genotypeData > genoFile") &
    positioned_feedback(x$genotypeData$snpFile, is_valid_string, "genotypeData > snpFile") &
    positioned_feedback(x$genotypeData$indFile, is_valid_string, "genotypeData > indFile") &
    positioned_feedback(x$jannoFile, is_valid_string, "jannoFile")
  )
}

is_valid_email <- function(x) {
  check_0 <- is_valid_string(x)
  if ( check_0 ) {
    check_1 <- grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
    if ( !check_1 ) {
      cli::cli_alert_danger("Not a valid email adress for the contributor")
      return(check_1)
    }
  }
  return(check_0)
}

is_valid_date <- function(x) {
  check_0 <- is_valid_string(x)
  if ( check_0 ) {
    check_1 <- !is.na(suppressWarnings(as.POSIXlt(x, format = "%Y-%m-%d")))
    if ( !check_1 ) {
      cli::cli_alert_danger("Not a valid date of format %Y-%m-%d")
      return(check_1)
    }
  }
  return(check_0)
}

is_valid_poseidon_version <- function(x) {
  check_0 <- is_valid_string(x)
  if ( check_0 ) {
    check_1 <- grepl("[2]{1}\\.[0-9]+\\.[0-9]+", x)
    if ( !check_1 ) {
      cli::cli_alert_danger("Not a valid POSEIDON v.2 version number")
      return(check_1)
    }
  }
  return(check_0)
}

has_POSEIDON_yml_the_necessary_elements <- function(
  x,
  elements = c(
    "poseidonVersion", "title", "contributor", 
    "lastModified", "genotypeData", "jannoFile",
    "description", "bibFile"
  )
) {
  check <- all(elements %in% x)
  if ( !check ) {
    cli::cli_alert_danger(paste(
      "The following mandatory elements of the POSEIDON.yml are missing:",
      paste(elements[!elements %in% x], collapse = ", ")
    ))
  }
  return(check)
}
