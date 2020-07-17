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
    is_valid_poseidon_version(x$poseidonVersion) &
      is_valid_string(x$title) &
      is_valid_string(x$contributor$name) &
      is_valid_email(x$contributor$email)
  )
}

is_valid_email <- function(x) {
  check <- grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
  if ( !check ) {
    cli::cli_alert_danger("Not a valid email adress for the contributor")
  }
  return(check)
}

is_valid_poseidon_version <- function(x) {
  check <- grepl("[2]{1}\\.[0-9]+\\.[0-9]+", x)
  if ( !check ) {
    cli::cli_alert_danger("Not a valid POSEIDON v.2 version number")
  }
  return(check)
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
