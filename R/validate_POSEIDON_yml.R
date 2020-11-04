validate_POSEIDON_yml <- function(
  POSEIDON_yml_file, input_package, ignore_genotype_files = FALSE
) {
  
  cli::cli_alert_info(basename(POSEIDON_yml_file))
  if ( !can_POSEIDON_yml_be_read(POSEIDON_yml_file) ) {
    return(FALSE)
  }
  pyml <- yaml::read_yaml(POSEIDON_yml_file)
  return(
    has_POSEIDON_yml_the_necessary_elements(pyml) &&
      check_POSEIDON_yml_elements(
        pyml, input_package, ignore_genotype_files = ignore_genotype_files
      )
  )
}

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

has_POSEIDON_yml_the_necessary_elements <- function(
  x
) {
  mandatory_top_level_elements <- c("poseidonVersion", "title", "contributor", "genotypeData", "jannoFile")
  if ( !all(mandatory_top_level_elements %in% names(x)) ) {
    cli::cli_alert_danger(paste(
      "The following mandatory top level elements of the POSEIDON.yml are missing:",
      paste(mandatory_top_level_elements[!mandatory_top_level_elements %in% names(x)], collapse = ", ")
    ))
    return(FALSE)
  }
  mandatory_genotype_data_elements <- c("format", "genoFile", "snpFile", "indFile")
  if ( !all(mandatory_genotype_data_elements %in% names(x[["genotypeData"]])) ) {
    cli::cli_alert_danger(paste(
      "The following mandatory top level elements of the POSEIDON.yml are missing:",
      paste(mandatory_genotype_data_elements[!mandatory_genotype_data_elements %in% names(x[["genotypeData"]])], collapse = ", ")
    ))
    return(FALSE)
  }
  if ( !all(sapply(x[["contributor"]], function(x) {"name" %in% names(x) & "email" %in% names(x)})) ) {
    cli::cli_alert_danger(paste(
      "There is an issue with the contributor yaml array. Each contributor must be listed with name and email."
    ))
    return(FALSE)
  }
  return(TRUE)
}

check_POSEIDON_yml_elements <- function(x, package_path, ignore_genotype_files = FALSE) {
  return(
    positioned_feedback(x$poseidonVersion, is_valid_poseidon_version, "poseidonVersion") &
    positioned_feedback(x$title, is_valid_string, "title") &
    is_valid_contributors_list(x$contributor) &
    positioned_feedback(x$lastModified, is_valid_date, "lastModified") &
    positioned_feedback(x$bibFile, produce_check_for_valid_file(package_path), "bibFile") &
    positioned_feedback(x$genotypeData$format, is_valid_string, "genotypeData > format") &
    ifelse(
      !ignore_genotype_files,
      positioned_feedback(
        x$genotypeData$genoFile, produce_check_for_valid_file(package_path), "genotypeData > genoFile"
      ) &
      positioned_feedback(
        x$genotypeData$snpFile, produce_check_for_valid_file(package_path), "genotypeData > snpFile"
      ) &
      positioned_feedback(
        x$genotypeData$indFile, produce_check_for_valid_file(package_path), "genotypeData > indFile"
      ),
      TRUE
    ) &
    positioned_feedback(x$jannoFile, produce_check_for_valid_file(package_path), "jannoFile")
  )
}

produce_check_for_valid_file <- function(package_path) {
  function(x) {
    check <- checkmate::test_file_exists(file.path(package_path, x))
    if ( !check ) {
      cli::cli_alert_danger("File does not seem to exist.")
    }
    return(check)
  }
}

is_valid_contributors_list <- function(x) {
  sapply(seq_along(x), function(i) {
    y <- x[[i]]
    positioned_feedback(y$name, is_valid_string, paste("contributor", i, "> name"))
    positioned_feedback(y$email, is_valid_email, paste("contributor", i, "> email"))
  }) %>% all()
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
