validate_plink <- function(bed_file, bim_file, fam_file) {
  validate_plink_bed(bed_file) &
    validate_plink_bim(bim_file) &
    validate_plink_fam(fam_file)
}

validate_plink_bed <- function(bed_file) {
  cli::cli_alert_info(bed_file)
  if ( !all(as.character(readBin(bed_file, what = "raw", n = 3)) == c("6c", "1b", "01")) ) {
    cli::cli_alert_danger(paste(
      "According to the start bytes this is not a valid .bed file",
      "(see https://www.cog-genomics.org/plink/1.9/formats#bed)"
    ))
    return(FALSE)
  }
  return(TRUE)
}

validate_plink_bim <- function(bim_file) {
  cli::cli_alert_info(bim_file)
  tryCatch({
    readr::read_tsv(
      bim_file, 
      col_names = FALSE,
      col_types = readr::cols(
        readr::col_character(),
        readr::col_character(),
        readr::col_double(),
        readr::col_double(),
        readr::col_character(),
        readr::col_character()
      )
    )
    TRUE
  }, warning = function(w) {
    cli::cli_alert_danger(paste(
      "Test reading of .bim file showed an issue:",
      w
    ))
    return(FALSE)
  }, error = function(e) {
    cli::cli_alert_danger(paste(
      "Test reading of .bim file showed an issue:",
      e
    ))
    return(FALSE)
  })
}

validate_plink_fam <- function(fam_file) {
  cli::cli_alert_info(fam_file)
  return(TRUE)
}
