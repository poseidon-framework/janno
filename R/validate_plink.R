validate_plink <- function(bed_file, bim_file, fam_file) {
  validate_plink_bed(bed_file) &
    validate_plink_bim(bim_file) &
    validate_plink_fam(fam_file)
}

validate_plink_bed <- function(bed_file) {
  cli::cli_alert_info(bed_file)
  return(TRUE)
}

validate_plink_bim <- function(bim_file) {
  cli::cli_alert_info(bim_file)
  return(TRUE)
}

validate_plink_fam <- function(fam_file) {
  cli::cli_alert_info(fam_file)
  return(TRUE)
}
