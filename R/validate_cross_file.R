validate_fam_janno_interaction <- function(fam_file, janno_file) {
  # read files
  fam <- readr::read_delim(
    fam_file, 
    " ", 
    col_types = readr::cols(.default = readr::col_character()),
    col_names = F
  )
  janno <- read_janno(janno_file, validate = F, to_janno = F)
  # extract relevant information
  fam_ind <- fam[[2]]
  fam_group <- fam[[1]]
  janno_ind <- janno[["Individual_ID"]]
  janno_group <- sapply(strsplit(janno[["Group_Name"]], ";"), `[[`, 1)
  # check if equal
  if (!all(fam_ind == janno_ind) || !all(fam_group == janno_group)) {
    cli::cli_alert_danger(paste(
      "Individual and Group IDs in .fam file and .janno file must",
      "be identical and in the same order."
    ))
    return(FALSE)
  }
  return(TRUE)
}

validate_bib_janno_interaction <- function(bib_file, janno_file) {
  
}
