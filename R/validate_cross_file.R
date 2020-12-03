validate_fam_janno_interaction <- function(fam_file, janno_file) {
  # read files
  fam <- readr::read_delim(
    fam_file, 
    " ", 
    col_types = readr::cols(.default = readr::col_character()),
    col_names = F
  )
  janno <- read_janno(janno_file, to_janno = F)
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
  # read files
  bib <- bibtex::read.bib(bib_file)
  janno <- read_janno(janno_file, to_janno = F)
  # extract relevant information
  bib_keys <- names(bib)
  janno_keys <- janno[["Publication_Status"]]
  # check if equal
  if ( !all(stats::na.omit(janno_keys[janno_keys != "unpublished"]) %in% bib_keys) ) {
    cli::cli_alert_warning(paste(
      "The .bib file does not contain the literature in the janno file",
      "or the bibtex keys are different"
    ))
    return(FALSE)
  }
  return(TRUE)
}
