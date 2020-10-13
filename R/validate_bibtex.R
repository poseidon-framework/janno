validate_bibtex <- function(bib_file) {
  cli::cli_alert_info(basename(bib_file))
  tryCatch({
    bibtex::read.bib(bib_file)
    TRUE
  }, warning = function(w) {
    cli::cli_alert_danger(paste(
      "Test reading of .bib file showed an issue:",
      conditionMessage(w)
    ))
    return(FALSE)
  }, error = function(e) {
    cli::cli_alert_danger(paste(
      "Test reading of .bib file showed an issue:",
      conditionMessage(e)
    ))
    return(FALSE)
  })
}
