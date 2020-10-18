#' @rdname cli_modules
#' @export
summarise_module <- function(input_janno_file_or_packages, validate = F) {
  # input check and prep
  checkmate::assert_character(input_janno_file_or_packages, any.missing = FALSE, all.missing = FALSE, min.len = 1)
  type <- janno_or_package(input_janno_file_or_packages)
  # loop through all input elements (packages or janno files)
  list_of_janno_files <- list()
  for (i in 1:length(input_janno_file_or_packages)) {
    # find janno file
    if (type[i] == "janno") {
      list_of_janno_files[[i]] <- input_janno_file_or_packages[i]
    } else if (type[i] == "package") {
      list_of_janno_files[[i]] <- list.files(input_janno_file_or_packages[i], pattern = "\\.janno", full.names = T, recursive = T)
    }
  }
  all_janno_files <- unique(unlist(list_of_janno_files))
  for (i in all_janno_files) {cli::cli_alert_info(i)}
  list_of_janno_tables <- lapply(all_janno_files, function(y) {read_janno(y, validate = validate)})
  superjanno <- dplyr::bind_rows(list_of_janno_tables)
  cat("\n")
  print(superjanno, only_header = T)
  return(0)
}
