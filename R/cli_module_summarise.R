#' @rdname cli_modules
#' @export
summarise_module <- function(input_janno_file_or_packages) {
  # input check and prep
  checkmate::assert_character(input_janno_file_or_packages, any.missing = FALSE, all.missing = FALSE, min.len = 1)
  type <- janno_or_package(input_janno_file_or_packages)
  # loop through all input elements (packages or janno files)
  list_of_janno_files <- list()
  for (i in 1:length(input_janno_file_or_packages)) {
    # find janno file
    if (type[i] == "janno") {
      janno_file <- input_janno_file_or_packages[i]
    } else if (type[i] == "package") {
      janno_file <- list.files(input_janno_file_or_packages[i], pattern = "\\.janno", full.names = T)
    }
    list_of_janno_files[[i]] <- read_janno(janno_file)
    cat("\n")
  }
  superjanno <- dplyr::bind_rows(list_of_janno_files)
  print(superjanno)
  return(0)
}
