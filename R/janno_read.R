#' @rdname janno
#' @export
read_janno <- function(
  path,
  to_janno = TRUE,
  validate = TRUE
) {
  # input checks and search for janno files
  if (validate) { informative_validation(path) }
  # read files
  janno_file_paths <- get_janno_file_paths(path)
  message("Reading .janno files...")
  purrr::map_dfr(janno_file_paths, read_one_janno, to_janno)
}

read_one_janno <- function(x, to_janno) {
  # message(x)
  input_file <- readr::read_tsv(
    x, 
    col_types = readr::cols(.default = readr::col_character()), 
    na = c("", "n/a"),
    name_repair = "unique_quiet"
  ) %>%
    # remove columns without a header
    dplyr::select(!tidyselect::starts_with("..."))
  if (to_janno) {
    as.janno(input_file, source_janno_file = x)
  } else {
    input_file
  }
}
