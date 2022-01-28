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
  lapply(janno_file_paths, read_one_janno, to_janno) %>% dplyr::bind_rows()
}

read_one_janno <- function(x, to_janno) {
  input_file <- readr::read_tsv(
    x, 
    col_types = readr::cols(.default = readr::col_character()), 
    na = c("", "n/a")
  )
  if (to_janno) {
    as.janno(input_file, source_janno_file = x)
  } else {
    input_file
  }
}
