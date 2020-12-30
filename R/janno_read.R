#' @rdname janno
#' @export
read_janno <- function(
  path,
  to_janno = TRUE,
  validate = TRUE
) {
  # input checks and search for janno files
  janno_file_paths <- get_janno_file_paths(path)
  # read files
  if (validate) {
    validation_result <- validate_janno(janno_file_paths)
    if (nrow(validation_result) > 0) {
      print(validation_result)
      message(paste0(
        "Run this to get the table of issues: \nposeidonR::validate_janno(",
        paste(utils::capture.output(dput(path)), collapse = ""),
        ")\n"
      ))
    } else {
      message("No issues with these .janno files")
    }
  }
  message("Reading .janno files...")
  lapply(janno_file_paths, read_one_janno, to_janno) %>% dplyr::bind_rows()
}

read_one_janno <- function(x, to_janno) {
  input_file <- readr::read_tsv(
    x, 
    col_types = readr::cols(.default = readr::col_character()), 
    na = "n/a"
  )
  if (to_janno) {
    as.janno(input_file, source_janno_file = x)
  } else {
    input_file
  }
}
