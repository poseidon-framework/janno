#' @rdname janno
#' @export
read_janno <- function(
  path,
  to_janno = TRUE,
  validate = TRUE
) {
  # input checks and search for janno files
  if (strsplit(path, "\\.") %>% unlist %>% utils::tail(1) == "janno") {
    checkmate::assert_file_exists(path)
    janno_files <- path
  } else {
    checkmate::assert_directory_exists(path)
    janno_files <- list.files(path, pattern = "\\.janno", full.names = T, recursive = T)
  }
  # read files
  if (validate) {
    message("Running validation")
    validation_result <- validate_janno(janno_files)
    if (nrow(validation_result) > 0) {
      print(validation_result)
      message(paste0(
        "Run validate_janno(\"", path, "\") to get the table of issues\n"
      ))
    } else {
      message("No issues with these janno files\n")
    }
  }
  lapply(janno_files, read_one_janno, to_janno) %>% dplyr::bind_rows()
}

read_one_janno <- function(x, to_janno) {
  input_file <- readr::read_tsv(x, col_types = readr::cols(.default = readr::col_character()), na = "n/a") 
  if (to_janno) {
    as.janno(input_file)
  } else {
    input_file
  }
}
