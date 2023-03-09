get_janno_file_paths <- function(path) {
  lapply(
    path, function(x) {
      if (strsplit(x, "\\.") %>% unlist %>% utils::tail(1) == "janno") {
        checkmate::assert_file_exists(x)
        return(x)
      } else {
        checkmate::assert_directory_exists(x)
        return(list.files(x, pattern = "\\.janno", full.names = T, recursive = T))
      }
    }
  ) %>% unlist()
}

get_last_two_elements_of_path <- function(x) {
  if (is.na(x)) {
    x
  } else {
    file.path(
      basename(dirname(x)),
      basename(x)
    )
  }
}
