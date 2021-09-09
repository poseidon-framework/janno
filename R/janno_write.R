#' @rdname janno
#' 
#' @param remove_source_file_column Should the special column source_file be removed
#' from the janno object before writing?
#' 
#' @export
write_janno <- function(
  x, path, remove_source_file_column = TRUE
) {
  checkmate::assert_class(x, classes = "janno")
  if (remove_source_file_column) {
    x <- x %>% dplyr::select(-.data[["source_file"]])
  }
  readr::write_tsv(
    flatten_janno(x),
    file = path,
    na = "n/a"
  )
}

#' @rdname janno
#' 
#' @details \code{flatten_janno} transforms list columns to string lists (separated by ;)
#' 
#' @export
flatten_janno <- function(x) {
  checkmate::assert_class(x, classes = "janno")
  x %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::vars_select_helpers$where(is.list),
        flatten_vector_list_column
      )
    ) %>%
    tibble::as_tibble()
}

flatten_vector_list_column <- function(x) {
  if (is.vector(x[[1]])) {
    purrr::map_chr(x, function(y) { 
      if (all(is.na(y))) {
        "n/a"
      } else {
        paste(y, collapse = ";")
      }
    })
  } else {
    rep("n/a", length(x))
  }
}
