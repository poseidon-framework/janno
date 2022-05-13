#' @rdname janno
#' 
#' @param remove_source_file_column Should the special column source_file be removed
#' from the janno object before writing?
#' 
#' @export
write_janno <- function(
  x, path, remove_source_file_column = TRUE
) {
  UseMethod("write_janno")
}

#' @export
write_janno.default <- function(
  x, path, remove_source_file_column = TRUE
) {
  stop("x is not an object of class janno")
}

#' @export
write_janno.janno <- function(
  x, path, remove_source_file_column = TRUE
) {
  if (remove_source_file_column) {
    x <- x %>% dplyr::select(-.data[["source_file"]])
  }
  readr::write_tsv(
    flatten_janno(x),
    file = path,
    na = "n/a",
    quote = "none",
    escape = "none"
  )
}

#' @rdname janno
#' 
#' @details \code{flatten_janno} transforms list columns to string lists (separated by ;)
#' 
#' @export
flatten_janno <- function(x) {
  UseMethod("flatten_janno")
}

#' @export
flatten_janno.default <- function(x) {
  stop("x is not an object of class janno")
}

#' @export
flatten_janno.janno <- function(x) {
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
