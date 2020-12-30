#' @name janno
#' @title \strong{janno}
#'
#' @description The \strong{janno} S3 class provides a data structure 
#' derived from \link[tibble]{tibble} to represent the content of Poseidon
#' .janno files in R.
#'
#' @param x an object
#' @param path character vector. Paths to one or multiple .janno files or 
#' directories that should be recursively searched for .janno files
#' @param to_janno logical. Should the read function transform the input 
#' file to a janno object
#' @param validate logical. Run the file validation as part of the reading 
#' process
#' @param only_header logical. Should only the header be printed.
#' @param ... further arguments passed to or from other methods
#' @param source_janno_file character. Path to the source .janno file.
#' 
#' @rdname janno
#'
NULL

#' @rdname janno
#' @export
as.janno <- function(x, source_janno_file = NA_character_, ...) {
  
  # input checks
  checkmate::assert_data_frame(x)
  check_if_all_columns_present(x)
  
  # extract last two elements of file path in source_janno_file
  source_janno_short <- get_last_two_elements_of_path(source_janno_file)
  
  # do the actual conversion!
  x %>%
    enforce_types() %>%
    tibble::new_tibble(., nrow = nrow(.), class = "janno") %>%
    tibble::add_column(source_file = source_janno_short, .before = 1)

}

check_if_all_columns_present <- function(x) {
  if ( !all(janno_column_names %in% names(x)) ) {
    stop("columns missing")
  }
}

#' @rdname janno
#' @export
format.janno <- function(x, ...) {
  out_str <- list()
  # compile information
  out_str$source_files <- print_number_and_name(unique(x[["source_file"]]), "files")
  out_str$individuals <- print_number_and_name(unique(x[["Individual_ID"]]), "individuals")
  groups <- unique(sapply(x[["Group_Name"]], function(y) {y[1]}))
  out_str$groups <- print_number_and_name(groups, "populations")
  out_str$countries <- print_number_and_name(unique(x[["Country"]]), "countries")
  out_str$age <- paste0(
    round(mean(x[["Date_BC_AD_Median"]], na.rm = T)), " \u00B1 ",
    round(stats::sd(x[["Date_BC_AD_Median"]], na.rm = T)),
    " mean age BC/AD"
  )
  out_str$publications <- print_number_and_name(unique(x[["Publication_Status"]]), "publications")
  # merge information
  return_value <- paste(out_str, collapse = "\n", sep = "")
  invisible(return_value)
}

print_number_and_name <- function(x, name) {
  show_number <- if (length(x) > 3) {3} else {length(x)}
  paste0(
    length(x), 
    paste0(" ", name)
  )
}

#' @rdname janno
#' @export
print.janno <- function(x, only_header = FALSE, ...) {
  # own format function
  cat(format(x, ...), "\n\n")
  if (!only_header) {
    # add table printed like a tibble
    x %>% `class<-`(c("tbl", "tbl_df", "data.frame")) %>% print
  }
}
