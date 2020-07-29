#' as.janno
#'
#' @param x data.frame. Object to be converted to a janno object
#' @param ... further arguments passed to or from other methods
#' 
#' @export
as.janno <- function(x, ...) {
  
  # input checks
  checkmate::assert_data_frame(x)
  check_if_all_columns_present(x)
  
  # do the actual conversion!
  x %>%
    #poseidon2::order_variables() %>%
    tibble::new_tibble(., nrow = nrow(.), class = "janno") %>%
    poseidon2::enforce_types() %>%
    tibble::new_tibble(., nrow = nrow(.), class = "janno")

}

check_if_all_columns_present <- function(x) {
  if ( !all(janno_column_names %in% names(x)) ) {
    stop("columns missing")
  }
}

#' read_janno
#'
#' @param x character. Path to a .janno file
#' @param validate logical. Should the janno file be validated upon reading
#' @param to_janno logical. Should the read function transform the input file to a janno object
#'
#' @export
read_janno <- function(
  x = "test_data/1_good_test_package/file1.janno", 
  validate = TRUE, 
  to_janno = TRUE
) {
  # input checks
  checkmate::assert_file_exists(x)
  # validation
  if (validate) {
  validation_result <- validate_janno(x)
    if (validation_result == 1) {
      stop("Input janno file has significant shortcomings and can't be loaded as a janno object in R.")
    } else if (validation_result == 2) {
      message("Input janno file has shortcomings, but loading is attempted anyway.")
    }
  }
  # read file
  input_file <- readr::read_tsv(x, col_types = readr::cols(.default = readr::col_character()), na = "n/a") 
  if (to_janno) {
    as.janno(input_file)
  } else {
    input_file
  }
}
