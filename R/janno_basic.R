#' @name janno
#' @title \strong{janno}
#'
#' @description ...
#'
#' @param x an object
#' @param file character. Path to a .janno file
#' @param validate logical. Should the janno file be validated upon reading
#' @param to_janno logical. Should the read function transform the input file to a janno object
#' @param only_header logical. Should only the header be printed.
#' @param ... further arguments passed to or from other methods
#'
#' @rdname janno
#'
NULL

#' @rdname janno
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

#' @rdname janno
#' @export
read_janno <- function(
  file = "test_data/1_good_test_package/file1.janno", 
  validate = TRUE, 
  to_janno = TRUE
) {
  # input checks
  checkmate::assert_file_exists(file)
  # validation
  if (validate) {
  validation_result <- validate_janno(file)
    if (validation_result == 1) {
      stop("Input janno file has significant shortcomings and can't be loaded as a janno object in R.")
    } else if (validation_result == 2) {
      message("Input janno file has shortcomings, but loading is attempted anyway.")
    }
  }
  # read file
  input_file <- readr::read_tsv(file, col_types = readr::cols(.default = readr::col_character()), na = "n/a") 
  if (to_janno) {
    as.janno(input_file)
  } else {
    input_file
  }
}

#' @rdname janno
#' @export
format.janno <- function(x, ...) {
  out_str <- list()
  # compile information
  out_str$individuals <- paste(nrow(x), "\tIndividuals")
  groups <- unique(sapply(x[["Group_Name"]], function(y) {y[1]}))
  out_str$groups <- print_number_and_name(groups, "Populations")
  out_str$sep1 <- "--"
  out_str$countries <- print_number_and_name(unique(x[["Country"]]), "Countries")
  out_str$sites <- print_number_and_name(unique(x[["Site"]]), "Sites")
  out_str$age <- paste0(
    "\tMean age BC/AD: ",
    round(mean(x[["Date_BC_AD_Median"]], na.rm = T)), " \u00B1 ",
    round(stats::sd(x[["Date_BC_AD_Median"]], na.rm = T))
  )
  out_str$publications <- print_number_and_name(unique(x[["Publication_Status"]]), "Publications")
  out_str$sep2 <- "--"
  out_str$endogenous <- print_min_mean_max(x[["Endogenous"]], "% endogenous human DNA")
  out_str$snps1240K <- print_min_mean_max(x[["Nr_autosomal_SNPs"]], "Number of SNPs on 1240K")
  out_str$coverage1240K <- print_min_mean_max(x[["Coverage_1240K"]], "Mean coverage of 1240K SNPs")
  out_str$udg <- print_table(x[["UDG"]], "UDG treatment")
  out_str$library_built <- print_table(x[["Library_Built"]], "Library building process")
  # merge information
  return_value <- paste(out_str, collapse = "\n", sep = "")
  invisible(return_value)
}

print_number_and_name <- function(x, name) {
  show_number <- if (length(x) > 3) {3} else {length(x)}
  paste0(
    length(x), 
    paste0("\t", name, ": "),
    paste0(x[1:show_number], collapse = ", "),
    if (length(x) > 3) {", ..."}
  )
}

print_table <- function(x, name) {
  tab <- table(x, useNA = "ifany")
  string_tab <- paste(names(tab), tab, sep = " \u2192 ", collapse = ", ")
  paste0(name, ": ", string_tab)
}

print_min_mean_max <- function(x, name) {
  if (!all(is.na(x))) {
    paste0(
      name, ": \t", 
      "min \u2192 ", round(min(x, na.rm = T), 3), ", ",
      "mean \u2192 ", round(mean(x, na.rm = T), 3), ", ",
      "max \u2192 ", round(max(x, na.rm = T), 3), " "
    )
  } else {
    paste0(name, ": \t", "min \u2192 NA, mean \u2192 NA, max \u2192 NA")
  }
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
