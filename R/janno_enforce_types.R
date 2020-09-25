#' enforce_types
#' 
#' @param x object of class janno.
#' @param suppress_na_introduced_warnings suppress warnings caused by data removal in
#' type transformation due to wrong database entries (such as text in a number column)
#' 
#' @export
#' @rdname enforce_types
enforce_types <- function(x, suppress_na_introduced_warnings = TRUE) {
  UseMethod("enforce_types")
}

#' @rdname enforce_types
#' @export
enforce_types.default <- function(x, suppress_na_introduced_warnings = TRUE) {
  stop("x is not an object of class janno")
}

#' @rdname enforce_types
#' @export
enforce_types.janno <- function(x, suppress_na_introduced_warnings = TRUE) {
  
  res <- purrr::map2_df(
    x, 
    names(x), 
    .f = apply_col_types,
    suppress_na_introduced_warnings = suppress_na_introduced_warnings
  )
  
  # special treatment of list columns, because purrr::map2_df can't deal with them :-(
  string_list_cols <- hash::keys(janno_column_name_data_type)[
    grep("String list", hash::values(janno_column_name_data_type))
    ]
  integer_list_cols <- hash::keys(janno_column_name_data_type)[
    grep("Integer list", hash::values(janno_column_name_data_type))
    ]
  for (i in string_list_cols) {
    # TODO: should check if type is already character list column
    if (is.character(res[[i]])) {
      res[[i]] <- as_string_list_column(res[[i]])
    }
  }
  for (i in integer_list_cols) {
    # TODO: should check if type is already integer list column
    if (is.character(res[[i]])) {
      res[[i]] <- as_integer_list_column(res[[i]])
    }
  }
  
  return(res %>% tibble::new_tibble(., nrow = nrow(.), class = "janno"))
}

apply_col_types <- function(col_data, col_name, suppress_na_introduced_warnings) {
  res <- col_data
  # lookup type for variable in hash
  col_type <- hash::values(janno_column_name_data_type, col_name)
  # get trans function
  col_trans_function <- string_to_as(col_type)
  # transform variable, if trans function is available
  if (!is.null(col_trans_function)) {
    if (suppress_na_introduced_warnings) {
      withCallingHandlers({
        res <- col_trans_function(res) 
      }, warning = na_introduced_warning_handler
      )
    } else 
      res <- col_trans_function(res) 
  }
  return(res)
}

string_to_as <- function(x) {
  switch(
    x,
    "String" = as.character,
    "String choice" =  as.character,
    "String list" = as.character,
    "Char choice" = as.character,
    "Integer" = as.integer, 
    "Integer list" = as.character,
    "Float" = as.numeric,
    NA
  )
}

na_introduced_warning_handler <- function(x) {
  if (any(
    grepl("NAs introduced by coercion", x)
  )) {
    invokeRestart("muffleWarning")
  }
}

as_string_list_column <- function(x) {
  as_list_column(x)
}

as_integer_list_column <- function(x) {
  lapply(as_list_column(x), as.integer)
}

as_list_column <- function(x) {
  as.list(ifelse(
    grepl(";", x),
    strsplit(x, split = ";"),
    x
  ))
}
