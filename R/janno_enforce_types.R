enforce_types <- function(x, suppress_na_introduced_warnings = TRUE) {
  
  defined_janno_columns <- x %>% dplyr::select(tidyselect::any_of(janno_column_names))
  undefined_janno_columns <- x %>% dplyr::select(-tidyselect::any_of(janno_column_names))
  
  defined_janno_columns_typed <- purrr::map2(
    as.list(defined_janno_columns), 
    names(defined_janno_columns), 
    apply_col_types,
    suppress_na_introduced_warnings = suppress_na_introduced_warnings
  )
  defined_janno_columns_typed <- tibble::as_tibble(defined_janno_columns_typed)
  
  res <- cbind(
    defined_janno_columns_typed,
    undefined_janno_columns
  )
  
  return(res %>% tibble::new_tibble(., nrow = nrow(.), class = "janno"))
}

apply_col_types <- function(col_data, col_name, suppress_na_introduced_warnings) {
  res <- col_data
  # lookup context for variable in hashes
  expected_type <- hash::values(janno_column_name_data_type, col_name)
  # get trans function
  col_trans_function <- string_to_as(expected_type)
  # split to multi if necessary
  multi <- col_name %in% janno_multi_value_columns
  already_list_column <- is.list(res)
  if (multi && !already_list_column) {
    res <- strsplit(res, ";")
    # for debugging: if (col_name == "Date_C14_Labnr") { print(res) }
    # turn empty list column entries to NULL instead of NA
    res <- purrr::map(res, function(x) {if (all(is.na(x))) { NULL } else { x }} )
  }
  # transform variable, if trans function is available
  # assumes multi == TRUE and multi == FALSE is handled below
  if (!is.null(col_trans_function)) {
    
    if (suppress_na_introduced_warnings) {
      withCallingHandlers({
        res <- purrr::map(res, function(y) { 
          if (is.null(y)) { y } else { col_trans_function(y) }
        })
      }, warning = na_introduced_warning_handler
      )
    } else {
        res <- purrr::map(res, function(y) { 
          if (is.null(y)) { y } else { col_trans_function(y) }
        })
    }
    
  }
  # unlist if not multi
  if (!multi && !already_list_column) {
    res <- unlist(res)
  }
  return(res)
}

string_to_as <- function(x) {
  switch(
    x,
    "String" = as.character,
    "Char" = as.character,
    "Integer" = as.integer,
    "Float" = as.numeric,
    NULL
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
