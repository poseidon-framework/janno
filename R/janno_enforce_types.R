enforce_types <- function(x, suppress_na_introduced_warnings = TRUE) {
  UseMethod("enforce_types")
}

enforce_types.default <- function(x, suppress_na_introduced_warnings = TRUE) {
  stop("x is not an object of class janno")
}

enforce_types.janno <- function(x, suppress_na_introduced_warnings = TRUE) {
  
  res <- Map(
    apply_col_types,
    as.list(x), 
    names(x), 
    suppress_na_introduced_warnings = suppress_na_introduced_warnings
  )
  res <- tibble::as_tibble(res)
  
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
  }
  # transform variable, if trans function is available
  # assumes multi == TRUE and multi == FALSE is handled below
  if (!is.null(col_trans_function)) {
    if (suppress_na_introduced_warnings) {
      withCallingHandlers({
        res <- lapply(res, function(y) { col_trans_function(y) })
      }, warning = na_introduced_warning_handler
      )
    } else 
      res <- lapply(res, function(y) { col_trans_function(y) })
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
