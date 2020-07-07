#' @rdname cli_modules
#' @export
validate_module <- function(input_janno_file_or_packages) {
  type <- janno_or_package(input_janno_file_or_packages)
  validate_start_message(input_janno_file_or_packages, type)
  for (i in 1:length(input_janno_file_or_packages)) {
    if (type[i] == "janno") {
      validate_janno(input_janno_file_or_packages[i])
    } else if (type[i] == "package") {
      validate_package(input_janno_file_or_packages[i])
    }
  }
}

validate_start_message <- function(input_janno_file_or_packages, type) {
  cat(
    "\n",
    "*******************************************************\n",
    "validate => Validates janno files and poseidon packages\n",
    "*******************************************************\n",
    "\n",
    paste(type, input_janno_file_or_packages, sep = "\t=> ", collapse = "\n"),
    "\n\n",
    sep = ""
  )
}

janno_or_package <- function(input_janno_file_or_packages) {
  ifelse(grepl(".janno", input_janno_file_or_packages), "janno", "package")
}

validate_janno <- function(input_janno) {
  
}

validate_package <- function(input_package) {
  
}
