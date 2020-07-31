#' Get a random alphanumeric string
#'
#' @param n integer. Number of random strings
#' @param l integer. Length of random strings (number of characters)
#'
#' @export
random_alphanumeric_string <- function(n = 1, l = 7) {
  sapply(1:n, function(i) { 
    paste(
      sample(c(0:9, LETTERS, letters), l, TRUE), 
      collapse = ""
    ) 
  })
}

create_new_POSEIDON_yml_file <- function(output_files_name, output_directory) {
  cli::cli_alert_info("Create new POSEIDON.yml file...")
  new_poseidon_yml <- file.path(output_directory, "POSEIDON.yml")
  writeLines(
    c(
      "poseidonVersion:",
      "title:",
      "description:",
      "contributor:",
      "  - name:",
      "    email:",
      paste0("lastModified: ", Sys.Date()),
      "bibFile: LITERATURE.bib",
      "genotypeData:",
      "  format: PLINK",
      paste0("  genoFile: ", output_files_name, ".bed"),
      paste0("  snpFile: ", output_files_name, ".bim"),
      paste0("  indFile: ", output_files_name, ".fam"),
      paste0("jannoFile: ", output_files_name, ".janno")
    ),
    con = new_poseidon_yml
  )
  cli::cli_alert_success(new_poseidon_yml)
  cli::cli_alert_warning("Don't forget to edit it!")
}
