#' @rdname cli_modules
#' @export
merge_module <- function(input_file, output_directory, log_directory) {
  # input check and prep
  checkmate::assert_file_exists(input_file, access = "r")
  merge_read_package_list(input_file) -> list_of_packages
  checkmate::assert_directory_exists(list_of_packages, access = "r")
  checkmate::assert_directory_exists(log_directory, access = "rw")
  #validate_module(list_of_packages)
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  checkmate::assert_directory_exists(output_directory, access = "rw")
  # start message
  merge_start_message(input_file, output_directory, log_directory)
  merge_print_packages(list_of_packages)
  # process merge
  output_files_name <- "poseidon2_merged"
  merge_create_new_POSEIDON_yml_file(output_files_name, output_directory)
  merge_concat_janno_files(list_of_packages, output_directory, output_files_name)
  merge_concat_LITERATURE_bib_files(list_of_packages, output_directory)
  merge_create_plink_merge_input_file(list_of_packages, log_directory) -> plink_merge_file
  merge_create_order_file_from_fam_files(list_of_packages, log_directory) -> plink_order_file
  merge_plink_merge(plink_merge_file, plink_order_file, output_directory, output_files_name, log_directory)
}

merge_concat_LITERATURE_bib_files <- function(list_of_packages, output_directory) {
  cli::cli_alert_info("Concat LITERATURE.bib files...")
  list_of_bib_files <- list.files(list_of_packages, pattern = "*.bib", full.names = T)
  if (length(list_of_bib_files) == 0) {
    cli::cli_alert_info("No *.bib files found")
    return()
  }
  new_bib_file <- file.path(output_directory, "LITERATURE.bib")
  bib_files_read <- lapply(list_of_bib_files, function(x) {readLines(x)} )
  writeLines(unlist(bib_files_read), con = new_bib_file)
  cli::cli_alert_success(new_bib_file)
}

merge_create_new_POSEIDON_yml_file <- function(output_files_name, output_directory) {
  cli::cli_alert_info("Create new POSEIDON.yml file...")
  new_poseidon_yml <- file.path(output_directory, "POSEIDON.yml")
  writeLines(
    c(
      "poseidonVersion:",
      "title:",
      "description:",
      "contributor:",
      "  name:",
      "  email:",
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

merge_plink_merge <- function(plink_merge_file, plink_order_file, output_directory, output_files_name, log_directory) {
  cat("\n")
  cli::cli_alert_info("You can trigger the merging now with")
  cat(paste0(
    'sbatch -p short -c 4 --mem=10000 -J poseidon2_merge ',
    '-o ', file.path(log_directory, 'poseidon2_%j.out '),
    '-e ', file.path(log_directory, 'poseidon2_%j.err '),
    '--wrap=',
    '"',
      'plink ',
      '--merge-list ', plink_merge_file,
      ' --make-bed ', 
      '--indiv-sort f ', plink_order_file,
      ' --keep-allele-order ',
      '--out ', file.path(output_directory, output_files_name),
      ' && mv ', paste0(file.path(output_directory, output_files_name), '.log'), ' ', file.path(log_directory, 'plink.log'),
    '"'
  ))
  cat("\n")
}

merge_create_order_file_from_fam_files <- function(list_of_packages, log_directory) {
  cli::cli_alert_info("Merge fam files to get order file...")
  list_of_fam_files <- list.files(list_of_packages, pattern = ".fam", full.names = T)
  list_of_fam_tables <- lapply(list_of_fam_files, function(fam) {
    suppressMessages(readr::read_delim(fam, delim = " ", col_names = F))
  })
  concat_first_two_columns <- do.call(rbind, list_of_fam_tables)[,1:2]
  plink_order_file <- file.path(log_directory, "poseidon2_merge_plink_order_file.txt")
  readr::write_delim(concat_first_two_columns, path = plink_order_file, delim = " ", col_names = FALSE)
  cli::cli_alert_success(plink_order_file)
  return(plink_order_file)
}

merge_concat_janno_files <- function(list_of_packages, output_directory, output_files_name) {
  cli::cli_alert_info("Merge janno files...")
  list_of_janno_files <- list.files(list_of_packages, pattern = ".janno", full.names = T)
  list_of_janno_tables <- lapply(list_of_janno_files, function(janno) {
    suppressMessages(readr::read_tsv(janno))
  })
  new_janno <- do.call(rbind, list_of_janno_tables)
  new_janno_file <- paste0(file.path(output_directory, output_files_name), ".janno")
  readr::write_tsv(new_janno, path = new_janno_file)
  cli::cli_alert_success(new_janno_file)
}

merge_create_plink_merge_input_file <- function(list_of_packages, log_directory) {
  cli::cli_alert_info("Creating input file for plink merge...")
  list_of_file_tripels <- sapply(list_of_packages, function(package) {
    file_list <- list.files(package, pattern = ".bed|.bim|.fam", full.names = T)
    paste(file_list, collapse = " ")
  }, USE.NAMES = F)
  plink_merge_file <- file.path(log_directory, "poseidon2_merge_plink_input_file.txt")
  writeLines(list_of_file_tripels, con = plink_merge_file)
  cli::cli_alert_success(plink_merge_file)
  return(plink_merge_file)
}

merge_print_packages <- function(list_of_packages) {
  cli::cli_alert_info("Packages to be merged:")
  sapply(list_of_packages, function(x) { cli::cli_alert(x) })
}

merge_start_message <- function(input_file, output_directory, log_directory) {
  cli::cli_h1("merge => Merges multiple poseidon packages")
  cli::cli_alert(paste0("Package list:\t\t", input_file))
  cli::cli_alert(paste0("Output directory:\t", output_directory))
  cli::cli_alert(paste0("Log file directory:\t", log_directory))
}

merge_read_package_list <- function(input_file) {
  lines_in_file <- readLines(input_file)
  lines_in_file[!grepl("^#", lines_in_file) & !lines_in_file == ""]
}
