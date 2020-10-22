#' @rdname cli_modules
#' @export
merge_module <- function(input_file, output_directory, log_directory = tempdir()) {
  # input check and prep
  checkmate::assert_file_exists(input_file, access = "r")
  merge_read_package_list(input_file) -> list_of_packages
  checkmate::assert_directory_exists(list_of_packages, access = "r")
  checkmate::assert_directory_exists(log_directory, access = "rw")
  # validate input packages
  validation_result <- validate_module(list_of_packages)
  if (validation_result == 1) {
    cli::cli_alert_danger("Can't merge broken packages.")
    return(1)
  } else if (validation_result == 2) {
    cli::cli_alert_info("Merging will be attempted anyway.")
  }
  # create output directory
  if (dir.exists(output_directory)) {
    cli::cli_alert_danger("Output directory already exists.")
    return(1)
  } else {
    dir.create(output_directory, recursive = T)
  }
  checkmate::assert_directory_exists(output_directory, access = "rw")
  # start message
  merge_start_message(input_file, output_directory, log_directory)
  merge_print_packages(list_of_packages)
  # process merge
  output_files_name <- "poseidon2_merged"
  create_new_POSEIDON_yml_file(output_files_name, output_directory)
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
    return(2)
  }
  new_bib_file <- file.path(output_directory, "LITERATURE.bib")
  bib_files_read <- lapply(list_of_bib_files, function(x) {readLines(x)} )
  writeLines(unlist(bib_files_read), con = new_bib_file)
  cli::cli_alert_success(new_bib_file)
}

merge_plink_merge <- function(plink_merge_file, plink_order_file, output_directory, output_files_name, log_directory) {
  cli::cli_alert_info("Run plink...")
  command <- paste0(
    'plink1.9',
    ' --merge-list ', plink_merge_file,
    ' --make-bed ', 
    ' --indiv-sort f ', plink_order_file,
    ' --keep-allele-order ',
    ' --out ', file.path(output_directory, output_files_name),
    ' && mv ', paste0(file.path(output_directory, output_files_name), '.log'), 
    ' ', 
    file.path(log_directory, 'plink.log')
  )
  cat(command)
  cat("\n\n")
  system(command)
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
  readr::write_delim(concat_first_two_columns, file = plink_order_file, delim = " ", col_names = FALSE)
  cli::cli_alert_success(plink_order_file)
  return(plink_order_file)
}

merge_concat_janno_files <- function(list_of_packages, output_directory, output_files_name) {
  cli::cli_alert_info("Merge janno files...")
  list_of_janno_files <- list.files(list_of_packages, pattern = ".janno", full.names = T)
  list_of_janno_tables <- lapply(list_of_janno_files, function(janno) {
    suppressMessages(readr::read_tsv(janno, col_types = readr::cols(.default = "c")))
  })
  new_janno <- do.call(rbind, list_of_janno_tables)
  new_janno_file <- paste0(file.path(output_directory, output_files_name), ".janno")
  readr::write_tsv(new_janno, file = new_janno_file)
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
