#' @rdname cli_modules
#' @export
merge_module <- function(input_file, output_directory, log_directory) {
  merge_start_message(input_file, output_directory, log_directory)
  list_of_packages <- merge_read_package_list(input_file)
  merge_print_packages(list_of_packages)
  #validate_module(list_of_packages)
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  output_files_name <- "poseidon2_merged"
  merge_concat_janno_files(list_of_packages, output_directory, output_files_name)
  plink_merge_file <- merge_create_plink_merge_input_file(list_of_packages, log_directory)
  plink_order_file <- merge_create_order_file_from_fam_files(list_of_packages, log_directory)
  plink_merge(plink_merge_file, plink_order_file, output_directory, output_files_name, log_directory)
}

plink_merge <- function(plink_merge_file, plink_order_file, output_directory, output_files_name, log_directory) {
  cat("You can trigger the merging now with\n$ ")
  cat(paste(
    'sbatch -p short -c 4 --mem=10000 -J poseidon2_merge',
    '-o', file.path(log_directory, 'poseidon2_%j.out'),
    '-e', file.path(log_directory, 'poseidon2_%j.err'),
    '--wrap=',
    '"',
      'plink',
      '--merge-list', plink_merge_file,
      '--make-bed', 
      '--indiv-sort', plink_order_file,
      '--keep-allele-order',
      '--out', file.path(output_directory, output_files_name),
      '&& mv', paste0(file.path(output_directory, output_files_name), '.log'), file.path(log_directory, 'plink.log'),
    '"'
  ))
}

merge_create_order_file_from_fam_files <- function(list_of_packages, log_directory) {
  cat("Merge fam files to get order file...\n")
  list_of_fam_files <- list.files(list_of_packages, pattern = ".fam", full.names = T)
  list_of_fam_tables <- lapply(list_of_fam_files, function(fam) {
    suppressMessages(readr::read_delim(fam, delim = " ", col_names = F))
  })
  concat_first_two_columns <- do.call(rbind, list_of_fam_tables)[,1:2]
  plink_order_file <- file.path(log_directory, "poseidon2_merge_plink_order_file.txt")
  readr::write_tsv(concat_first_two_columns, path = plink_order_file, col_names = FALSE)
  return(plink_order_file)
}

merge_concat_janno_files <- function(list_of_packages, output_directory, output_files_name) {
  cat("Merge janno files...\n")
  list_of_janno_files <- list.files(list_of_packages, pattern = ".janno", full.names = T)
  list_of_janno_tables <- lapply(list_of_janno_files, function(janno) {
    suppressMessages(readr::read_tsv(janno))
  })
  new_janno <- do.call(rbind, list_of_janno_tables)
  readr::write_tsv(new_janno, path = paste0(file.path(output_directory, output_files_name), ".janno"))
}

merge_create_plink_merge_input_file <- function(list_of_packages, log_directory) {
  cat("Creating input file for plink merge...\n")
  list_of_file_tripels <- sapply(list_of_packages, function(package) {
    file_list <- list.files(package, pattern = ".bed|.bim|.fam", full.names = T)
    paste(file_list, collapse = " ")
  }, USE.NAMES = F)
  plink_merge_file <- file.path(log_directory, "poseidon2_merge_plink_input_file.txt")
  writeLines(list_of_file_tripels, con = plink_merge_file)
  return(plink_merge_file)
}

merge_print_packages <- function(list_of_packages) {
  cat("Packages to be merged:\n\n")
  cat(paste(list_of_packages, collapse = "\n"))
  cat("\n\n")
}

merge_start_message <- function(input_file, output_directory, log_directory) {
  cat(
    "merge => Merges multiple poseidon directories\n",
    "\n",
    "Input file with package list:", input_file, "\n",
    "Output directory:", output_directory, "\n",
    "Log file directory:", log_directory, "\n",
    "\n"
  )
}

merge_read_package_list <- function(input_file) {
  lines_in_file <- readLines(input_file)
  lines_in_file[!grepl("^#", lines_in_file) & !lines_in_file == ""]
}
