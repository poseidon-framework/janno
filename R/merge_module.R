merge_module <- function(input_file, output_directory, log_directory) {
  
  list_of_packages <- merge_read_package_list(input_file)
  
  merge_start_message(input_file, output_directory, log_directory)
  merge_print_packages(list_of_packages)
  
  #validate_module(list_of_packages)

  plink_merge_file <- merge_create_plink_merge_input_file(list_of_packages, log_directory)
  
  
    
}

plink_merge <- function() {
  
}

merge_create_order_file_from_fam_files <- function() {
}

merge_create_plink_merge_input_file <- function(list_of_packages, log_directory) {
  cat("Creating input file for plink merge...\n")
  list_of_file_tripels <- sapply(list_of_packages, function(package) {
    file_list <- list.files(package, full.names = T)
    relevant_files <- grep(".bed|.bim|.fam", file_list, value = T)
    paste(relevant_files, collapse = " ")
  }, USE.NAMES = F)
  plink_merge_file <- file.path(log_directory, "merge_list_file")
  writeLines(list_of_file_tripels, con = plink_merge_file)
  return(plink_merge_file)
}

merge_print_packages <- function(list_of_packages) {
  cat("Packages to be merged:\n\n")
  cat(paste(list_of_packages, collapse = "\n"))
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
