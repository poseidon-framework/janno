convert_ped2eig <- function(input_package, output_directory, log_directory) {
  cli::cli_alert_info("Converting plink files to eigenstrat format...")
  # copy janno file
  janno_file <- list.files(input_package, pattern = ".janno", full.names = T)
  file.copy(janno_file, file.path(output_directory, basename(janno_file)))
  # find genetic data files
  bed_file <- list.files(input_package, pattern = ".bed", full.names = T)
  bim_file <- list.files(input_package, pattern = ".bim", full.names = T)
  fam_file <- list.files(input_package, pattern = ".fam", full.names = T)
  return_file_name <- sub(".bed", "", basename(bed_file))
  # create .pedind and .par file
  pedind_file <- convert_create_pedind_file(fam_file, log_directory)
  par_file <- convert_create_par_file(bed_file, bim_file, pedind_file, output_directory, return_file_name, log_directory)
  # prepare conversion command
  convert_start_ped2eig_run(par_file, log_directory)
}

convert_start_ped2eig_run <- function(par_file, log_directory) {
  cli::cli_alert_info("Run convertf...")
  command <- paste0(
    'convertf ',
    '-p ', par_file,
    ' > ', file.path(log_directory, "convert.log")
  )
  cat(command)
  cat("\n\n")
  system(command)
  cat("\n")
}

convert_create_par_file <- function(bed_file, bim_file, pedind_file, output_directory, return_file_name, log_directory) {
  cli::cli_alert_info("Create .par file...")
  par_file <- file.path(log_directory, "convertf.par")
  writeLines(
    c(
      paste("genotypename:", bed_file),
      paste("snpname:", bim_file),
      paste("indivname:", pedind_file),
      "outputformat: EIGENSTRAT",
      paste("genotypeoutname:", paste0(file.path(output_directory, return_file_name), ".geno")),
      paste("snpoutname:", paste0(file.path(output_directory, return_file_name), ".snp")),
      paste("indivoutname:", paste0(file.path(output_directory, return_file_name), ".ind")),
      "familynames: NO"
    ),
    con = par_file
  )
  cli::cli_alert_success(par_file)
  return(par_file)
}

convert_create_pedind_file <- function(fam_file, log_directory) {
  cli::cli_alert_info("Create .pedind file...")
  fam_table <- suppressMessages(readr::read_delim(fam_file, delim = " ", col_names = F))
  pedind_file <- file.path(log_directory, "for_conversion.pedind")
  readr::write_delim(fam_table[,c(1:5, 1)], file = pedind_file, delim = " ", col_names = F)
  cli::cli_alert_success(pedind_file)
  return(pedind_file)
}
