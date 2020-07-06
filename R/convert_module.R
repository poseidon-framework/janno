#' @rdname cli_modules
#' @export
convert_module <- function(output_format, input_package, output_directory, log_directory) {
  convert_start_message(output_format, input_package, output_directory, log_directory)
  #validate_module(list_of_packages)
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  if (output_format == "eigenstrat") {
    convert_ped2eig(input_package, output_directory, log_directory)
  } else {
    stop("Unknown output format.")
  }
}

convert_ped2eig <- function(input_package, output_directory, log_directory) {
  cat("Converting plink files to eigenstrat format...\n")
  bed_file <- list.files(input_package, pattern = ".bed", full.names = T)
  bim_file <- list.files(input_package, pattern = ".bim", full.names = T)
  fam_file <- list.files(input_package, pattern = ".fam", full.names = T)
  return_file_name <- sub(".bed", "", basename(bed_file))
  pedind_file <- convert_create_pedind_file(fam_file, log_directory)
  par_file <- convert_create_par_file(bed_file, bim_file, pedind_file, output_directory, return_file_name, log_directory)
  convert_start_ped2eig_run(par_file, log_directory)
}

convert_start_ped2eig_run <- function(par_file, log_directory) {
  cat("\nYou can trigger the actual conversion now with\n=> ")
  cat(paste(
    'sbatch -p short -c 1 --mem=2000 -J poseidon_convert',
    '-o', file.path(log_directory, 'poseidon2_%j.out'),
    '-e', file.path(log_directory, 'poseidon2_%j.err'),
    '--wrap=',
    '"',
    'convertf',
    '-p', par_file,
    '>', file.path(log_directory, "convert.log"),
    '"'
  ), "\n")
}

convert_create_par_file <- function(bed_file, bim_file, pedind_file, output_directory, return_file_name, log_directory) {
  cat("Create .par file...\n")
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
  cat("=>", par_file, "\n")
  return(par_file)
}

convert_create_pedind_file <- function(fam_file, log_directory) {
  cat("Create .pedind file...\n")
  fam_table <- suppressMessages(readr::read_delim(fam_file, delim = " ", col_names = F))
  pedind_file <- file.path(log_directory, "for_conversion.pedind")
  readr::write_delim(fam_table[,c(1:5, 1)], path = pedind_file, delim = " ", col_names = F)
  return(pedind_file)
}

convert_start_message <- function(output_format, input_package, output_directory, log_directory) {
  cat(
    "convert => Converts data in poseidon directories\n",
    "\n",
    "Output format:", output_format, "\n",
    "Input package:", input_package, "\n",
    "Output directory:", output_directory, "\n",
    "Log file directory:", log_directory, "\n",
    "\n"
  )
}
