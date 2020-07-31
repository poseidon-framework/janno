#' @rdname cli_modules
#' @export
extract_module <- function(filter_file, input_package, output_directory, log_directory = tempdir()) {
  # input check and prep
  checkmate::assert_file_exists(filter_file, access = "r")
  checkmate::assert_directory_exists(input_package, access = "r")
  checkmate::assert_directory_exists(log_directory, access = "rw")
  # validate input package
  validation_result <- validate_module(input_package)
  if (validation_result == 1) {
    cli::cli_alert_danger("Can't extract from broken package.")
    return(1)
  } else if (validation_result == 2) {
    cli::cli_alert_info("Extraction will be attempted anyway.")
  }
  # create output directory
  if (dir.exists(output_directory)) {
    stop("output directory already exists")
  } else {
    dir.create(output_directory, recursive = T)
  }
  checkmate::assert_directory_exists(output_directory, access = "rw")
  # start message
  extract_start_message(filter_file, input_package, output_directory, log_directory)
  # process extraction
  output_files_name <- "poseidon2_extracted"
  bed_file_name <- sub(".bed", "", list.files(input_package, "\\.bed"))
  create_new_POSEIDON_yml_file(output_files_name, output_directory)
  filter_and_copy_janno(filter_file, input_package, output_directory, output_files_name)
  filter_and_copy_plink(bed_file_name, filter_file, input_package, output_directory, output_files_name, log_directory)
}

extract_start_message <- function(filter_file, input_package, output_directory, log_directory) {
  cli::cli_h1("extract => Extracts a subset of individuals from a poseidon package")
  cli::cli_alert(paste0("Filter file:\t\t", filter_file))
  cli::cli_alert(paste0("Input package:\t", input_package))
  cli::cli_alert(paste0("Output directory:\t", output_directory))
  cli::cli_alert(paste0("Log file directory:\t", log_directory))
}

filter_and_copy_janno <- function(filter_file, input_package, output_directory, output_files_name) {
  cli::cli_alert_info("Subsetting janno file...")
  # collect data
  janno_file <- list.files(input_package, "\\.janno", full.names = T)
  janno <- read_janno(janno_file, validate = F, to_janno = F)
  filter_list <- readr::read_delim(filter_file, " ", col_names = FALSE, col_types = readr::cols(
    readr::col_character(),
    readr::col_character()
  ))
  # filter
  janno_filtered <- janno[janno[["Individual_ID"]] %in% filter_list[[2]], ]
  # write result
  new_janno_file <- file.path(output_directory, paste0(output_files_name, ".janno"))
  readr::write_tsv(x = janno_filtered, path = new_janno_file)
  cli::cli_alert_success(new_janno_file)
}

filter_and_copy_plink <- function(bed_file_name, filter_file, input_package, output_directory, output_files_name, log_directory) {
  cat("\n")
  cli::cli_alert_info("You can trigger the plink extraction with")
  cat(paste0(
    'sbatch -p short -c 1 --mem=5000 -J poseidon2_extract ',
    '-o ', file.path(log_directory, 'poseidon2_%j.out '),
    '-e ', file.path(log_directory, 'poseidon2_%j.err '),
    '--wrap=',
    '"',
    'plink',
    ' --bfile ', file.path(input_package, bed_file_name),
    ' --keep ', filter_file,
    ' --make-bed ',
    ' --out ', file.path(output_directory, output_files_name),
    ' && mv ', paste0(file.path(output_directory, output_files_name), '.log'), ' ', file.path(log_directory, 'plink.log'),
    '"'
  ))
  cat("\n")
}



