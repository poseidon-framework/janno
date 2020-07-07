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
  cat(input_janno, "\n")
  # does it exist?
  if (file.exists(input_janno)) {
    cat("=> The janno file exists\n")
  } else {
    stop("The janno file does not exist")
  }
  # does it contain tab separated columns?
  input_janno_linewise <- readr::read_lines(input_janno, n_max = 50)
  if (all(grepl(".*\\t.*\\t.*\\t.*", input_janno_linewise))) {
    cat("=> The janno file seems to be a valid tab separated file\n")
  } else {
    stop("The janno file can't be a valid .tsv file with at least 4 columns")
  }
  # read file
  character_janno <- readr::read_tsv(input_janno, col_types = readr::cols(.default = "c"))
  # are the necessary columns present
  if (all(janno_column_names %in% colnames(character_janno))) {
    cat("=> The janno file has all necessary columns\n")
  } else {
    stop(
      "The janno file lacks the following columns: ", 
      paste(janno_column_names[!(janno_column_names %in% colnames(character_janno))], collapse = ", ")
    )
  }
  
  
}

validate_package <- function(input_package) {
  cat(input_package, "\n")
  # does it exist?
  if (dir.exists(input_package)) {
    cat("=> The directory exists\n")
  } else {
    stop("The directory does not exist")
  }
  # does it contain the necessary files once?
  necessary_files <- list.files(input_package, pattern = ".janno|.bed|.bim|.fam")
  extensions_necessary_files <- tools::file_ext(necessary_files)
  if (all(extensions_necessary_files == c("bed", "bim", "fam", "janno"))) {
    cat("=> The package contains the necessary files .bed, .bim, .fam and .janno exactly once\n")
  } else {
    stop("Necessary files (.bed, .bim, .fam and .janno) are missing or multiple of these are present")
  }
  # are other files present?
  all_files <- list.files(input_package)
  if (any(!all_files %in% necessary_files)) {
    cat("=> There are other files present as well:", paste(all_files[!all_files %in% necessary_files], collapse = ", "), "\n")
  }
  # check .janno file
  validate_janno(list.files(input_package, pattern = ".janno", full.names = T))
  cat("\n")
}








