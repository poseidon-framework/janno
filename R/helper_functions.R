get_genetic_data_type <- function(input_package) {
  files_in_package <- list.files(input_package)
  file_types_in_package <- unique(get_extension(files_in_package))
  res <- c()
  # plink_binary
  if (all(c("bed", "bim", "fam") %in% file_types_in_package)) { 
    res <- append(res, "plink_binary") 
  }
  # eigenstrat
  if (all(c("geno", "snp", "ind") %in% file_types_in_package)) { 
    res <- append(res, "eigenstrat") 
  }
  return(res)
}

get_extension <- function(files){ 
  sapply(strsplit(basename(files), split="\\."), function(x) {x[-1]})
} 

get_janno_file_paths <- function(path) {
  lapply(
    path, function(x) {
      if (strsplit(x, "\\.") %>% unlist %>% utils::tail(1) == "janno") {
        checkmate::assert_file_exists(x)
        return(x)
      } else {
        checkmate::assert_directory_exists(x)
        return(list.files(x, pattern = "\\.janno", full.names = T, recursive = T))
      }
    }
  ) %>% unlist()
}

get_last_two_elements_of_path <- function(x) {
  if (is.na(x)) {
    x
  } else {
    file.path(
      basename(dirname(x)),
      basename(x)
    )
  }
}
