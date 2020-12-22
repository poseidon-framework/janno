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
