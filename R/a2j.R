#' Title
#'
#' @param in_anno_path test
#' @param out_janno_path test
#'
#' @export
a2j <- function(
  in_anno_path, 
  out_janno_path = file.path(dirname(in_anno_path), gsub(".anno", ".janno", basename(in_anno_path)))
) {
  
  anno <- readr::read_tsv("testdata/1240K.anno.mod2", na = c("..", "n/a")) %>% janitor::clean_names()
  
  janno <- tibble::tibble(
    Individual_ID = rep(NA, nrow(anno)),
    Collection_ID = NA,
    Skeletal_Element = NA,
    Country = NA,
    Location = NA,
    Site = NA,
    Latitude = NA,
    Longitude = NA,
    Date_C14_Labnr = NA,
    Date_C14_Uncal_BP = NA,
    Date_C14_Uncal_Dev = NA,
    Date_BC_Median = NA,
    Date_BC_Start = NA,
    Date_BC_Stop = NA,
    Date_Type = NA,
    No_of_Libraries = NA,
    Data_Type = NA,
    Genotype_Ploidy = NA,
    Group_Name = NA,
    Genetic_Sex = NA,
    Nr_autosomal_SNPs = NA,
    Coverage_1240K = NA,
    MT_Haplogroup = NA,
    Y_Haplogroup = NA,
    Percent_endogenous  = NA,
    UDG  = NA,
    Library_Built = NA,
    Damage = NA,
    Xcontam = NA, 
    Xcontam_stderr = NA,
    mtContam = NA,
    mtContam_stderr = NA,
    Primary_Contact = NA,
    Publication_status = NA,
    Note = NA
  )
  
  janno$Individual_ID = anno %c% "instance_id"
  janno$Collection_ID = anno %c% "master_id"
  janno$Skeletal_Element = anno %c% "skeletal_element"
  janno$Country = anno %c% "country"
  janno$Location = anno %c% "location"
  janno$Site = anno %c% "site"
  janno$Latitude = anno %cr% "lat"
  janno$Longitude = anno %cr% "long"
  
  age_table <- split_age_string(anno[["date_one_of_two_formats_format_1_95_4_percent_ci_calibrated_radiocarbon_age_conventional_radiocarbon_age_b_lab_number_e_g_5983_5747_cal_bce_6980_50_b_b_beta_226472_format_2_archaeological_context_date_b_e_g_2500_1700_bce"]])
  
  janno$Date_C14_Labnr = age_table$age_c14_labnr
  janno$Date_C14_Uncal_BP = age_table$age_c14_uncal_BP
  janno$Date_C14_Uncal_Dev = age_table$age_c14_uncal_BP_dev
  janno$Date_BC_Median = NA
  janno$Date_BC_Start = NA
  janno$Date_BC_Stop = NA
  janno$Date_Type = NA
   
  
  janno <- anno
  writeLines(janno, con = out_janno_path)
}

# copy from anno without changes
`%c%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { anno[[variable]] } else { NA }
}

# copy and round to 5 decimals
`%cr%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { round(anno[[variable]], 5) } else { NA }
}

# age string parsing
split_age_string <- function(x) {
  
  res <- tibble::tibble(
    age_arch_start_BC = rep(NA, length(x)),
    age_arch_stop_BC = NA,
    age_c14_uncal_BP = NA,
    age_c14_uncal_BP_dev = NA,
    age_c14_labnr = NA
  )
  
  # c14 age
  c14_age_ids <- grep("\\(", x)
  res$age_c14_labnr[c14_age_ids] <- stringr::str_extract_all(x[c14_age_ids], "[A-Z,a-z]{2,7}-[0-9]*") %>% sapply(., function(y) { paste(y, collapse = ";") } )
  uncal_dates <- stringr::str_extract_all(x[c14_age_ids], "[0-9]{1,5}±[0-9]{1,4}")
  res$age_c14_uncal_BP[c14_age_ids] <- sapply(uncal_dates, function(z) { 
    sapply(strsplit(z, "±"), function(a) { a[1] } ) %>% 
      paste(collapse = ";") 
  } )
  res$age_c14_uncal_BP_dev[c14_age_ids] <- sapply(uncal_dates, function(z) { 
    sapply(strsplit(z, "±"), function(a) { a[2] } ) %>% 
      paste(collapse = ";") 
  } )
  
  # arch age
  arch_age_split <- x[-c14_age_ids] %>% strsplit("-|\\ ") %>% lapply(function(y) {y[y != ""]})
  stop <- start <- rep(NA, length(arch_age_split))
  for (i in 1:length(arch_age_split)) {
    if (is.na(arch_age_split[[i]][1])) {
      start[i] <- NA
      stop[i] <- NA
      next
    }
    if (arch_age_split[[i]][1] == "present") {
      start[i] <- 2000
      stop[i] <- 2000
      next
    }
    if (length(arch_age_split[[i]]) == 2) {
      if (arch_age_split[[i]][2] == "BCE") {
        start[i] <- -as.numeric(arch_age_split[[i]][1])
        stop[i] <- -as.numeric(arch_age_split[[i]][1])
        next
      }
      if (arch_age_split[[i]][2] == "CE") {
        start[i] <- as.numeric(arch_age_split[[i]][1])
        stop[i] <- as.numeric(arch_age_split[[i]][1])
        next
      } 
      if (all(grepl("^[0-9]+$", arch_age_split[[i]]))) {
        start[i] <- -as.numeric(arch_age_split[[i]][1])
        stop[i] <- -as.numeric(arch_age_split[[i]][2])
        next
      }
    }
    if (arch_age_split[[i]][3] == "BCE") {
      start[i] <- -as.numeric(arch_age_split[[i]][1])
      stop[i] <- -as.numeric(arch_age_split[[i]][2])
      next
    }
    if (arch_age_split[[i]][3] == "CE") {
      start[i] <- as.numeric(arch_age_split[[i]][1])
      stop[i] <- as.numeric(arch_age_split[[i]][2])
      next
    }
    if (arch_age_split[[i]][2] == "BCE" & arch_age_split[[i]][4] == "CE") {
      start[i] <- -as.numeric(arch_age_split[[i]][1])
      stop[i] <- as.numeric(arch_age_split[[i]][3])
      next
    }
  }
  res$age_arch_start_BC[-c14_age_ids] <- start
  res$age_arch_stop_BC[-c14_age_ids] <- stop
  
  return(res)
}
