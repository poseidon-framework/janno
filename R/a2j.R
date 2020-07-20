#' Convert an anno file to a janno file
#'
#' @param in_anno_path character. Path to a anno file
#' @param out_janno_path chacrater. Path were the resulting janno file should be stored (default: same path as input)
#' @param to_data_frame logical. Write to file (FALSE) or return a data.fram (TRUE)
#'
#' @export
a2j <- function(
  in_anno_path, 
  out_janno_path = file.path(dirname(in_anno_path), gsub(".anno", ".janno", basename(in_anno_path))),
  to_data_frame = F
) {
  
  #### read anno file ####
  anno <- readr::read_tsv(in_anno_path, na = c("..", "n/a")) %>% janitor::clean_names()
  
  #### construct janno file sceleton ####
  janno <- tibble::tibble(
    # IDs
    Individual_ID = rep(NA, nrow(anno)),
    Collection_ID = NA,
    # sample info
    Source_Tissue = NA,
    # spatial location
    Country = NA,
    Location = NA,
    Site = NA,
    Latitude = NA,
    Longitude = NA,
    # temporal location
    Date_C14_Labnr = NA,
    Date_C14_Uncal_BP = NA,
    Date_C14_Uncal_BP_Dev = NA,
    Date_BC_Start = NA,
    Date_BC_Stop = NA,
    Date_Type = NA,
    Date_BC_Simple_Mean = NA,
    # aDNA info
    No_of_Libraries = NA,
    Data_Type = NA,
    Genotype_Ploidy = NA,
    Group_Name = NA,
    Genetic_Sex = NA,
    Nr_autosomal_SNPs = NA,
    Coverage_1240K = NA,
    MT_Haplogroup = NA,
    Y_Haplogroup = NA,
    Endogenous  = NA,
    UDG  = NA,
    Library_Built = NA,
    Damage = NA,
    Xcontam = NA, 
    Xcontam_stderr = NA,
    mtContam = NA,
    mtContam_stderr = NA,
    # meta info
    Primary_Contact = NA,
    Publication_status = NA,
    Note = NA
  )
  
  #### try to fill janno from anno ####
  # IDs
  janno$Individual_ID <- anno %c% "instance_id"
  janno$Collection_ID <- anno %c% "master_id"
  
  # sample info
  janno$Source_Tissue <- anno %c% "skeletal_element"
  
  # spatial location
  janno$Country <- anno %c% "country"
  janno$Location <- anno %c% "location"
  janno$Site <- anno %c% "site"
  janno$Latitude <- anno %round_to_5% "lat"
  janno$Longitude <- anno %round_to_5% "long"
  
  # temporal location
  age_string_variable <- "date_one_of_two_formats_format_1_95_4_percent_ci_calibrated_radiocarbon_age_conventional_radiocarbon_age_b_lab_number_e_g_5983_5747_cal_bce_6980_50_b_b_beta_226472_format_2_archaeological_context_date_b_e_g_2500_1700_bce"
  if (age_string_variable %in% colnames(anno)) { 
    age_table <- split_age_string(anno[[age_string_variable]]) # split function
    janno$Date_C14_Labnr <- age_table$Date_C14_Labnr
    janno$Date_C14_Uncal_BP <- age_table$Date_C14_Uncal_BP
    janno$Date_C14_Uncal_BP_Dev <- age_table$Date_C14_Uncal_BP_Dev
    janno$Date_BC_Start <- age_table$Date_BC_Start
    janno$Date_BC_Stop <- age_table$Date_BC_Stop
    janno$Date_Type <- age_table$Date_Type
    janno$Date_BC_Simple_Mean <- (janno$Date_BC_Start + janno$Date_BC_Stop)/2
  }
   
  # aDNA info
  janno$No_of_Libraries <- anno %c% "no_libraries"
  janno$Data_Type <- anno %data_type_from_id% "instance_id"
  janno$Genotype_Ploidy <- anno %ploidy_from_id% "instance_id"
  janno$Group_Name <- anno %c% "group_label"
  janno$Genetic_Sex <- anno %sex_simplification% "sex"
  janno$Nr_autosomal_SNPs <- anno %c% "sn_ps_hit_on_autosomes"
  janno$Coverage_1240K <- anno %c% "coverage"
  janno$MT_Haplogroup <- anno %c% "mt_dna"
  janno$Y_Haplogroup <- anno %c% "y_chrom_calls"
  janno$Endogenous <- anno %fraction_to_rounded_percent% "Endogenous_in_shotgun_sequencing_for_the_best_library"
  janno$UDG <- anno %clean_UDG% "udg_treatment_minus_untreated_half_treated_except_in_last_nucleotides_plus_treated_over_all_nucleotides"
  janno$Library_Built <- anno %library_type_from_UDG_id% "udg_treatment_minus_untreated_half_treated_except_in_last_nucleotides_plus_treated_over_all_nucleotides"
  janno$Damage <- anno %c% "damage_restrict"
  janno$Xcontam <- anno %c% "xcontam_point_estimate_if_male_and_200_sn_ps"
  janno$Xcontam_stderr <- anno %c% "xcontam_z_score_if_male_and_200_sn_ps"
  janno$mtContam <- NA # does not exist in anno files
  janno$mtContam_stderr <- NA # does not exist in anno files
  
  # meta info
  janno$Primary_Contact <- NA # does not exist in anno files
  janno$Publication_status <- anno %extract_publication_name% "publication_or_publication_plans"
  janno$Note <- anno %publication_info_to_note% "publication_or_publication_plans"
  
  #### user messages ####
  empty_cols <- sapply(janno, function(x) { all(is.na(x)) })
  names_empty_cols <- names(empty_cols)[empty_cols]
  message(c(paste(
    "\nThe following columns of the resulting janno file are empty.",
    "That may be because the input anno file simply does not contain any information about these variables,",
    "but it's also possible that the anno file does contain info in a column with",
    "another name unknown to the poseidon2 package.",
    "anno files are not well standardized.", 
    "Please check and decide if you want to apply the necessary parsing operation yourself."
    ), 
    "\n\n",
    "Missing columns:\n", paste(names_empty_cols, collapse = ", ")
  ))
  
  #### return resulting janno file/table #### 
  if (to_data_frame) {
    return(janno)
  } else {
    writeLines(janno, con = out_janno_path)
  }
}

# copy from anno without changes
`%c%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { anno[[variable]] } else { NA }
}

# copy and round to 5 decimals
`%round_to_5%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { round(anno[[variable]], 5) } else { NA }
}

# determine data type from id
`%data_type_from_id%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { ifelse(grepl(".SG$|.DG$", anno[[variable]]), "shotgun" , NA) } else { NA }
}

# ploidy
`%ploidy_from_id%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { ifelse(grepl(".DG$", anno[[variable]]) , "diploid" , "haploid") } else { NA }
}

# genetic sex
`%sex_simplification%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { ifelse(!grepl("F|M|U", anno[[variable]]), "U" , anno[[variable]]) } else { NA }
}

# percent endogenous
`%fraction_to_rounded_percent%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { round(anno[[variable]] * 100, 2) } else { NA }
}

# UDG treatment
`%clean_UDG%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { 
    new_UDG <- ifelse(length(unique(unlist(stringr::str_split(anno[[variable]], ",")))) > 1, "mixed", unique(unlist(stringr::str_split(anno[[variable]], ","))))
    # in case multiple libraries with different UDG treatment were merged
    new_UDG <- gsub("^ss.", "" , new_UDG) 
    return(new_UDG)
  } else { NA }
}

# how was the library build?
`%library_type_from_UDG_id%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { ifelse(grepl("^ss.", anno[[variable]]), "ss" , NA) } else { NA }
}

# extract publication info
`%extract_publication_name%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { 
    sapply(stringr::str_split(anno[[variable]], " "), function(x) { x[1] }) 
  } else { NA }
}

# publication info to node
`%publication_info_to_note%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { 
    sapply(
      lapply(stringr::str_split(anno[[variable]], " "), function(x) { x[-1] }),
      function(x) { paste(x, collapse = " ") }
    )
  } else { NA }
}

# age string parsing
split_age_string <- function(x) {
  
  res <- tibble::tibble(
    x = x,
    Date_C14_Labnr = rep(NA, length(x)),
    Date_C14_Uncal_BP = NA,
    Date_C14_Uncal_BP_Dev = NA,
    Date_BC_Start = NA,
    Date_BC_Stop = NA,
    Date_Type = NA
  )
  
  # determine type of date info
  none_ids <- which(is.na(x))
  present_ids <- grep("present", x)
  c14_age_ids <- grep("cal", x)
  res$Date_Type[c14_age_ids] <- "C14"
  res$Date_Type[-c14_age_ids] <- "contextual"
  res$Date_Type[present_ids] <- "modern"
  res$Date_Type[none_ids] <- "none"
  
  # parse uncalibrated c14 age info
  res$Date_C14_Labnr[c14_age_ids] <- stringr::str_extract_all(x[c14_age_ids], "[A-Z,a-z]{2,7}-[0-9]*") %>% sapply(., function(y) { paste(y, collapse = ";") } )
  uncal_dates <- stringr::str_extract_all(x[c14_age_ids], "[0-9]{1,5}\u00B1[0-9]{1,4}")
  res$Date_C14_Uncal_BP[c14_age_ids] <- sapply(uncal_dates, function(z) { 
    sapply(strsplit(z, "\u00B1"), function(a) { a[1] } ) %>% 
      paste(collapse = ";") 
  } )
  res$Date_C14_Uncal_BP_Dev[c14_age_ids] <- sapply(uncal_dates, function(z) { 
    sapply(strsplit(z, "\u00B1"), function(a) { a[2] } ) %>% 
      paste(collapse = ";") 
  } )
  
  # parse simplified start and stop age
  simple_age_split <- x %>% strsplit("-|\\ ") %>% lapply(function(y) {y[y != ""]})
  stop <- start <- rep(NA, length(simple_age_split))
  for (i in 1:length(simple_age_split)) {
    # no age info
    if (is.na(simple_age_split[[i]][1])) {
      start[i] <- NA
      stop[i] <- NA
      next
    }
    # no range: only one value e.g. 5000 BCE
    if (length(simple_age_split[[i]]) == 2) {
      if (simple_age_split[[i]][2] == "BCE") {
        start[i] <- -as.numeric(simple_age_split[[i]][1])
        stop[i] <- -as.numeric(simple_age_split[[i]][1])
        next
      }
      if (simple_age_split[[i]][2] == "CE") {
        start[i] <- as.numeric(simple_age_split[[i]][1])
        stop[i] <- as.numeric(simple_age_split[[i]][1])
        next
      } 
      if (all(grepl("^[0-9]+$", simple_age_split[[i]]))) {
        start[i] <- -as.numeric(simple_age_split[[i]][1])
        stop[i] <- -as.numeric(simple_age_split[[i]][2])
        next
      }
    }
    # normal range 5000-4700 BCE
    if (simple_age_split[[i]][3] %in% c("BCE", "calBCE")) {
      start[i] <- -as.numeric(simple_age_split[[i]][1])
      stop[i] <- -as.numeric(simple_age_split[[i]][2])
      next
    }
    if (simple_age_split[[i]][3] %in% c("CE", "calCE")) {
      start[i] <- as.numeric(simple_age_split[[i]][1])
      stop[i] <- as.numeric(simple_age_split[[i]][2])
      next
    }
    if (simple_age_split[[i]][2] %in% c("BCE", "calBCE") & simple_age_split[[i]][4] %in% c("CE", "calCE")) {
      start[i] <- -as.numeric(simple_age_split[[i]][1])
      stop[i] <- as.numeric(simple_age_split[[i]][3])
      next
    }
  }
  
  res$Date_BC_Start <- start
  res$Date_BC_Stop <- stop
  
  # replace all empty values with NA
  res[res == ""] <- NA
  
  return(res)
}
