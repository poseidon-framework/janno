
#### read anno file ####

anno <- data.table::fread("https://reichdata.hms.harvard.edu/pub/datasets/amh_repo/curated_releases/V42/V42.4/SHARE/public.dir/v42.4.1240K.anno",na.strings = c("..")) %>% janitor::clean_names()

#### functions ####

# copy from anno without changes
`%c%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { anno[[variable]] } else { NA }
}

# clean source tissue to a well-structured format
`%clean_source_tissue%` <- function(anno,variable) {
  if (variable %in% colnames(anno)) {mgsub::mgsub(tolower(anno[[variable]]),c("\\s|\\)","\\(","&|and|\\+|,|;","teeth"),c("","_",";","tooth"))} else { NA }
}

# copy and round to 5 decimals
`%round_to_5%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { round(anno[[variable]], 5) } else { NA }
}


# clean data type
`%clean_data_type%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) {ifelse(grepl("^shotgun",anno[[variable]],ignore.case=TRUE),"Shotgun",ifelse(grepl("1240K",anno[[variable]]),"1240K",ifelse(grepl("Reference.Genome",anno[[variable]],ignore.case=TRUE),"ReferenceGenome","OtherCapture")))} else { NA }
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
    treatment_list <- lapply(anno[[variable]] %>% strsplit(","), function(x) {unique(x)})
    new_UDG <- sapply(treatment_list, function(x) {
      if ( length(x) > 1 || (!is.na(x) && x == "Mix")) {
        "mixed"
      } else if (!is.na(x) && x == "..") {
        NA
      } else {
        x
      }
    })
    # in case multiple libraries with different UDG treatment were merged
    new_UDG <- gsub("^ss.", "" , new_UDG)
    return(new_UDG)
  } else { NA }
}


# how was the library build?
`%library_type_from_UDG_id%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { ifelse(grepl("^ss.", anno[[variable]]),"ss","ds") } else { NA } # if not ss, put ds
}

# extract publication info
`%extract_publication_name%` <- function(anno, variable) {
  if (variable %in% colnames(anno)) { 
    sapply(stringr::str_split(anno[[variable]], " "), function(x) { x[1] }) 
  } else { NA }
}

# clean publication info
`%clean_publication%` <- function(anno,variable) {
  if (variable %in% colnames(anno)) {
    gsub("\\s*\\([^\\)]+\\)", "", anno[[variable]])
  } else { NA }
}

# publication info to note 
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
    Date_C14_Uncal_BP_Err = NA,
    Date_BC_AD_Start = NA,
    Date_BC_AD_Stop = NA,
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
  res$Date_C14_Uncal_BP_Err[c14_age_ids] <- sapply(uncal_dates, function(z) { 
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
  
  res$Date_BC_AD_Start <- start
  res$Date_BC_AD_Stop <- stop
  
  # replace all empty values with NA
  res[res == ""] <- NA
  
  return(res)
}

`%clean_Xcontam%` <- function(anno,variable) {
  if (variable %in% colnames(anno)) {gsub("n/a \\(<200 SNPs\\)|^-",NA,anno[[variable]])} else { NA }
}

# Explanation of what's in the .anno file:
# Yes, we compute ANGSD confidence intervals as follows. I believe ANGSD outputs a mean=M and a standard error=S.
# The 95% confidence interval is then computed as: [max(0,M-1.65*S),min(1,M+1.65S)]
# It’s possible that ANGSD instead outputs a mean=M and a number of standard errors from zero=Z. 
# The 95% confidence interval is then computed as: [max(0,M-1.65*(abs(M/Z)),min(1,M+1.65*(abs(M/Z))]
derive_standard_error <- function(anno, mean_var, err_var) {
  mean_val <- anno[[mean_var]]
  mean_val[mean_val == "n/a (<200 SNPs)"] <- NA
  mean_val <- as.numeric(mean_val)
  err_val <- anno[[err_var]]
  err_val[err_val == "n/a (<200 SNPs)"] <- NA
  range_list <- strsplit(gsub("\\[|\\]", "", err_val), ",")
  unlist(Map(
    function(mean_one, range_list_one) {
      lower_range <- as.numeric(range_list_one[1])
      upper_range <- as.numeric(range_list_one[2])
      if (is.na(mean_one) || is.na(lower_range) || is.na(upper_range)) {
        NA
      } else if ( upper_range < 1 ) {
        abs(upper_range - mean_one) / 1.65
      } else if ( upper_range >= 1 & lower_range > 0 ) {
        abs(mean_one - lower_range) / 1.65
      } else {
        NA
      }
    }, 
    mean_val, range_list
  ))
}

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
  Date_C14_Uncal_BP_Err = NA,
  Date_BC_AD_Median = NA,
  Date_BC_AD_Start = NA,
  Date_BC_AD_Stop = NA,
  Date_Type = NA,
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
  Publication_Status = NA,
  Note = NA,
  Keywords= NA
)



#### try to fill janno from anno ####
# IDs
janno$Individual_ID <- anno %c% "instance_id_all_means_includes_a_mix_of_udg_treated_and_non_udg_treated_published_distinguishes_a_published_sample_for_a_still_unpublished_higher_quality_version"
janno$Collection_ID <- anno %c% "master_id"

# sample info
janno$Source_Tissue <- anno %clean_source_tissue% "skeletal_element" 

# spatial location
janno$Country <- anno %c% "country"
janno$Location <- anno %c% "locality"
janno$Site <- anno %c% "site" # there is no site name? 
janno$Latitude <- anno %round_to_5% "lat"
janno$Longitude <- anno %round_to_5% "long"


# temporal location
age_string_variable <- "date_one_of_two_formats_format_1_95_4_percent_ci_calibrated_radiocarbon_age_conventional_radiocarbon_age_bp_lab_number_e_g_5983_5747_cal_bce_6980_50_bp_beta_226472_format_2_archaeological_context_date_e_g_2500_1700_bce"
if (age_string_variable %in% colnames(anno)) { 
  age_table <- split_age_string(anno[[age_string_variable]]) # split function
  janno$Date_C14_Labnr <- age_table$Date_C14_Labnr
  janno$Date_C14_Uncal_BP <- age_table$Date_C14_Uncal_BP
  janno$Date_C14_Uncal_BP_Err <- age_table$Date_C14_Uncal_BP_Err
  janno$Date_BC_AD_Start <- age_table$Date_BC_AD_Start
  janno$Date_BC_AD_Stop <- age_table$Date_BC_AD_Stop
  janno$Date_Type <- age_table$Date_Type
  janno$Date_BC_AD_Median <- round((janno$Date_BC_AD_Start + janno$Date_BC_AD_Stop)/2) # round calculated median age
}


# aDNA info
janno$No_of_Libraries <- anno %c% "no_libraries"
janno$Data_Type <- anno %clean_data_type% "data_type"
janno$Genotype_Ploidy <- anno %ploidy_from_id% "instance_id_all_means_includes_a_mix_of_udg_treated_and_non_udg_treated_published_distinguishes_a_published_sample_for_a_still_unpublished_higher_quality_version"
janno$Group_Name <- anno %c% "group_id_format_convention_which_we_try_to_adhere_to_is_country_geographic_region_geographic_subregion_archaeological_period_or_date_bp_alternative_archaeological_period_archaeological_culture_alternative_archaeological_culture_genetic_subgrouping_index_if_necessary_o_sometimes_with_additional_detail_if_an_outlier_additional_suffix_especially_relative_status_if_we_recommend_removing_from_main_analysis_grouping_contam_if_contaminated_lc_if_15000_sn_ps_on_autosomal_targets_sg_or_dg_if_shotgun_data_hg_hunter_gatherer_n_neolithic_c_chalcolithic_copper_age_ba_bronze_age_ia_iron_age_e_early_m_middle_l_late_a_antiquity"
janno$Genetic_Sex <- anno %sex_simplification% "sex"
janno$Nr_autosomal_SNPs <- anno %c% "sn_ps_hit_on_autosomal_targets"
janno$Coverage_1240K <- anno %c% "coverage_on_autosomal_targets"
janno$MT_Haplogroup <- anno %c% "mt_dna_haplogroup_if_a_2_coverage_or_published_merged_data_or_consensus_if_not_available"
janno$Y_Haplogroup <- anno %c% "y_chrom_automatically_called_only_if_50000_autosomal_sn_ps_hit"
janno$Endogenous <- NA # does not exist in anno files
janno$UDG <- anno %clean_UDG% "library_type_minus_no_damage_correction_half_damage_retained_at_last_position_plus_damage_fully_corrected_ss_single_stranded_library_preparation"
janno$Library_Built <- anno %library_type_from_UDG_id% "library_type_minus_no_damage_correction_half_damage_retained_at_last_position_plus_damage_fully_corrected_ss_single_stranded_library_preparation"
janno$Damage <- anno %c% "damage_rate_in_first_nucleotide_on_sequences_overlapping_1240k_targets_merged_data"
janno$Xcontam <- anno %clean_Xcontam% "xcontam_angsd_mom_point_estimate_only_if_male_and_200"
janno$Xcontam_stderr <- derive_standard_error(
  anno, 
  "xcontam_angsd_mom_point_estimate_only_if_male_and_200", 
  "xcontam_angsd_mom_95_percent_ci_truncated_at_0_only_if_male_and_200"
) 
janno$mtContam <- NA # does not exist in anno files
janno$mtContam_stderr <- NA # does not exist in anno files

# meta info
janno$Primary_Contact <- anno %c% "representative_contact" 
janno$Publication_Status <- paste0("@" , anno %clean_publication% "publication")
janno$Note <- anno %c% "assessment_xcontam_listed_if_z_2_standard_errors_from_zero_0_02_0_05_questionable_0_05_questionable_critical_or_fail_mtcontam_97_5th_percentile_estimates_listed_if_coverage_2_0_8_is_questionable_critical_0_8_0_95_is_questionable_and_0_95_0_98_is_recorded_but_pass_gets_overriden_by_angsd"
janno$Note <- trimws(paste(janno$Note,anno %publication_info_to_note% "publication"))
janno$Keywords <- NA # does not exist in anno files

