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
    Average_Date = NA,
    Date_Earlier = NA,
    Date_Later = NA,
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
  
  anno2janno::colname_reference$janno[]
   
  
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
