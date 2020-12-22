#' @name quickcalibrate
#' @title quickcalibrate
#' 
#' @description Get values for the .janno file columns Date_BC_AD_Median,
#' Date_BC_AD_Start and Date_BC_AD_Stop from radiocarbon ages with mean
#' and sd.
#' 
#' @param uncalibrated_ages_BP character. One or multiple radiocarbon date ages BP 
#' (e.g. "3000" or "3000,3100,3050" or c("2000", "2000,2300,2100"))
#' @param standard_deviations character. One or multiple standard deviations 
#' (1 sigma ±) (e.g. "30" or "30,40,50" or c("20", "20,30,70"))
#' 
#' @export
quickcalibrate <- function(uncalibrated_ages_BP, standard_deviations) {
  # check equal length of input
  checkmate::assert_true(length(uncalibrated_ages_BP) == length(standard_deviations))
  # transform to numeric vector if it is not already a numeric scalar
  ages_list <- lapply(uncalibrated_ages_BP, function(x){
    if (grepl(",", x)) { y <- as.integer(unlist(strsplit(x, ","))) } else { y <- as.integer(x) }
  })
  sds_list <- lapply(standard_deviations, function(x){
    if (grepl(",", x)) { y <- as.integer(unlist(strsplit(x, ","))) } else { y <- as.integer(x) }
  })
  # check input
  sapply(unlist(ages_list), function(x) { checkmate::assert_count(x) })
  sapply(unlist(sds_list), function(x) { checkmate::assert_count(x) })
  Map(function(x,y) { checkmate::assert_true(length(x) == length(y)) }, ages_list, sds_list)
  # run sumcalibration
  sumcul_res_list <- Map(function(x,y) { sumcal(x, y) }, ages_list, sds_list)
  # start message
  quickcalibrate_start_message()
  # prepare output table
  result_table <- lapply(sumcul_res_list, function(x) {
    data.frame(
      Date_BC_AD_Median = x[["age"]][x[["center"]]],
      Date_BC_AD_Start = utils::head(x[["age"]][x[["two_sigma"]]], 1),
      Date_BC_AD_Stop = utils::tail(x[["age"]][x[["two_sigma"]]], 1)
    )
  }) %>% dplyr::bind_rows()
  # print output
  utils::write.table(result_table, row.names = F, quote = F, sep = '\t')
}

quickcalibrate_start_message <- function() {
  cli::cli_h1(paste(
    "quickcalibrate => Simplifies filling the .janno file dating columns"
  ))
  cat("\n")
}

sumcal <- function(xs, errs) {
  
  bol <- 1950
  threshold <- 0.025
  
  raw_calibration_output <- Bchron::BchronCalibrate(
    ages      = xs,
    ageSds    = errs,
    calCurves = rep("intcal20", length(xs))
  )
  
  density_tables <- lapply(
    raw_calibration_output,
    function(y) { 
      tibble::tibble(
        age = as.integer(-y$ageGrid + bol),
        densities = y$densities
      )
    }
  )
  
  sum_density_table <- dplyr::bind_rows(density_tables) %>%
    dplyr::group_by(.data[["age"]]) %>%
    dplyr::summarise(
      sum_dens = sum(.data[["densities"]])/length(density_tables),
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      .,
      data.frame(age = min(.[["age"]]):max(.[["age"]])),
      by = "age"
    ) %>%
    dplyr::mutate(
      sum_dens = tidyr::replace_na(.data[["sum_dens"]], 0)
    )
  
  a <- cumsum(sum_density_table$sum_dens) # cumulated density
  bottom <- sum_density_table$age[max(which(a <= threshold))]
  top <- sum_density_table$age[min(which(a > 1 - threshold))]
  center <- sum_density_table$age[which.min(abs(a - 0.5))]
  
  result_table <- sum_density_table %>% dplyr::mutate(
    two_sigma = .data[["age"]] >= bottom & .data[["age"]] <= top,
    center = .data[["age"]] == center
  )
  
  return(result_table)
}
