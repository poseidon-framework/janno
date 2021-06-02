#' @name quickcalibrate
#' @title quickcalibrate
#' 
#' @description Get values for the .janno file columns Date_BC_AD_Median,
#' Date_BC_AD_Start and Date_BC_AD_Stop from radiocarbon ages with mean
#' and sd.
#' See the package README for more information.
#' 
#' @param ages list. List of one or multiple radiocarbon date ages BP 
#' (e.g. list(3000) or list(2000, c(2000, 2300, 2100)))
#' @param sds list. One or multiple standard deviations 
#' (1 sigma ±) (e.g. list(30) or list(20, c(20, 30, 70)))
#' 
#' @export
quickcalibrate <- function(ages, sds) {
  # check equal length of input
  checkmate::assert_true(length(ages) == length(sds))
  # check input
  sapply(unlist(ages), function(x) { checkmate::assert_count(x, na.ok = T) })
  sapply(unlist(sds), function(x) { checkmate::assert_count(x, na.ok = T) })
  Map(function(x,y) { checkmate::assert_true(length(x) == length(y)) }, ages, sds)
  # run sumcalibration
  sumcul_res_list <- Map(function(x,y) { sumcal(x, y) }, ages, sds)
  # prepare output table
  result_table <- lapply(sumcul_res_list, function(x) {
    data.frame(
      Date_BC_AD_Median = x[["age"]][x[["center"]]],
      Date_BC_AD_Start = utils::head(x[["age"]][x[["two_sigma"]]], 1),
      Date_BC_AD_Stop = utils::tail(x[["age"]][x[["two_sigma"]]], 1)
    )
  }) %>% dplyr::bind_rows()
  # return output
  return(result_table)
}

sumcal <- function(xs, errs) {
  
  if (any(is.na(xs)) | any(is.na(errs))) {
    return(
      data.frame(
        age = NA,
        sum_dens = NA,
        center = NA,
        two_sigma = NA
      )
    )
  }
  
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
