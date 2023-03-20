#' @name quickcalibrate
#' @title quickcalibrate
#' 
#' @description Get values for the .janno file columns Date_BC_AD_Median,
#' Date_BC_AD_Start and Date_BC_AD_Stop from radiocarbon ages with mean
#' and sd. Uses \link[Bchron]{Bchron} for the calibration.
#' 
#' @param ages list. List of one or multiple radiocarbon date ages BP 
#' (e.g. list(3000) or list(2000, c(2000, 2300, 2100)))
#' @param sds list. One or multiple standard deviations 
#' (±1 sigma) (e.g. list(30) or list(20, c(20, 30, 70)))
#' @param cal_curves character vector. Calibration curves to be used for the calibration.
#' See the argument \code{calCurves} for \link[Bchron]{BchronCalibrate}
#' @param ... additional arguments are passed to \link[Bchron]{BchronCalibrate}
#' (will not work for the \code{dfs} argument and multiple dates)
#'  
#' @examples
#' quickcalibrate(
#'   ages = list(c(3000,3100), 4000), 
#'   sds = list(c(100,30), 50),
#'   cal_curves = c("shcal20", "intcal20")
#' )
#' 
#' @export
quickcalibrate <- function(ages, sds, cal_curves = rep("intcal20", length(ages)), ...) {
  # check equal length of input
  checkmate::assert_true(length(ages) == length(sds))
  # check input
  sapply(unlist(ages), function(x) { checkmate::assert_count(x, na.ok = T) })
  sapply(unlist(sds), function(x) { checkmate::assert_count(x, na.ok = T) })
  Map(function(x,y) { checkmate::assert_true(length(x) == length(y)) }, ages, sds)
  checkmate::assert_true(length(ages) == length(cal_curves))
  # run sumcalibration
  sumcul_res_list <- Map(function(x,y,z) { sumcal(x, y, z, ...) }, ages, sds, cal_curves)
  # prepare output table
  result_table <- lapply(sumcul_res_list, function(x) {
    data.frame(
      Date_BC_AD_Start_2Sigma = utils::head(x[["age"]][x[["two_sigma"]]], 1),
      Date_BC_AD_Start_1Sigma = utils::head(x[["age"]][x[["one_sigma"]]], 1),
      Date_BC_AD_Median = x[["age"]][x[["center"]]],
      Date_BC_AD_Stop_1Sigma = utils::tail(x[["age"]][x[["one_sigma"]]], 1),
      Date_BC_AD_Stop_2Sigma = utils::tail(x[["age"]][x[["two_sigma"]]], 1)
    )
  }) %>% dplyr::bind_rows()
  # return output
  return(result_table)
}

sumcal <- function(xs, errs, cal_curve, ...) {
  
  if (any(is.na(xs)) | any(is.na(errs))) {
    return(
      data.frame(
        age = NA,
        sum_dens = NA,
        center = NA,
        one_sigma = NA,
        two_sigma = NA
      )
    )
  }
  
  bol <- 1950 # c14 reference zero

  raw_calibration_output <- Bchron::BchronCalibrate(
    ages      = xs,
    ageSds    = errs,
    calCurves = rep(cal_curve, length(xs)),
    ...
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
  
  sum_density_table_by_year <- dplyr::bind_rows(density_tables) %>%
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
    ) %>%
    dplyr::arrange(.data[["age"]])
  
  cum_dens_by_year <- cumsum(sum_density_table_by_year$sum_dens)
  center_year <- sum_density_table_by_year$age[which.min(abs(cum_dens_by_year - 0.5))]
  
  sum_density_table_by_dens <- sum_density_table_by_year %>%
    dplyr::arrange(dplyr::desc(.[["sum_dens"]]))
  cum_dens_by_dens <- cumsum(sum_density_table_by_dens$sum_dens)
  one_sigma_years <- sum_density_table_by_dens$age[cum_dens_by_dens <= 0.683]
  two_sigma_years <- sum_density_table_by_dens$age[cum_dens_by_dens <= 0.954]
  
  result_table <- sum_density_table_by_year %>%
    dplyr::left_join(
      data.frame(age = center_year, center = TRUE),
      by = "age"
    ) %>%
    dplyr::left_join(
      data.frame(age = one_sigma_years, one_sigma = TRUE),
      by = "age"
    ) %>%
    dplyr::left_join(
      data.frame(age = two_sigma_years, two_sigma = TRUE),
      by = "age"
    ) %>%
    base::replace(is.na(.), FALSE)
  
  return(result_table)
}
