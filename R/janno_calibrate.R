#### calibrate ####

#' @name calibrate
#' @title Calibrate all valid dates in a \strong{janno} table
#'
#' @param x an object of class janno
#' @param ... further arguments passed to or from other methods
#' 
#' @export
#'
#' @rdname calibrate
#'
calibrate <- function(x, ...) {
  UseMethod("calibrate")
}

#' @rdname calibrate
#' @export
calibrate.default <- function(x, ...) {
  stop("x is not an object of class janno")
}

#' @rdname calibrate
#' @export
calibrate.janno <- function(x, ...) {
  
  # no_dates <- sapply(x$Date_C14_Uncal_BP, function(z) { all(is.na(z)) } )
  # no_errors <- sapply(x$Date_C14_Uncal_BP_Err, function(z) { all(is.na(z)) } )
  
  x %>% dplyr::mutate(
    Date_C14_Cal_BC_AD_prob = sumcal_list_of_multiple_dates(
      .data[["Date_C14_Uncal_BP"]], .data[["Date_C14_Uncal_BP_Err"]]
    )
  )
  
}

arch_age <- function(start, stop)

sumcal_list_of_multiple_dates <- function(age_list, err_list) {
  
  bol <- 1950 # c14 reference zero
  threshold <- (1 - 0.9545) / 2 # 2sigma range probability threshold
  
  # run for each date collection
  Map(function(cur_xs, cur_errs) {
    
    if (all(is.na(cur_xs)) | all(is.na(cur_errs))) {
      # return(
      #   tibble::tibble(
      #     age = rlang::na_int,
      #     sum_dens = rlang::na_dbl,
      #     two_sigma = rlang::na_lgl,
      #     center = rlang::na_lgl
      #   )
      # )
      return(NA)
    }
    
    cur_raw_calibration_output <- Bchron::BchronCalibrate(
      ages      = cur_xs,
      ageSds    = cur_errs,
      calCurves = rep("intcal13", length(cur_xs))
    )
    
    density_tables <- lapply(
      cur_raw_calibration_output,
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
    
  }, age_list, err_list)
  
}

