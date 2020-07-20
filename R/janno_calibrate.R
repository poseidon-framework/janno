#### calibrate ####

#' @name calibrate
#' @title Calibrate all valid dates in a \strong{janno} table
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
calibrate.janno <- function(x,...) {
  
  no_dates <- sapply(x$Date_C14_Uncal_BP, function(z) { all(is.na(z)) } )
  no_errors <- sapply(x$Date_C14_Uncal_BP_Err, function(z) { all(is.na(z)) } )
  
  x_filtered <- x[!no_dates & !no_errors,]
  
  x_filtered <- sumcal(x_filtered$Date_C14_Uncal_BP, x_filtered$Date_C14_Uncal_BP_Err)
  
}

sumcal <- function(x, err) {
  
  bol <- 1950 # c14 reference zero
  threshold <- (1 - 0.9545) / 2 # 2sigma range probability threshold
  
  Map(function(cur_xs, cur_errs) {
    
    cur_raw_calibration_output <- Bchron::BchronCalibrate(
      ages      = cur_xs,
      ageSds    = cur_errs,
      calCurves = rep("intcal13", length(cur_xs))
    )
    
    density_tables <- lapply(
      cur_raw_calibration_output,
      function(y) { 
        tibble::tibble(
          age = -y$ageGrid + bol,
          densities = y$densities
        )
      }
    )
    
    sum_density_table <- dplyr::bind_rows(density_tables) %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(
        sum_dens = sum(densities)/length(density_tables),
        .groups = "drop"
      ) %>%
      dplyr::right_join(
        .,
        data.frame(age = min(.$age):max(.$age)),
        by = "age"
      ) %>%
      dplyr::mutate(
        sum_dens = tidyr::replace_na(sum_dens, 0)
      )
    
    a <- cumsum(sum_density_table$sum_dens) # cumulated density
    bottom <- sum_density_table$age[max(which(a <= threshold))]
    top <- sum_density_table$age[min(which(a > 1 - threshold))]
    center <- sum_density_table$age[which.min(abs(a - 0.5))]
    
    result_table <- sum_density_table %>% dplyr::mutate(
      two_sigma = age >= bottom & age <= top,
      center = age == center
    )
    
    return(result_table)
    
  }, x, err)
  
}

