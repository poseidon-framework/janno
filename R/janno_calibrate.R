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
    
    densities <- lapply(
      cur_raw_calibration_output,
      function(y) { 
        tibble::tibble(
          age = -y$ageGrid + bol,
          dens_dist = y$densities
        )
      }
    )
    
    dplyr::bind_rows(densities) %>%
      dplyr::group_by(age) %>%
      dplyr::summarise(
        dens = sum(dens_dist)
      ) %>%
      dplyr::ungroup()
    
    # transform BchronCalibrate result to a informative tibble
    # this tibble includes the years, the density per year,
    # the normalized density per year and the information,
    # if this year is in the two_sigma range for the current date
    lapply(
      cur_raw_calibration_output,
      function(y) {
        a <- cumsum(y$densities) # cumulated density
        bottom <- y$ageGrid[max(which(a <= threshold))]
        top <- y$ageGrid[min(which(a > 1 - threshold))]
        center <- y$ageGrid[which.min(abs(a - 0.5))]
        tibble::tibble(
          age = -y$ageGrid + bol,
          dens_dist = y$densities,
          #norm_dens = y$densities/max(y$densities),
          #two_sigma = y$ageGrid >= bottom & y$ageGrid <= top,
          #center = y$ageGrid == center
        )
      }
    )
    
  }, x, err)
  
  Bchron::BchronCalibrate(
    ages      = x,
    ageSds    = err,
    calCurves = rep("intcal13", nrow(.))
  ) %>%
    # transform BchronCalibrate result to a informative tibble
    # this tibble includes the years, the density per year,
    # the normalized density per year and the information,
    # if this year is in the two_sigma range for the current date
    pbapply::pblapply(
      function(y) {
        a <- y$densities %>% cumsum # cumulated density
        bottom <- y$ageGrid[which(a <= threshold) %>% max]
        top <- y$ageGrid[which(a > 1 - threshold) %>% min]
        center <- y$ageGrid[which.min(abs(a - 0.5))]
        tibble::tibble(
          age = -y$ageGrid + bol,
          dens_dist = y$densities,
          norm_dens = y$densities/max(y$densities),
          two_sigma = y$ageGrid >= bottom & y$ageGrid <= top,
          center = y$ageGrid == center
        )
      }
    )
  
}

