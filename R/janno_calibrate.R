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
  
  x$Date_BC_AD_Prob <- age_probability_master(
    x[["Date_Type"]],
    x[["Date_C14_Uncal_BP"]], x[["Date_C14_Uncal_BP_Err"]],
    x[["Date_BC_AD_Start"]], x[["Date_BC_AD_Stop"]]
  )
  
  x$Date_BC_AD_Median_Prob <- get_center_age(x$Date_BC_AD_Prob)
  
  x$Date_BC_AD_Sample <- get_random_ages(x$Date_BC_AD_Prob, n = 100)
  
  return(x)
  
}

get_random_ages <- function(prob, n) {
  lapply(
    prob, function(y, n) {
      if (!is.data.frame(y) && is.na(y)) {
        return(NA_integer_)
      } else {
        sample(y[["age"]], n, y[["sum_dens"]], replace = T)
      }
    }, n = n
  )
}

get_center_age <- function(prob) {
  sapply(
    prob, function(y) {
      if (!is.data.frame(y) && is.na(y)) {
        return(NA_integer_)
      } else {
        y[["age"]][y[["center"]]]
      }
    }
  )
}

age_probability_master <- function(date_type, c14bp, c14std, startbcad, stopbcad) {
  
  res_list <- lapply(seq_along(date_type), function(i) {NA})
  
  is_c14 <- !is.na(date_type) & date_type == "C14"
  is_contextual <- !is.na(date_type) & date_type == "contextual"
  
  res_list[is_c14] <- sumcal_list_of_multiple_dates(c14bp[is_c14], c14std[is_c14])
  res_list[is_contextual] <- contextual_date_uniform(startbcad[is_contextual], stopbcad[is_contextual])
  
  return(res_list)

}

contextual_date_uniform <- function(startbcad, stopbcad) {
  
  lapply(seq_along(startbcad), function(i) {
    tibble::tibble(
      age = startbcad[i]:stopbcad[i],
      sum_dens = 1/(length(startbcad[i]:stopbcad[i])),
      two_sigma = TRUE,
      center = .data[["age"]] == round(mean(c(startbcad[i]:stopbcad[i])))
    )
  })
  
}

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

