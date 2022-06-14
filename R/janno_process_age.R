#' @name process_age
#' @title process_age
#'
#' @description Process all valid dates in a \strong{janno} table. 
#' Look here for more information: \url{https://poseidon-framework.github.io/#/poseidonR}
#' 
#' @param x an object of class \strong{janno}
#' @param choices character vector. Which output columns should be added? 
#' By default all possible columns are added.
#' @param n integer. If "Date_BC_AD_Sample" is in \code{choices}, 
#' then how many samples should be drawn?
#' @param cal_curve String. Calibration curve to use for the (sum)calibration of
#' radiocarbon dates (see \link[Bchron]{BchronCalibrate})
#' @param ... further arguments passed to or from other methods
#' 
#' @rdname process_age
#' @export
process_age <- function(
  x, 
  choices = c(
    "Date_BC_AD_Prob",
    "Date_BC_AD_Start_Derived",
    "Date_BC_AD_Median_Derived",
    "Date_BC_AD_Stop_Derived",
    "Date_BC_AD_Sample"
  ),
  n = 100,
  cal_curve = "intcal20",
  ...
) {
  UseMethod("process_age")
}

#' @export
process_age.default <- function(
  x, 
  choices = c(
    "Date_BC_AD_Prob",
    "Date_BC_AD_Start_Derived",
    "Date_BC_AD_Median_Derived",
    "Date_BC_AD_Stop_Derived",
    "Date_BC_AD_Sample"
  ),
  n = 100,
  cal_curve = "intcal20",
  ...
) {
  stop("x is not an object of class janno")
}

#' @export
process_age.janno <- function(
  x, 
  choices = c(
    "Date_BC_AD_Prob",
    "Date_BC_AD_Start_Derived",
    "Date_BC_AD_Median_Derived",
    "Date_BC_AD_Stop_Derived",
    "Date_BC_AD_Sample"
  ),
  n = 100,
  cal_curve = "intcal20",
  ...
) {
  
  has_columns <- has_necessary_columns(
    x, 
    c("Poseidon_ID", "Date_Type", "Date_C14_Uncal_BP_Err", 
      "Date_C14_Uncal_BP_Err", "Date_BC_AD_Start", "Date_BC_AD_Stop")
  )
  if (!is.na(has_columns)) {
    stop(has_columns)
  }
  
  if ("Date_BC_AD_Prob" %in% choices) {
    x$Date_BC_AD_Prob <- age_probability_master(
      poseidon_id = x[["Poseidon_ID"]],
      date_type = x[["Date_Type"]],
      c14bp = x[["Date_C14_Uncal_BP"]], c14std = x[["Date_C14_Uncal_BP_Err"]],
      startbcad = x[["Date_BC_AD_Start"]], stopbcad = x[["Date_BC_AD_Stop"]],
      cal_curve = cal_curve
    )
  }

  if ("Date_BC_AD_Prob" %in% choices && "Date_BC_AD_Start_Derived" %in% choices) {
    x$Date_BC_AD_Start_Derived <- get_start_age(x$Date_BC_AD_Prob)
  }
    
  if ("Date_BC_AD_Prob" %in% choices && "Date_BC_AD_Median_Derived" %in% choices) {
    x$Date_BC_AD_Median_Derived <- get_center_age(x$Date_BC_AD_Prob)
  }
  
  if ("Date_BC_AD_Prob" %in% choices && "Date_BC_AD_Stop_Derived" %in% choices) {
    x$Date_BC_AD_Stop_Derived <- get_stop_age(x$Date_BC_AD_Prob)
  }
  
  if ("Date_BC_AD_Prob" %in% choices && "Date_BC_AD_Sample" %in% choices) {
    x$Date_BC_AD_Sample <- get_random_ages(x$Date_BC_AD_Prob, n = n)
  }
  
  return(x)
  
}

get_simple_ages <- function(prob, operation) {
  sapply(prob, function(y) {
      if (!is.data.frame(y)) {
        return(NA_integer_)
      } else {
        operation(y)
      }
    }
  )
}

get_start_age <- function(prob) {
  get_simple_ages(prob, function(y) { utils::head(y[["age"]][y[["two_sigma"]]], n = 1) })
}

get_stop_age <- function(prob) {
  get_simple_ages(prob, function(y) { utils::tail(y[["age"]][y[["two_sigma"]]], n = 1) })
}

get_center_age <- function(prob) {
  get_simple_ages(prob, function(y) { y[["age"]][y[["center"]]] })
}

get_random_ages <- function(prob, n) {
  lapply(prob, function(y, n) {
      if (!is.data.frame(y)) {
        return(NA_integer_)
      } else {
        sample(y[["age"]], n, y[["sum_dens"]], replace = T)
      }
    }, n = n
  )
}

age_probability_master <- function(poseidon_id, date_type, c14bp, c14std, startbcad, stopbcad, cal_curve) {
  
  res_list <- lapply(seq_along(date_type), function(i) {NA})
  
  is_c14 <- !is.na(date_type) & 
    date_type == "C14" & 
    sapply(c14bp, function(x) { !any(is.na(x)) }) &
    sapply(c14std, function(x) { !any(is.na(x)) })
  
  res_list[is_c14] <- sumcal_list_of_multiple_dates(
    poseidon_id_list = poseidon_id[is_c14],
    age_list = c14bp[is_c14], 
    err_list = c14std[is_c14],
    cal_curve = cal_curve
  )
  
  is_contextual <- !is.na(date_type) & 
    # also include samples for which calibration failed
    (date_type == "contextual" | (date_type == "C14" & is.na(res_list))) & 
    sapply(startbcad, function(x) { !any(is.na(x)) }) &
    sapply(stopbcad, function(x) { !any(is.na(x)) })
  
  res_list[is_contextual] <- contextual_date_uniform(
    startbcad = startbcad[is_contextual], 
    stopbcad = stopbcad[is_contextual]
  )
  
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

sumcal_list_of_multiple_dates <- function(poseidon_id_list, age_list, err_list, cal_curve) {
  
  pb <- progress::progress_bar$new(total = length(age_list))
  
  # run for each date collection
  Map(function(poseidon_id, cur_xs, cur_errs) {
    
    pb$tick()
    #message(poseidon_id)
    
    result_table <- tryCatch(
      sumcal(cur_xs, cur_errs, rep(cal_curve, length(cur_xs))),
      error = function(e){
        message("\nAn error occurred when calibrating C14 age for individual ", poseidon_id, " - ", e)
      }
    )
     
    return(result_table)
    
  }, poseidon_id_list, age_list, err_list)
  
}

