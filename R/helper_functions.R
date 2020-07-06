#' Title
#'
#' @param n 
#'
#' @export
random_alphanumeric_string <- function(n = 1, l = 7) {
  sapply(1:n, function(i) { 
    paste(
      sample(c(0:9, LETTERS, letters), l, TRUE), 
      collapse = ""
    ) 
  })
}
