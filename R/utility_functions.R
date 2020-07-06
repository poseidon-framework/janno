#' quick_install
#'
#' @param path test
#'
#' @export
quick_install <- function(path = "~")  {
  
  path_to_script <- system.file("poseidon2.R", package = "anno2janno")
  
  file.copy(
    from = path_to_script,
    to = file.path(path, "poseidon2.R"),
    overwrite = T
  )
  
}
