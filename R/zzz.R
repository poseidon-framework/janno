# defining global variables
# ugly solution to avoid magrittr NOTE
# see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(".")

#'@importFrom magrittr "%>%"
#'@importFrom rlang .data 
#'
NULL

startupmsg <- function() {
  paste(
    paste0("This janno version was build for Poseidon v", poseidon_version),
    paste0("Reading and validation will strictly follow this schema version."),
    sep = "\n"
  )
}

# package startup message
.onAttach <- function(lib, pkg) {
  if ( interactive() ) { packageStartupMessage(startupmsg()) }
  invisible()
}

