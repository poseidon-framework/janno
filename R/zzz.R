# defining global variables
# ugly solution to avoid magrittr NOTE
# see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(".")

#'@importFrom magrittr "%>%"
#'@importFrom rlang .data 
#'
NULL

startupmsg <- function() {
  paste0("janno was build for Poseidon v", poseidon_version)
}

# package startup message
.onAttach <- function(lib, pkg) {
  if ( interactive() ) { packageStartupMessage(startupmsg()) }
  invisible()
}

