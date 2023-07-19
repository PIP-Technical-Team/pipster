pipster_default_options <- list(
  pipster.verbose      = TRUE,
  pipster.gd_threshold = 200
)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipster_default_options) %in% names(op))
  if (any(toset)) options(pipster_default_options[toset])

  invisible()
}
