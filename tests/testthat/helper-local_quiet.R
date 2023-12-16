
#' Silence BirdFlowR messages for the local scope
#'
#' This is intended for use with testing. Calling it once at the
#' top of a test will silence BirdFlowR output for the test by setting
#' `verbose` to `FALSE` just in the calling scope.
#'
#' @param env Necessary to capture the calling environment and set that
#'  as the scope within which verbose is temporarily set. Leave at default
#'  value.
#'
local_quiet <- function(env = parent.frame()) {
  ov <- birdflow_options("verbose")
  birdflow_options(verbose = FALSE)
  withr::defer(birdflow_options(verbose = ov), env)
}
