#' Buffer a SpatExtent
#'
#' `buffer_extent()` is a helper function used in testing `extend_birdflow()`.
#'
#' @param e An SpatExtent as produced by [terra::ext()]
#' @param buffer The buffer to add, in same units as the `SpatExtent`.
#' @return a buffered SpatExtent
buffer_extent <- function(e, buffer){
  e[1] <- e[1] - buffer
  e[2] <- e[2] + buffer
  e[3] <- e[3] - buffer
  e[4] <- e[4] + buffer
  return(e)
}
