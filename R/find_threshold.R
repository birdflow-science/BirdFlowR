#' function to determine the threshold in values that preserves a given
#' proportion of the value.
#'
#' @param x A vector of values
#' @param p A proportion of x that we wish retain
#'
#' @return A threshold in values of x such that dropping everything smaller than
#' the threshold retains at least proportion `p` of the total of `x`
#' @export
#' @keywords internal
find_threshold <- function(x, p){
  x <- sort(x[!is.na(x) & !x==0], decreasing = TRUE)
  target <- sum(x) * p
  cs <- cumsum(x)
  i <- which(cs >= target)[1]
  return(x[i])
}




