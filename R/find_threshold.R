#' function to determine the threshold in values that preserves a given
#' proportion of the value.
#'
#' @param x A vector of values
#' @param p A proportion of x that we wish retain
#' @param method Either `"weight"` to retain a proportion of the total weight
#' or `"values"` to retain a proportion of non-zero values.
#' @return A threshold in values of x such that dropping everything smaller than
#' the threshold retains at least proportion `p` of the total weight or
#' number of values in `x`
#' @keywords internal
find_threshold <- function(x, p, method = "weight") {

  stopifnot(is.numeric(p),
            length(p) == 1,
            !is.na(p),
            p >= 0,
            p <= 1)

  x <- sort(x[!is.na(x) & !x == 0], decreasing = TRUE)
  if (method == "weight") {
    target <- sum(x) * p
    cs <- cumsum(x)
    i <- which(cs >= target)[1]
    return(x[i])
  }

  if (method == "values") {
    i <- ceiling(length(x) * p)
    return(x[i])
  }

  stop('method should be either "weight" or "values"')

}
