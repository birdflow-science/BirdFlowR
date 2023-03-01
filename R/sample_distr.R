#'Sample locations from a distribution
#'
#'This function treats `distr` as one or more probability distributions and converts
#'each to a single location by assigning a 1 to one value and
#'converting the rest to 0.
#'
#'@param distr a vector representing a single distribution; a matrix respresenting
#' one distribution per column, or (unlikely) an  array in which the first
#' dimension represents model states. The values in distr are treated as relative
#' probability of the species being in each position.
#'@param n Only used if `distr` is a vector representing a single model state, in
#'  which case that model state will be sampled `n` times to generate a
#'  matrix representing `n` sampled locations from that distribution.
#'@return An object ussually with the same dimensions as `distr` (but see `n`)
#   in which all the weight in each distribution in `distr` is assigned to a
#'   single location (containing a 1) while the remaining locations
#'   have 0's.
#'@export
#'
#' @examples
#' # Generate a hypothetical state space vector
#' # representing the probability of finding a bird in each location.
#' # This is based on a normal distribution with higher probabilities in the
#' # center of the vector.
#' s <- dnorm(seq(-2, 2, by = .25))
#' s <- s / sum(s)
#'
#' # sample it once
#' loc <- sample_distr(s)
#'
#' # sample it 200 times
#' locations <- sample_distr(s, 200)
#'
#' \dontrun{
#' # Plot the probability of each location
#' plot(1:length(s), s, xlab = "location", ylab = "prob")
#' # Plot the number of times each location was selected
#' plot(1:length(s), apply(locations, 1, sum),
#'      xlab = "location", ylab = "times sampled")
#' }
#'
sample_distr <- function(distr, n = 1){

  select_one <- function(x){
    # Takes a probability vector and randomly converts one value to 1
    # using the original values as probabilities
    i <- sample(1:length(x), size = 1, prob = x)
    r <- rep(0, length(x))
    r[i] <- 1
    return(r)
  }

  distr <- as.array(distr)
  ndim <- length(dim(distr))

  if(ndim == 1){
    if(n == 1){# special, one dimensional case
      return(select_one(distr))
    } else{
      # If n > 1 AND ndim == 1
      # repeat values in distr, n times while converting to matrix
      distr <- array(data = distr, dim = c(length(distr), n) )
      ndim <- 2
    }
  }

  # Select one from each state (if 2 or more dimensions)
  return(apply(distr, 2:ndim, select_one))
}
