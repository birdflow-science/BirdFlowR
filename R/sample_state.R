if(FALSE){
  # Examples


}


#'Sample locations from a model state
#'
#'This function treats `x` as one or more probability distributions and converts
#'each to a single location by assigning a 1 to one value and
#'converting the rest to 0.
#'
#'@param x a vector representing a single model state; or a matrix or array
#'  representing multiple states. If more than 1 dimension the first should be
#'  used to represent the locations. The values in x are treated as relative
#'  probability of the species being in each position.
#'@param n Only used if `x` is a vector representing a single model state, in
#'  which case that model state will be sampled `n` times to generate a
#'  matrix representing `n` sampled locations from that distribution.
#'@return an object with the same dimensions as `x` in which the distribution
#'  for each model state in `x` all the probability is assigned to a single
#'  location containing a 1 while the rest contain 0s.
#'@export
#'
#' @examples
#' # Generate a hypothetical state space vector
#' # representing the probability of finding a bird in each location.
#' # This is based on a normal distribution with higher probabilities in the
#' # center of the vector
#' s <- dnorm(seq(-2, 2, by = .25))
#' s <- s / sum(s)
#'
#'
#' # sample it once
#' loc <- sample_state(s)
#'
#' # sample it 200 times
#' locations <- sample_state(s, 200)
#'
#' \dontrun{
#' # Plot the probability of each location
#' plot(1:length(s), s, xlab = "location", ylab = "prob")
#' # Plot the number of times each location was selected
#' plot(1:length(s), apply(locations, 1, sum),
#'      xlab = "location", ylab = "times sampled")
#' }
sample_state <- function(x, n = 1){
  select_one <- function(x){
    # Takes a probability vector and randomly converts one value to 1
    # using the original values as probabilities
    i <- sample(1:length(x), size = 1, prob = x)
    r <- rep(0, length(x))
    r[i] <- 1
    return(r)
  }
  x <- as.array(x)
  ndim <- length(dim(x))

  if(ndim == 1){
    if(n == 1){# special, one dimensional case
      return(select_one(x))
    } else{
      # If n > 1 AND ndim == 1
      # repeat values in x, n times while converting to matrix
      x <- array(data = x, dim = c(length(x), n) )
      ndim <- 2
    }
  }

  # Select one from each state (if 2 or more dimensions)
  return(apply(x, 2:ndim, select_one))
}
