# Functions defined here are available to test code

make_test_birdflow <- function(){
  # create pseudo bird flow object
  # lacks both transitions and marginals so can't be used to project
  #  has only 2 distributions (week 1 and week 2)

  bf <- BirdFlowR:::new_BirdFlow()
  bf$geom$ext <-  c(0, 100, 0, 90)
  bf$geom$res <- c(10, 10)
  bf$geom$mask <- matrix(FALSE, nrow = 9, ncol = 10)
  bf$geom$mask[4:6, ] <- TRUE
  bf$geom$mask[, 5:8] <- TRUE
  n <- sum(bf$geom$mask)
  bf$distr <- matrix(c(1:n, 100 + 1:n), ncol = 2, nrow = n)
  bf$dates <- BirdFlowR:::get_dates(2019)[1:2, ]
  bf$metadata$has_distr <- TRUE
  bf$geom$ncol <- 10
  bf$geom$nrow <- 9
  bf$n_active <- sum(bf$geom$mask)

  return(bf)

}
