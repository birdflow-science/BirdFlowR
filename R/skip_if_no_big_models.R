#' helper function for unit testing with testthat
#'
#' This allows skipping test that require large model files.  In practice this
#' means that those tests are only run in my local development environment,
#' or, potentially, on another developer's local environment if they copy or
#' recreate the ../Models/ folder.  See `vignette("skipping")` in \pkg{testthat}
#' for more information.
#' @noRd
skip_if_no_big_models <- function() {
  # testthat helper function
  # see testthat:  vignette("skipping")
  if (!file.exists("../Models/run_big_tests.txt"))

  testthat::skip("Large model files are not available")

}
