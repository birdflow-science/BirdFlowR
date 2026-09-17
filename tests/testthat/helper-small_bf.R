
#' A small, fast BirdFlow model for use in tests
#'
#' Truncates and sparsifies `BirdFlowModels::amewoo` down to a fixture
#' that's fast enough to use repeatedly across the test suite.
#'
#' @return A small `BirdFlow` model.
small_test_bf <- function() {
  testthat::skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 5)
  bf <- sparsify(bf, "conditional", .9, p_protected = 0.05)
  bf
}
