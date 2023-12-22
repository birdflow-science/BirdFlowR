#' Create a temporary directory for a test
#'
#' It will will be empty at start and cleared after test completes.
#' Based on concepts from:
#' https://testthat.r-lib.org/articles/test-fixtures.html#test-hygiene
#'
#' @param subdir The name of a directory within `tempdir()` to use for the test.
#' @param env
#'
#' @return the path to the test directory
#' @keywords internal
local_test_dir <- function(subdir = "test_dir", env = parent.frame()) {
  test_dir <- file.path(tempdir(), subdir)
  if (file.exists(test_dir))
    unlink(test_dir, recursive = TRUE)
  dir.create(test_dir)
  withr::defer(unlink(test_dir, recursive = TRUE), envir = env)
  return(test_dir)
}
