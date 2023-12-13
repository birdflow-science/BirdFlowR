test_that("cache_path() works", {
  expect_no_error(p <-  cache_path())
  expect_true(is.character(p) && !is.na(p) && nchar(p) > 1 && length(p) == 1)
})
