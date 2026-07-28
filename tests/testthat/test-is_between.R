test_that("is_between() works", {

  local_quiet()

  bf <- small_test_bf()

  between <- is_between(bf)

  expect_snapshot(sum(between[[1]]))


})
