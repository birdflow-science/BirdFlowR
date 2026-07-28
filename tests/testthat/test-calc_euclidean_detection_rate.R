test_that("calc_euclidean_detection_rate() works", {
  local_quiet()
  bf <- small_test_bf()

  expect_no_error(between <- calc_euclidean_detection_rate(bf))
  expect_snapshot(sum(between[[1]]))
})

test_that("calc_euclidean_detection_rate() works with kernel = 'bb'", {
  local_quiet()
  bf <- small_test_bf()

  expect_no_error(between <- calc_euclidean_detection_rate(bf, kernel = "bb"))
  expect_snapshot(sum(between[[1]]))
})

test_that("calc_euclidean_detection_rate() forwards spread overrides", {
  local_quiet()
  bf <- small_test_bf()

  default <- calc_euclidean_detection_rate(bf)
  wider <- calc_euclidean_detection_rate(bf, gamma = 100000, kl = 10000)
  expect_false(isTRUE(all.equal(sum(default[[1]]), sum(wider[[1]]))))
})
