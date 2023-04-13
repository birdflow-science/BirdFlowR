test_that("get_distr() works", {
  bf <- BirdFlowModels::rewbla
  d <- column_sums <- get_distr(bf, 1:4)
  expect_equal(d, bf$distr[, 1:4], ignore_attr = TRUE)
  expect_snapshot(colnames(d))
  expect_equal(get_distr(bf, "2022-01-01"), bf$distr[, 1], ignore_attr = TRUE)

  expect_equal(get_distr(bf, "all"), bf$distr, ignore_attr = TRUE)

  expect_equal(get_distr(bf, lubridate::as_date("2022-12-28")),
               bf$distr[, 52], ignore_attr = TRUE)

})
