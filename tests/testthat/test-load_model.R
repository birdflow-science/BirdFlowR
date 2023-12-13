test_that("load_model() works", {
  skip_on_cran()
  expect_no_error(bf <- load_model("amewoo_prebreeding"))
})
