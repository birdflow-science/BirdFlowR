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


test_that("get_distr() type argument", {
  bf <- BirdFlowModels::rewbla

  # Default and "normalized" are identical
  expect_identical(get_distr(bf, 1:3),
                   get_distr(bf, 1:3, type = "normalized"))

  # "marginal" matches the legacy from_marginals = TRUE behavior
  suppressWarnings({
    legacy <- get_distr(bf, 1:3, from_marginals = TRUE)
  })
  expect_equal(get_distr(bf, 1:3, type = "marginal"), legacy,
               ignore_attr = TRUE)

  # "raw" needs abundance$totals — fixture predates this metadata, so
  # it must error with a helpful message.
  expect_error(get_distr(bf, 1, type = "raw"),
               "type = \"raw\" requires")
})


test_that("get_distr() emits a deprecation warning for from_marginals", {
  bf <- BirdFlowModels::rewbla
  expect_warning(get_distr(bf, 1, from_marginals = FALSE),
                 "`from_marginals` is deprecated")
  expect_warning(get_distr(bf, 1, from_marginals = TRUE),
                 "`from_marginals` is deprecated")
})



test_that("get_distr(type = \"raw\") works", {

  skip_on_cran()
  skip_if_unsupported_ebirdst_version(use = "preprocess_species")

  # Temporarily suppress BirdFlowR chatter
  local_quiet()

  expect_no_error(a <- preprocess_species("example_data",
                                          hdf5 = FALSE, res = 100))

  # `a` is a cyclical preprocessed model: distr has n_timesteps + 1
  # columns (the duplicate of column 1 added for fitting), and
  # abundance$totals is padded to match.
  expect_equal(length(a$metadata$abundance$totals), ncol(a$distr))

  raw <- get_distr(a, type = "raw")
  norm <- get_distr(a)
  expect_equal(dim(raw), dim(norm))

  # raw == norm * totals[t] elementwise.
  totals <- a$metadata$abundance$totals
  expected <- norm * matrix(rep(totals, each = nrow(norm)), nrow = nrow(norm))
  expect_equal(raw, expected, ignore_attr = TRUE)

  # The cyclical extra column is a duplicate of column 1 in both forms.
  expect_equal(raw[, ncol(raw)], raw[, 1], ignore_attr = TRUE)
})

