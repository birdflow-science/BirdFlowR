test_that("preprocess_species runs on test dataset", {
  # Run on example data setting resolution based on gb (and then overiding for example_data)
  expect_no_error(a <- preprocess_species("example_data", hdf5 = FALSE, tiff = FALSE))
  expect_no_error(validate_BirdFlow(a, allow_incomplete = TRUE))
  expect_error(validate_BirdFlow(a))
})

test_that("preprocess_species runs with pre-set resolution and matches prior results", {
  # Using snapshot on 50 m version because it results in a small object.
  expect_no_error(b <- preprocess_species("example_data", hdf5 = FALSE, tiff = FALSE, res = 50))
  expect_snapshot(b)
})
