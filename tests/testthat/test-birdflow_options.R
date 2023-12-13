test_that("birdflow_options works", {

  expect_no_error(original <- birdflow_options())
  on.exit(birdflow_options(original))

  # Check default options for changes skip cache as it varies
  expect_snapshot(original[!names(original) == "cache"])

  alt <- list(
    cache = tempdir(),
    collection_url = "https://birdflow-science.s3.amazonaws.com/test/",
    max_param_per_gpu_gb = 24000000, time_format = "timestep",
    verbose = FALSE)


  birdflow_options(alt)

  reset <- birdflow_options()
  expect_equal(alt[-1], reset[-1])

  birdflow_options(original)
  expect_equal(original, birdflow_options())


  expect_error(birdflow_options("cache", "time_format"))
  expect_error(birdflow_options("garbage"),
               "is not a BirdFlowR configuration option")



})
