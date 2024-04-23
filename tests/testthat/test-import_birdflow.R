test_that("import_birdflow() works with preprocessed species", {
  local_quiet()
  skip_on_cran()
  dir <- withr::local_tempdir(pattern = "import_bf")
  bf1 <- preprocess_species("example_data", hdf5 = TRUE, out_dir = dir,
                            res = 200)
  f <- list.files(dir, "\\.hdf5$", full.names = TRUE)[1]

  bf2 <- import_birdflow(f)
  expect_equal(bf1, bf2)

  rexport <-  file.path(dir, "rexport.hdf5")
  export_birdflow(bf2, rexport)

  bf3 <- import_birdflow(rexport)
  expect_equal(bf1, bf3)

  expect_no_condition(validate_BirdFlow(bf3, allow_incomplete = TRUE))

})

test_that("export_birdflow() and import_birdflow() work with sparse models", {
  local_quiet()
  skip_on_cran()

  bf <- BirdFlowModels::amewoo |> truncate_birdflow(start = 10, end = 15)
  sbf <- sparsify(bf, method = "conditional", p = 0.99)

  ### Back compatability, Needed for BirdFlowModels 0.0.2.9002
  names(sbf$metadata)[names(sbf$metadata) == "birdFlowr_version"] <-
    "birdflowr_version"


  file <- withr::local_tempfile(fileext = ".hdf5")
  expect_no_error(export_birdflow(sbf, file = file))

  expect_no_error(sbf2 <- import_birdflow(file))

  ### back compatibility these will have to be deleted when we update the
  ### BirdFlowModels::amewoo model, Needed for BirdFlowModels 0.0.2.9002
  sbf2$metadata$ebirdst_version <- NULL
  sbf2$metadata$birdflowr_preprocess_version <- NULL


  expect_equal(sbf, sbf2)


})
