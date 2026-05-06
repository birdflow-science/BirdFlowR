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

test_that("export_birdflow() and import_birdflow() work with NA in metadata", {

  #   https://github.com/birdflow-science/BirdFlowR/issues/168
  dir <- local_test_dir("na_metadata")

  skip_on_cran()

  local_test_dir()

  # These were NA in magfri
  na_items <- c("postbreeding_migration_start",
                "postbreeding_migration_end",
                "prebreeding_migration_start",
                "prebreeding_migration_end")

  f <- file.path(dir, "amewoo.hdf5")

  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 1, end = 3) # faster read/write in test
  bf$species[na_items] <- NA # Overwrite some species info with NA

  export_birdflow(bf, f)

  bf2 <- import_birdflow(f)


  # Metadata in BirdFlowModels::amewoo isn't always the same as the
  # current metadata, so I'm deleting both before comparison
  bf$metadata <- NULL
  bf2$metadata <- NULL

  expect_equal(bf, bf2)

})

test_that("import_birdflow() does not leak rhdf5 handles across calls", {
  local_quiet()
  skip_on_cran()

  bf <- BirdFlowModels::amewoo |> truncate_birdflow(start = 1, end = 3)
  f <- withr::local_tempfile(fileext = ".hdf5")
  export_birdflow(bf, f)

  # Prime: ensure no stray handles linger from earlier tests / setup.
  rhdf5::h5closeAll()

  # First import sets the baseline; subsequent imports must not increase
  # the count of open handles. Pre-fix this triggered the rhdf5 message
  # "An open HDF5 file handle exists ..." reported in #197.
  for (i in 1:3) import_birdflow(f)
  expect_length(rhdf5::h5validObjects(), 0L)
})
