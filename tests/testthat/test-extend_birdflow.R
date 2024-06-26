test_that("extend_birdflow() works with BirdFlow object in memory", {
  bf1 <- BirdFlowModels::amewoo
  # Define expanded extent for example
  cell_buffer <- 3
  buffer <- xres(bf1) * cell_buffer # 3 cells converted to map units (m)

   e <-  ext(bf1) |> buffer_extent(buffer = buffer)
  expect_no_error(bf2 <- extend_birdflow(bf1, e))
  expect_no_error(validate_BirdFlow(bf2))
  expect_equal(nrow(bf1) + 2 * cell_buffer, nrow(bf2))
  expect_equal(ncol(bf1) + 2 * cell_buffer, ncol(bf2))

})

test_that("extend_birdflow() works with hdf5", {

  skip_on_cran()
  skip_on_ci()

  local_quiet()

  dir <- withr::local_tempdir("extend_hdf5")

  bf <- BirdFlowModels::amewoo

  # Make new extent to extend to
  buffer <- xres(bf) * 3 # 3 cells converted to map units (m)
  e <-  ext(bf) |> buffer_extent(buffer = buffer)

  # Extend in memory
  bfe1 <- extend_birdflow(bf, e)

  # Write hdf5
  hdf <- file.path(dir, "amewoo.hdf5")
  export_birdflow(bf, hdf)

  # extend hdf5
  expect_true(extend_birdflow(hdf, e))
  bfe2 <- import_birdflow(hdf)

  ### Back compatibility code
  ### (delete metadata items that only exist in some model versions)
  names(bfe1$metadata)[names(bfe1$metadata) == "birdFlowr_version"] <-
    "birdflowr_version"
  bfe1$metadata$ebirdst_version <- NULL
  bfe2$metadata$ebirdst_version <- NULL
  bfe1$metadata$birdflowr_preprocess_version <- NULL
  bfe2$metadata$birdflowr_preprocess_version <- NULL


   # Reimporting changes the import version so nuke both
  bfe1$metadata$birdflowr_version <- ""
  bfe2$metadata$birdflowr_version <- ""

  expect_equal(bfe1, bfe2)
})

test_that("extend_birdflow() works with .rds", {

  skip_on_cran()
  skip_on_ci()

  dir <- withr::local_tempdir("extend_hdf5")
  bf <- BirdFlowModels::amewoo

  # Make new extent to extend to
  buffer <- xres(bf) * 3 # 3 cells converted to map units (m)
  e <-  ext(bf) |> buffer_extent(buffer = buffer)


  # Extend in memory
  bfe1 <- extend_birdflow(bf, e)

  # Write original
  rds <- file.path(dir, "amewoo.rds")
  export_birdflow(bf, rds, format = "rds")

  # Update rds on disk
  extend_birdflow(rds, e)

  # Reimport
  bfe2 <- readRDS(rds)

  expect_equal(bfe1, bfe2)


})
