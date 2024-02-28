test_that("extend_birdflow() works with BirdFlow object in memory", {
  bf1 <- BirdFlowModels::amewoo
  # Define expanded extent for example
  buffer <- xres(bf1) * 3 # 3 cells converted to map units (m)

   e <-  ext(bf1) |> buffer_extent(buffer = buffer)
  expect_no_error(bf2 <- extend_birdflow(bf1, e))
  expect_no_error(validate_BirdFlow(bf2))

})

test_that("extend_birdflow() works with hdf5", {

  skip_on_cran()
  skip_on_ci()

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

  # For weird historical reasons:
  names(bfe1$metadata)[names(bfe1$metadata) == "birdFlowr_version"] <-
    "birdflowr_version"

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
