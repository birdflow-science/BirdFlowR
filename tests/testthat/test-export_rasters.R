test_that("export_rasters() works with GeoTIFFs", {
  bf <- BirdFlowModels::amewoo
  bf <- add_dynamic_mask(bf) # Backwards compatibility
  dir <- file.path(tempdir(), "export_geotiff_test")
  if (dir.exists(dir))
    unlink(dir, recursive = TRUE)
  dir.create(dir)

  local_quiet()

  # GeoTIFF write
  expect_no_error(export_rasters(bf, dir, filetype = "GTiff"))

  # Expected files
  files <- sort(list.files(dir))
  n <- length(files)
  expect_snapshot(cat(files[c(1:8, (n-4):n)], sep = "\n"))


  # First exported distribution is very close to first distribution
  file <- file.path(dir, files[grep("distr", files)[1]])
  r <- terra::rast(file)
  suppressWarnings(d1 <- as_distr(r, bf))
  d2 <- get_distr(bf, 1)
  max_proportional_difference <- max(abs(d1 - d2) / d1, na.rm = TRUE)
  expect_true(max_proportional_difference < 1e-6)

  # Delete output
  unlink(dir, recursive = TRUE)

})

test_that("export_rasters() works with PNG and reprojection", {
  bf <- BirdFlowModels::amewoo
  bf <- add_dynamic_mask(bf) # Backwards compatibility
  dir <- file.path(tempdir(), "export_png_test")
  crs <- "EPSG:4326"
  if (dir.exists(dir))
    unlink(dir, recursive = TRUE)
  dir.create(dir)

  local_quiet()

  # PNG write
  expect_no_error(export_rasters(bf, dir, filetype = "PNG", crs = crs))

  # Expected files
  png_export_files <- sort(list.files(dir))
  n <- length(png_export_files)
  expect_snapshot(cat(png_export_files[c(1:8, (n-4):n)], sep = "\n"))

  # First exported distribution is very close to first distribution
  file <- file.path(dir, png_export_files[grep("distr", png_export_files)[1]])
  r <- terra::rast(file)
  suppressWarnings(d1 <- as_distr(r, bf)) # "average" interpolation
  d2 <- get_distr(bf, 1) # original distribution

  # Double transformation using nearest one way and average the other isn't
  # perfect.
  expect_true(cor(d1, d2) > 0.95)

  # Delete output
  unlink(dir, recursive = TRUE)

})
