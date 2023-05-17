test_that("preprocess_species runs on test dataset", {
  skip_on_cran()

  # Run on example data setting resolution based on gb (and then overiding for example_data)
  expect_no_error(a <- preprocess_species("example_data", hdf5 = FALSE, tiff = FALSE))
  expect_no_error(validate_BirdFlow(a, allow_incomplete = TRUE))
  expect_error(validate_BirdFlow(a))
})


test_that("preprocess_species runs with pre-set resolution and matches prior results", {

  skip_on_cran()

  # Using snapshot on 50 m version because it results in a small object.
  expect_no_error(b <- preprocess_species("example_data", hdf5 = FALSE, tiff = FALSE, res = 50))
  expect_snapshot(b)
})

test_that("preprocess_species catches error conditions", {
  # Mostly testing these to close gaps in code coverage
  expect_error(preprocess_species(c("amewoo", "Western kingbird"),
                                  hdf5 = FALSE),
               "Can only preprocess one species at a time")
  expect_error(
    preprocess_species("example_data", res = 20),
    "res must be at least 27 when working with the low resolution example_data")

  expect_error(
    preprocess_species("example_data"),
    "Need an output directory. Please set out_dir.")

  expect_error(
    preprocess_species("example_data", out_dir = "junkdirectory_airnfpw"),
    "Output directory junkdirectory_airnfpw does not exist.")

  expect_error(
    preprocess_species("grerhe1", hdf5 = FALSE),
    paste0("Greater Rhea (grerhe1) is a resident (non-migratory) species and",
           " is therefore a poor candidate for BirdFlow modeling."),
    fixed = TRUE
  )

  # Error when full range isn't modeled by ebirdst
  runs <- ebirdst::ebirdst_runs
  i <- which(!as.logical(runs$resident) &
               !as.logical(runs$nonbreeding_range_modeled))[1]
  code <- runs$species_code[i]
  species <- runs$common_name[i]
  err <- paste0(
    "eBird status and trends models do not cover the full range for ",
    species, " (", code, ")" )
  expect_error(preprocess_species(code, hdf5 = FALSE), err, fixed = TRUE)
})


test_that("preprocess_species() works with clip", {

  skip_on_cran()

  # Create a clipping polygon for the ebirdst "example_data" species.
  # It's already clipped so here I'm reducing just a little more
  xmin <-  810000
  ymin <-  665000
  xmax <-  1550000
  ymax <-  1300000
  poly <- matrix(c( xmin, xmin, xmax, xmax, xmin,
                    ymin, ymax, ymax, ymin, ymin), ncol = 2 ) |>
    list() |>
    sf::st_polygon()

  clip <- terra::vect(poly)
  sp_path <- ebirdst::get_species_path("example_data")
  proj  <- terra::crs(ebirdst::load_fac_map_parameters(sp_path)$custom_projection)
  terra::crs(clip) <- proj
  if(interactive()){
    # Plot "Full" abundance for "example_data" and our clipping polygon
    a <- preprocess_species("example_data", hdf5 = FALSE)
    terra::plot(rast(a, 1))
    terra::plot(poly, add = TRUE)
  }

  expect_no_error(
    b <- preprocess_species("example_data",
                            hdf5 = FALSE,  clip = clip)
    )

  expect_snapshot({
    ext(b)
    res(b)
  })

})





