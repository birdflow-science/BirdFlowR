test_that("preprocess_species runs on test dataset", {
  skip_on_cran()

  # Run on example data setting resolution based on gb (and then overiding for example_data)
  expect_no_error(a <- preprocess_species("example_data", hdf5 = FALSE, tiff = FALSE))
  expect_no_error(validate_BirdFlow(a, allow_incomplete = TRUE))
  expect_error(validate_BirdFlow(a))
  expect_true(all((ext(a)[,] %% xres(a)) == 0))  # Test if origin is at 0, 0


})


test_that("preprocess_species runs with pre-set resolution and matches prior results", {

  skip_on_cran()

  # Create and commit to cleaning up a temporary dir
  dir <- file.path(tempdir(), "preprocess_check")
  dir.create(dir, showWarnings = FALSE)
  on.exit(
    unlink(dir, recursive = TRUE)
  )

  # Using snapshot and write test
  # on 50 m version because it results in a small object.
  expect_no_error(
    b <- preprocess_species("example_data",
                            hdf5 = TRUE,
                            tiff = TRUE,
                            res = 50,
                            out_dir = dir,
                            treat_na_as_zero = FALSE
                            ))

  # Check if origin is at 0,0  - failed prior to 6/20/2023
  expect_true(all((ext(b)[,] %% xres(b)) == 0))


  expect_snapshot(b)

  # Snapshot added to verify that treat_na_as_zero = FALSE recreates old
  # behavior.  Snapshot created before function changes.
  # It shows row index and density for all non-zero cells at timestep 5
  d <- get_distr(b, 5)
  df <- data.frame(i = 1:length(d), density = d)
  df <- df[!df$density == 0, ]
  rownames(df) <- NULL
  expect_snapshot(df)

  created_files <- list.files(dir)
  expected_files <- c("example_data_2021_50km.hdf5",
                      "example_data_2021_50km.tif",
                      "example_data_2021_50km_lci.tif",
                      "example_data_2021_50km_uci.tif" )
  expect_setequal(created_files, expected_files)


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

  # Issue #106  (bad species input)
  expect_error(preprocess_species(NA), "species cannot be NA")
  expect_error(preprocess_species(NULL), "species cannot be NULL")
  expect_error(preprocess_species(
    species = c("American woodcock", "Chipping sparrow")),
    "Can only preprocess one species at a time")
  expect_error(
    preprocess_species(species = "Bad species", hdf5 = FALSE),
    '"Bad species" is not an eBird S&T species')

  expect_error(
    preprocess_species(species = "example_data", hdf5 = FALSE, res = 2),
    'res must be at least 27 when working with the low resolution example_data')

  expect_error(
    preprocess_species(species = "amewoo", hdf5 = FALSE, res = 2),
    'Resolution cannot be less than 3 km')

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








