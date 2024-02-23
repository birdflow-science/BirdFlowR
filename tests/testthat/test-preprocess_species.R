
# 1
test_that("preprocess_species runs on test dataset", {

  skip_on_cran()
  skip_if_unsupported_ebirdst_version()

  # Temporarily suppress BirdFlowR chatter
  local_quiet()

  # Run on example data setting resolution based on gb (and then overriding for
  # example_data)
  expect_no_error(a <- preprocess_species("example_data", hdf5 = FALSE))
  expect_no_error(validate_BirdFlow(a, allow_incomplete = TRUE))
  expect_error(validate_BirdFlow(a))
  expect_true(all((ext(a)[, ] %% xres(a)) < 1e-9))  # Test if origin is at 0, 0

  # Snapshot test of first 12 non-zero values in the 5th distribibution
  d <- get_distr(a, 5)
  df <- data.frame(i = seq_along(d), density = d)
  df <- df[!df$density == 0, ]
  df <- df[1:12, ]
  rownames(df) <- NULL

  skip_if_wrong_ebirdst_for_snapshot()
  expect_snapshot(df)
  expect_snapshot(ext(a))
  expect_snapshot(res(a))

  ### Test ability to use preprocessed model for date and other basic lookup
  bf <- a
  expect_no_error(lookup_timestep("2022-05-01", bf))
  expect_no_error(n_active(bf))
  expect_no_error(get_dynamic_mask(bf))
  expect_no_error(get_distr(bf, 1))
  expect_no_error(ext(bf))
  expect_true(is_cyclical(bf))
  expect_equal(n_distr(bf), 53)
  expect_equal(n_timesteps(bf), 52)
  expect_equal(n_transitions(bf), 52)


})

#3
test_that("preprocess_species catches error conditions", {

  skip_if_unsupported_ebirdst_version()

  # Temporarily suppress BirdFlowR chatter
  local_quiet()

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

  # Local and standardized copy of ebirest::runs
  runs <- ebirdst::ebirdst_runs |> as.data.frame()
  runs[names(runs) == "resident"] <- "is_resident" # 2022 name


  # Pull out a resident species
  sp <- runs[runs$is_resident, "species_code"][1]

  expect_error(
    preprocess_species(sp, hdf5 = FALSE),
    paste0("is a resident (non-migratory) species and",
           " is therefore a poor candidate for BirdFlow modeling."),
    fixed = TRUE
  )

  if (ebirdst_pkg_ver() < "3.2002.0") {
    # Error when full range isn't modeled by ebirdst (ebirdst 2021)
    runs <- ebirdst::ebirdst_runs
    i <- which(!as.logical(runs$resident) &
                 !as.logical(runs$nonbreeding_range_modeled))[1]
    code <- runs$species_code[i]
    species <- runs$common_name[i]
    err <- paste0(
      "eBird status and trends models do not cover the full range for ",
      species, " (", code, ")")
    expect_error(preprocess_species(code, hdf5 = FALSE), err, fixed = TRUE)
  } else {
    # Error when data quality isn't high enough (ebirdst 2022)
    runs <- ebirdst::ebirdst_runs |> as.data.frame()
    i <- which(!runs$is_resident & runs$breeding_quality < 3)[1]
    code <- runs$species_code[i]
    species <- runs$common_name[i]
    err <- paste0(
      "eBird status and trends model quality is less than",
      " 3 in one or more seasons for ",
      species, " (", code, ")")
    expect_error(preprocess_species(code, hdf5 = FALSE), err, fixed = TRUE)

  }


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
    "res must be at least 27 when working with the low resolution example_data")

  expect_error(
    preprocess_species(species = "amewoo", hdf5 = FALSE, res = 2),
    "Resolution cannot be less than 3 km")

})

#4
test_that("preprocess_species() works with clip", {

  skip_on_cran()
  skip_if_unsupported_ebirdst_version()


  # Temporarily suppress BirdFlowR chatter
  local_quiet()

  # Create and commit to cleaning up a temporary dir
  dir <- local_test_dir("preprocess_check")

  # Create a clipping polygon for the ebirdst "example_data" species.
  # It's already clipped so here I'm reducing just a little more
  xmin <-  810000
  ymin <-  665000
  xmax <-  1550000
  ymax <-  1300000
  poly <- matrix(c(xmin, xmin, xmax, xmax, xmin,
                    ymin, ymax, ymax, ymin, ymin), ncol = 2) |>
    list() |>
    sf::st_polygon()

  clip <- terra::vect(poly)

  sp <- ebirdst_example_species()
  sp_path <- ebirdst::get_species_path(sp)


  if (ebirdst_pkg_ver() < "3.2022.0") {
    proj <- ebirdst::load_fac_map_parameters(sp_path)$custom_projection
  } else {
    proj <- ebirdst::load_fac_map_parameters(species = sp)$custom_projection
  }

  terra::crs(clip) <- terra::crs(proj)
  if (interactive()) {
    # Plot "Full" abundance for "example_data" and our clipping polygon
    a <- preprocess_species(ebirdst_example_species(), hdf5 = FALSE, res = 30)
    terra::plot(rast(a, 1))
    terra::plot(poly, add = TRUE)
  }

  expect_no_error(
    b <- preprocess_species(ebirdst_example_species(),
                            out_dir = dir,
                            hdf5 = TRUE,
                            clip = clip,
                            season = "prebreeding",
                            gpu_ram = 2
                            )
    )


  # Test that expect file was created
  created_files <- list.files(dir)
  expect_in(created_files,  c("example_data_2021_30km.hdf5",  # ebird 2021
                              "yebsap-example_2022_30km.hdf5")) # 2022

  skip_if_wrong_ebirdst_for_snapshot()

  expect_snapshot({
    ext(b)
    res(b)
  })
})


test_that("preprocess_species() works with crs arg", {
  skip_on_cran()
  skip_on_ci()

  skip_if_unsupported_ebirdst_version()

  # Create and commit to cleaning up a temporary dir
  dir <- local_test_dir("preprocess_crs")

  expect_no_error(
    bf <- preprocess_species(species = "example_data", res = 400, hdf5 = FALSE, crs = birdflow_crs)
  )
})
