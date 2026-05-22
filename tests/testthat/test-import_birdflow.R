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
  for (m in c("trim_quantile", "clip", "ebird_model_coverage",
              "abundance")) {
    sbf2$metadata[[m]] <- NULL
  }


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

test_that("preprocess -> fit -> import preserves ebird_model_coverage", {

  # This test is only run on the Unity HPCC in all other situations it is
  # skipped

  on_unity <- dir.exists("/work/pi_drsheldon_umass_edu/birdflow_modeling")

  if(!on_unity)
     skip("This test requires Unity HPCC configured to fit models.")

  skip_if_unsupported_ebirdst_version(use = "preprocess_species")
  skip_if_not_installed("BirdFlowPipeline")

  local_quiet()

  # 1. Preprocess and write to HDF5. The cyclical extra timestep is
  #    present at this point: ebird_model_coverage has n_timesteps + 1
  #    layers, abundance$totals has n_timesteps + 1 entries, etc.
  dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/plunkett/zzz_temp_testing/"
  unlink(dir)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(dir))

  # 2. Fit the model with BirdFlowPy.
  # --------------------------------------------------------------
  # Takes a couple of minutes
  species <- "amewoo"
  res <- 200

  BirdFlowPipeline::batch_species(species, base_output_path = dir,
                                  hdf_path = dir, res = res,
                                  show_progress = TRUE)


  result_dir <- file.path(dir, paste0(species, "_", res, "km"))

  # Dir has preproecssed and post processed files.  Longest file is post.
  hdf_paths <- list.files(result_dir, pattern = "\\.hdf5$", full.names = TRUE)
  expect_equal(length(hdf_paths), 2)
  post_path <- hdf_paths[which.max(nchar(hdf_paths))]
  pre_path <- setdiff(hdf_paths, post_path)

  # The fit step adds marginals and hyperparameters to the HDF5 file
  # in place; the file is then a fitted model and `import_birdflow()`
  # will route through the `is_fitted_model` cleanup branch on the
  # next read.
  # --------------------------------------------------------------

  # 3. Import the preprocessed and fitted models.
  bf_pre <- import_birdflow(pre_path)
  bf_post <- import_birdflow(post_path)

  # 4. Inspect ebird_model_coverage on the imported, fitted model.
  cov <- bf_post$metadata$ebird_model_coverage
  expect_true(is.array(cov))
  expect_equal(length(dim(cov)), 3L)
  expect_equal(dim(cov)[1:2], dim(bf_post$geom$mask))
  # On a fitted model the cyclical extra layer has been trimmed, so
  # the time dimension equals n_timesteps(bf_post).
  expect_equal(dim(cov)[3], n_timesteps(bf_post))
  expect_type(cov, "logical")
  expect_equal(names(dimnames(cov)), c("row", "col", "time"))
  expect_equal(dimnames(cov)$time,
               paste0("t", seq_len(n_timesteps(bf_post))))

  # Coverage of the pre-fit model should match the imported,
  # fitted, layer-trimmed coverage at the corresponding timesteps.
  pre_cov <- bf_pre$metadata$ebird_model_coverage
  expect_equal(cov,
               pre_cov[, , seq_len(n_timesteps(bf_post)), drop = FALSE],
               ignore_attr = TRUE)

  # Clip polygon

  clip <- BirdFlowPipeline::set_pipeline_params()$clip
  clip2 <- get_clip(bf_post)

  # FAILS:
  expect_equal(clip, clip2)


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
