# Durable tests: synthetic legacy-shaped objects built by stripping/mutating
# fields on a current, valid model. These don't depend on BirdFlowModels'
# current shape and so survive the planned amewoo/rewbla refit (see
# dev/cran_prep.md Task 3).
#
# The dynamic-mask-addition step is intentionally *not* exercised via a
# synthetic fixture here: a real dynamic mask trims marginals down to the
# unmasked cells, so simply deleting geom$dynamic_mask from an already
# masked model (like amewoo) leaves marginals sized inconsistently with a
# "never masked" object and doesn't reproduce a real legacy shape. It's
# covered durably by the ambduc network test and transitionally by the
# real rewbla object below, both of which have marginals that genuinely
# have never been trimmed.

test_that("upgrade_birdflow() fixes the birdFlowr_version metadata typo", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  names(bf$metadata)[names(bf$metadata) == "birdflowr_version"] <-
    "birdFlowr_version"
  value <- bf$metadata$birdFlowr_version

  ubf <- upgrade_birdflow(bf)
  expect_null(ubf$metadata$birdFlowr_version)
  expect_equal(ubf$metadata$birdflowr_version, value)
})

test_that("upgrade_birdflow() backfills missing metadata fields", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  bf$metadata$clip <- NULL
  bf$metadata$trim_quantile <- NULL
  bf$metadata$ebird_coverage <- NULL
  bf$metadata$abundance <- NULL
  bf$metadata$ebirdst_version <- NULL
  bf$metadata$birdflowr_preprocess_version <- NULL

  ubf <- upgrade_birdflow(bf)
  defaults <- new_BirdFlow()$metadata
  expect_identical(ubf$metadata$clip, defaults$clip)
  expect_identical(ubf$metadata$trim_quantile, defaults$trim_quantile)
  expect_identical(ubf$metadata$ebird_coverage, defaults$ebird_coverage)
  expect_identical(ubf$metadata$abundance, defaults$abundance)
  expect_identical(ubf$metadata$ebirdst_version, defaults$ebirdst_version)
  expect_identical(ubf$metadata$birdflowr_preprocess_version,
                   defaults$birdflowr_preprocess_version)

  # Fields that are already present are never overwritten.
  expect_equal(ubf$metadata$birdflow_version, bf$metadata$birdflow_version)
  expect_equal(ubf$metadata$n_active, bf$metadata$n_active)
})

test_that("upgrade_birdflow() backfills timestep_padding", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  bf$metadata$timestep_padding <- NULL

  ubf <- upgrade_birdflow(bf)
  expect_equal(ubf$metadata$timestep_padding, get_timestep_padding(bf))
})

test_that("upgrade_birdflow() backfills dates$week", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo
  bf$dates$week <- NULL
  expect_equal(nrow(bf$dates), 52)

  ubf <- upgrade_birdflow(bf)
  expect_equal(ubf$dates$week, 1:52)
})

test_that("upgrade_birdflow() is idempotent", {
  skip_if_not_installed("BirdFlowModels")
  bf <- BirdFlowModels::amewoo

  scenarios <- list(
    typo = local({
      x <- bf
      names(x$metadata)[names(x$metadata) == "birdflowr_version"] <-
        "birdFlowr_version"
      x
    }),
    missing_metadata = local({
      x <- bf
      x$metadata$clip <- NULL
      x$metadata$ebirdst_version <- NULL
      x
    }),
    missing_padding = local({
      x <- bf
      x$metadata$timestep_padding <- NULL
      x
    }),
    missing_week = local({
      x <- bf
      x$dates$week <- NULL
      x
    })
  )

  for (name in names(scenarios)) {
    once <- upgrade_birdflow(scenarios[[name]])
    twice <- upgrade_birdflow(once)
    expect_identical(once, twice, info = name)
  }
})

test_that("upgrade_birdflow() is a no-op on an already-upgraded object", {
  skip_if_not_installed("BirdFlowModels")
  bf <- upgrade_birdflow(BirdFlowModels::amewoo)
  expect_identical(upgrade_birdflow(bf), bf)
})

# Durable test against ambduc, the real oldest model we explicitly commit to
# supporting going forward (see dev/cran_prep.md Task 4). Unlike the amewoo/
# rewbla tests below, this one keeps meaning the same thing after the
# BirdFlowModels -> BirdFlowExamples refit.
test_that("upgrade_birdflow() only backfills what's actually missing on ambduc", {
  skip_on_cran()
  bf <- load_model("ambduc",
                   collection_url =
                     "https://birdflow-science.s3.amazonaws.com/avian_flu/")
  expect_true(has_dynamic_mask(bf))

  ubf <- upgrade_birdflow(bf)
  expect_no_error(validate_BirdFlow(ubf))

  # Fields that were already present are untouched; only the newest
  # metadata fields (added since ambduc was created) get backfilled.
  for (nm in names(bf$metadata)) {
    expect_identical(bf$metadata[[nm]], ubf$metadata[[nm]], info = nm)
  }
  new_fields <- setdiff(names(ubf$metadata), names(bf$metadata))
  expect_true(length(new_fields) > 0)
})

# Transitional: today's real amewoo/rewbla objects predate the ambduc-level
# format (see dev/cran_prep.md Task 3 -- both are slated for a refit to the
# current format before this branch merges to main). Once that refit lands
# these assertions will likely collapse into no-ops, same as the ambduc test
# above, and can be simplified or removed then.
test_that("upgrade_birdflow() upgrades today's real amewoo/rewbla", {
  skip_if_not_installed("BirdFlowModels")

  bf <- BirdFlowModels::amewoo
  ubf <- upgrade_birdflow(bf)
  expect_no_error(validate_BirdFlow(ubf))

  bf <- BirdFlowModels::rewbla
  expect_false(has_dynamic_mask(bf))
  ubf <- upgrade_birdflow(bf)
  expect_true(has_dynamic_mask(ubf))
  expect_no_error(validate_BirdFlow(ubf))
})
