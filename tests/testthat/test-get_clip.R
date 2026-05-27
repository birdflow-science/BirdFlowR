# Round-trip tests for clip metadata: sf polygon -> clip_to_dataframe ->
# BirdFlow metadata -> get_clip -> sf polygon. These should not depend on
# ebirdst, so they run unconditionally on CI.

make_clipped_bf <- function(clip_sf) {
  # Build a minimal BirdFlow with the clip metadata and CRS we need to
  # exercise get_clip(). Reuses the bundled amewoo fixture so we get a
  # valid `geom$crs` and other slots without doing real preprocessing.
  bf <- BirdFlowModels::amewoo
  bf$geom$crs <- sf::st_crs(clip_sf)$wkt
  bf$metadata$clip <- list(
    clipped = TRUE,
    polygon = clip_to_dataframe(clip_sf),
    percent_lost = rep(0, n_timesteps(bf))
  )
  bf
}


test_that("sf POLYGON round-trips through clip_to_dataframe / get_clip", {
  coords <- matrix(c(0, 0,
                     0, 100,
                     100, 100,
                     100, 0,
                     0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(coords)), crs = "EPSG:3857")
  clip <- sf::st_sf(geometry = poly)

  bf <- make_clipped_bf(clip)
  recovered <- get_clip(bf)

  expect_s3_class(recovered, "sf")
  # CRS objects can carry different `input` strings ("EPSG:3857" vs the
  # full WKT) for the same underlying projection, so use the
  # projection-aware equality operator.
  expect_true(sf::st_crs(recovered) == sf::st_crs(clip))
  expect_true(all(sf::st_equals(clip, recovered, sparse = FALSE)))
})


test_that("sf POLYGON with a hole round-trips", {
  outer <- matrix(c(0, 0,
                    0, 100,
                    100, 100,
                    100, 0,
                    0, 0), ncol = 2, byrow = TRUE)
  hole <- matrix(c(25, 25,
                   25, 75,
                   75, 75,
                   75, 25,
                   25, 25), ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(outer, hole)), crs = "EPSG:3857")
  clip <- sf::st_sf(geometry = poly)

  # The dataframe representation flags the inner ring as a hole.
  df <- clip_to_dataframe(clip)
  expect_true(any(df$hole))
  expect_false(all(df$hole))

  bf <- make_clipped_bf(clip)
  recovered <- get_clip(bf)

  expect_s3_class(recovered, "sf")
  expect_true(all(sf::st_equals(clip, recovered, sparse = FALSE)))
})


test_that("sf MULTIPOLYGON round-trips", {
  poly1 <- matrix(c(0, 0,
                    0, 10,
                    10, 10,
                    10, 0,
                    0, 0), ncol = 2, byrow = TRUE)
  poly2 <- matrix(c(20, 20,
                    20, 30,
                    30, 30,
                    30, 20,
                    20, 20), ncol = 2, byrow = TRUE)
  multi <- sf::st_sfc(
    sf::st_multipolygon(list(list(poly1), list(poly2))),
    crs = "EPSG:3857"
  )
  clip <- sf::st_sf(geometry = multi)

  bf <- make_clipped_bf(clip)
  recovered <- get_clip(bf)

  expect_s3_class(recovered, "sf")
  # MULTIPOLYGON survives as MULTIPOLYGON; geometric equality holds.
  expect_equal(as.character(sf::st_geometry_type(recovered))[1],
               "MULTIPOLYGON")
  expect_true(all(sf::st_equals(clip, recovered, sparse = FALSE)))
})


test_that("Americas round-trips", {
  skip_on_cran()
  skip_if_not_installed("BirdFlowPipeline")

  clip <- BirdFlowPipeline::americas_clip
  df <- clip_to_dataframe(clip)
  # CRS is not stored in the data frame; pass it back in so the recovered
  # sfc carries the same projection as the input.
  clip2 <- dataframe_to_clip(df, crs = sf::st_crs(clip))
  expect_equal(clip, clip2)
})


test_that("polygon with multiple holes round-trips through clip helpers", {
  # Regression test for the bug exposed by the americas clip: storing the
  # ring index as a logical collapsed multiple holes within one part into
  # a single merged ring.
  outer <- matrix(c(0, 0,
                    0, 100,
                    100, 100,
                    100, 0,
                    0, 0), ncol = 2, byrow = TRUE)
  h1 <- matrix(c(10, 10,
                 10, 20,
                 20, 20,
                 20, 10,
                 10, 10), ncol = 2, byrow = TRUE)
  h2 <- matrix(c(60, 60,
                 60, 80,
                 80, 80,
                 80, 60,
                 60, 60), ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(outer, h1, h2)), crs = "EPSG:3857")

  df <- clip_to_dataframe(poly)
  # Two distinct holes should keep distinct hole indices (1 and 2), not
  # both collapse to TRUE / 1.
  expect_equal(sort(unique(df$hole)), c(0L, 1L, 2L))

  back <- dataframe_to_clip(df, crs = sf::st_crs(poly))
  expect_true(all(sf::st_equals(poly, back, sparse = FALSE)))
})

