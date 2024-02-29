test_that("getmask works with numeric", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(m <- get_mask(bf, format = "numeric"))
  expect_equal(m, bf$geom$mask)
})

test_that("getmask works with data.frame", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(df <- get_mask(bf, format = "dataframe"))
  expect_equal(sort(unique(df$i)), seq_len(n_active(bf)))
  expect_equal(sum(df$mask), n_active(bf))
})

test_that("getmask works with SpatRaster", {
  bf <- BirdFlowModels::amewoo
  expect_no_error(r <- get_mask(bf, format = "SpatRaster"))
  expect_equal(as.vector(terra::values(r)), as.logical(t(bf$geom$mask)))
})
