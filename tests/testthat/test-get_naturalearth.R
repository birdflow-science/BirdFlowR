test_that("get_coastline returns expected objects", {
  bf <- BirdFlowModels::amewoo
  expect_s3_class(coast <- get_coastline(bf), class = c("sf", "data.frame") )
  expect_s3_class(coast$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(coast$geometry[1],  c("sfc_LINESTRING", "sfc"))
  expect_true( sf::st_crs(coast) == sf::st_crs(crs(bf) ) )

})

test_that("get_countries returns expected objects", {

  bf <- BirdFlowModels::amewoo
  expect_s3_class(countries <- get_countries(bf),
                  class = c("sf", "data.frame") )
  expect_s3_class(countries$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(countries$geometry[1],  c("sfc_MULTIPOLYGON", "sfc" ) )
  expect_true( sf::st_crs(countries) == sf::st_crs(crs(countries) ) )
})

test_that("get_states returns expected objects", {
  # States requires rnaturalearthhires which is a large download
  # Use:  devtools::install_github("ropensci/rnaturalearthhires")
  # to install.
  skip_if_not_installed("rnaturalearthhires")

  bf <- BirdFlowModels::amewoo
  expect_s3_class(states <-
                    get_states(bf, country = "United States of America"),
                  class = c("sf", "data.frame") )
  expect_s3_class(states$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(states$geometry[1],  c("sfc_MULTIPOLYGON", "sfc" ) )

  expect_true( sf::st_crs(states) == sf::st_crs(crs(bf) ) )

})

test_that("get_naturalearth downloads and returns expected objects", {
  # This downloads data and writes it to disk so skip everywhere but local
  # machine
  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  bf <- BirdFlowModels::amewoo

  expect_s3_class(grat <- get_naturalearth(bf, type = "graticules_30",
                                           category = "physical"),
                  class = c("sf", "data.frame") )
  expect_s3_class(grat$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(grat$geometry[1],  c("sfc_LINESTRING", "sfc"))
  expect_true( sf::st_crs(grat) == sf::st_crs(crs(bf) ) )

})

test_that("get_naturalearth works with non-default scale", {

  skip_on_ci()
  skip_on_covr()
  skip_on_cran()

  bf <- BirdFlowModels::amewoo

  expect_no_error(
  grat_med <- get_naturalearth(bf, type = "graticules_30",
                               category = "physical", scale = "large")
  )

})


test_that("get_naturalearth() works at edge of WGS84", {

  # Construct a psuedo BirdFlow object that has a crs centered on the edge
  # of the wgs84 projection (used by rnaturalearth)
  seam_crs <- crs("+proj=moll +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")  # mollweide centered on 180 deg lon.
  bf <- new_BirdFlow()
  bf$geom$crs <- seam_crs
  bf$geom$ext <- c(-2000000, 2000000,-5000000, 1000000)

  # Visualize what we are hoping to do
  if(FALSE){ # This code is to run manually while debugging
    all_coast <- rnaturalearth::ne_coastline(returnclass = "sf")
    coast_proj <- sf::st_transform(all_coast, seam_crs)
    op <- par( no.readonly = TRUE)
    par(oma = rep(0, 4), mar = rep(0, 4))
    plot(coast_proj[, "geometry"])
    abline(v = c(-2000000, 2000000), col = "blue")
    abline(h = c(-5000000, 1000000), col = "red")

    par(op)
  }  # end skip visualization

  expect_no_error( coast <- get_coastline(bf, scale = 110) )
  expect_snapshot(coast) # eastern Australia and islands

})



