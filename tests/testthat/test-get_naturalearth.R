test_that("get_coastline returns expected objects", {
  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs(paste0("+proj=moll +lon_0=-90 +x_0=0 +y_0=0",
                            " +ellps=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-1564958.2138293, 2514826.55772382,
                      2822153.67510416, 6421966.58574292)

  # Using "new" method
  expect_s3_class(coast <- get_coastline(bf), class = c("sf", "data.frame") )
  expect_s3_class(coast$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(coast$geometry[1],  c("sfc_LINESTRING", "sfc"))
  expect_true( sf::st_crs(coast) == sf::st_crs(crs(bf) ) )
  expect_equal(nrow(coast), 269 )

  # using "old" method
  expect_no_error(coast2 <- get_naturalearth(bf, type = "coastline",
                            force_old_method = TRUE) )
  expect_s3_class(coast2, c("sf", "data.frame"))
  expect_s3_class(coast2$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(coast2$geometry[1],  c("sfc_LINESTRING", "sfc"))
  expect_true( sf::st_crs(coast2) == sf::st_crs(crs(bf) ) )
  expect_equal(nrow(coast2), 231 )

  if(interactive()){
    # Due to buffering and reprojection issues the extent of the
    # two versions is slightly different
    plot(coast2)
    plot(coast, add = TRUE, col = rgb(0, 0, 1, .2), lwd = 4)
  }

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
  skip_on_cran()
  skip_on_ci()

  bf <- BirdFlowModels::amewoo
  expect_s3_class(states <-
                    get_states(bf, country = "United States of America"),
                  class = c("sf", "data.frame") )
  expect_s3_class(states$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(states$geometry[1],  c("sfc_MULTIPOLYGON", "sfc" ) )

  expect_true( sf::st_crs(states) == sf::st_crs(crs(bf) ) )

  expect_no_condition(states2 <- get_states(bf, scale = "lowres") )


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



test_that("get_naturalearth() works at edge of WGS84 with old method", {

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

  expect_no_error(
    coast <- get_naturalearth(bf, type = "coastline",
                              scale = 110, force_old_method = TRUE) )
  if(interactive()){
    plot(coast)
  }

  expect_s3_class(coast, c("sf", "data.frame"))
  expect_s3_class(coast$geometry, c("sfc_GEOMETRY", "sfc") )
  expect_s3_class(coast$geometry[1],  c("sfc_LINESTRING", "sfc"))
  expect_true( sf::st_crs(coast) == sf::st_crs(crs(bf) ) )
  expect_equal(nrow(coast), 24 )

})

test_that("get_naturalearth() works at edge of WGS84 with double wrapping", {
  seam_crs <- crs("+proj=moll +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")  # mollweide centered on 180 deg lon.
  bf <- new_BirdFlow()
  bf$geom$crs <- seam_crs
  bf$geom$ext <- c(-2000000, 2000000,-5000000, 1000000)

  expect_no_error(
    coast <- get_naturalearth(bf, type = "coastline",
                              scale = 110, force_old_method = TRUE, buffer = 180) )


})

test_that("get_naturalearth() works with mollweide and broken bounding box",{
  # Construct a psuedo BirdFlow object with the extent and
  #  projection that a user submitted.  This is a mollweide where
  # one corner of the bounding box is not on the map.

  bf <- new_BirdFlow()
  bf$geom$crs <- crs(paste0("+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +datum=WGS84",
  " +units=m +no_defs"))
  bf$geom$ext <- c(-12400447.5244956, 7148844.12665357,
                   3241411.87373662, 7978416.80488446)


  if(interactive()){
    # This failed during with the back transform extent method
    coast <- rnaturalearth::ne_coastline(returnclass = "sf") |>
      sf::st_transform( crs = crs(bf))

    plot(coast[ , "geometry"])
    plot(terra::ext(bf), add = TRUE, border = "red")
  }

  expect_no_error(coast2 <- get_coastline(bf))
  expect_equal(nrow(coast2), 668 )
})



test_that("get_naturalearth() works with lambert equal area (laea)",{
  # Construct a psuedo BirdFlow object
  bf <- new_BirdFlow()
  bf$geom$crs <- crs(paste0("+proj=laea +lat_0=39.161 +lon_0=-85.094 +x_0=0 ",
                            "+y_0=0 +datum=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-2410760.5, 1958109.5, -1548178, 1658294)

  expect_no_error(coast2 <- get_coastline(bf))
  expect_equal(nrow(coast2),  241 )  # original run through had 134

  if(interactive()){
    coast <- rnaturalearth::ne_coastline(returnclass = "sf") |>
      sf::st_transform( crs = crs(bf))
    plot(coast[ , "geometry"])
    plot(terra::ext(bf), add = TRUE, border = "red")
  }

})

test_that("get_naturalearth() issues appropriate warning with empty extent", {

  crs <- crs("+proj=moll +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs")  # mollweide centered on 180 deg lon.
  bf <- new_BirdFlow()
  bf$geom$crs <- crs
  bf$geom$ext <- c(-500000, 1000000, 1000000, 2000000)

  # Visualize empty extent in target projection
  if(interactive()){
    all_coast <- rnaturalearth::ne_coastline(returnclass = "sf")
    coast_proj <- sf::st_transform(all_coast, crs)
    op <- par( no.readonly = TRUE)
    par(oma = rep(0, 4), mar = rep(0, 4))
    plot(coast_proj[, "geometry"])
    terra::plot(terra::ext(bf), add = TRUE, border = "red")
    par(op)
  }

  # new method
  expect_warning( get_naturalearth(bf, "coastline", buffer = 0),
                  "No objects within extent. Returning empty sf object.")

  # old method
  expect_warning( get_naturalearth(bf, "coastline",  buffer = 0,
                                        force_old_method = TRUE),
                  "No objects within extent. Returning empty sf object.")

})

test_that("match_extent argument crops to input extent", {
  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs(paste0("+proj=moll +lon_0=-90 +x_0=0 +y_0=0",
                                   " +ellps=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-1560000, 2515000,
                   2820000, 6420000)

  expect_no_error(coast <- get_naturalearth(bf, type = "coastline",
                                            res = "lowres", match_extent = TRUE))
  # Note this depends on the coasts intersecting each edge of the extent
  expect_equal(as.numeric(terra::ext(coast)[1:4]),
               as.numeric(terra::ext(bf)[1:4]))

})


test_that("get_naturalearth() works with projections that have +units=us-ft", {

  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs("EPSG:2249")
  if(interactive()){
    print(crs(bf, proj = TRUE))
  }
  bf$geom$ext <- c(40000, 1100000, 2500000, 3500000)

  if (interactive()) {
    box <- sf::st_bbox(ext(bf)) |> sf::st_as_sfc()
    sf::st_crs(box) <- crs(bf)
    box_wgs84 <- sf::st_transform(box, sf::st_crs("EPSG:4326") )


    coast <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
    plot(coast[, "geometry", drop = FALSE])
    plot(box_wgs84, add = TRUE, border = "red")

    plot(box_wgs84)
    plot(coast[ , "geometry"], add = TRUE, col  = "blue")
  }

  expect_no_error(coast <- get_naturalearth(bf, type = "coastline",
                                            res = "small", buffer = 0))


})


test_that("Double wrapped buffer works.", {
  # Note this only sort of works because there are artifacts but
  # if you don't force the old method it works perfectly
  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs(paste0("+proj=moll +lon_0=-90 +x_0=0 +y_0=0",
                                   " +ellps=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-1560000, 2515000,
                   2820000, 6420000)

  expect_no_error(coast <- get_naturalearth(bf, "coastline",
                                            scale = "small", buffer = 180,
                                            force_old_method = TRUE))

  expect_equal(nrow(coast), 110)



})


test_that("Left wrapped buffer works.", {
  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs(paste0("+proj=moll +lon_0=-90 +x_0=0 +y_0=0",
                                   " +ellps=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-1560000, 2515000,
                   2820000, 6420000)

  expect_no_error(coast <- get_naturalearth(bf, "coastline",
                                            scale = "small", buffer = 100,
                                            force_old_method = TRUE))
})



test_that("Right wrapped buffer works.", {
  bf <- new_BirdFlow()
  bf$geom$crs <- terra::crs(paste0("+proj=moll +lon_0=+90 +x_0=0 +y_0=0",
                                   " +ellps=WGS84 +units=m +no_defs"))
  bf$geom$ext <- c(-1560000, 2515000,
                   2820000, 6420000)

  expect_no_error(coast <- get_naturalearth(bf, "coastline",
                                            scale = "small", buffer = 100,
                                            force_old_method = TRUE))
  expect_equal(nrow(coast), 86)
})






