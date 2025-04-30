
test_that("dmi_to_i() is accurate", {

  bf <- BirdFlowModels::amewoo
  dmi <- 10
  timestep = 4
  this_dynamic_mask <- get_dynamic_mask(bf)[, timestep]

  # Make dynamic vector with indicated dmi hot
  d_vec <- rep(FALSE, sum(this_dynamic_mask))
  d_vec[dmi] <- TRUE

  # Make standard location/distribution vector with corresponding cell hot
  vec <- rep(FALSE, n_active(bf))
  vec[this_dynamic_mask] <- d_vec
  expected <- which(vec)

  expect_equal(dmi_to_i(dmi, timestep, bf), expected)
})

test_that("dmi_to_i() and i_to_dmi() are reversable", {

  library(BirdFlowR)
  bf <- BirdFlowModels::amewoo

  dm <- get_dynamic_mask(bf)

  dmi <- c(11:20)
  timesteps <- c(1, 1, 1, 1, 3, 3, 5,7:9 )


  i <- dmi_to_i(dmi, timesteps, bf)

  dmi2 <- i_to_dmi(i, timesteps, bf)

  expect_equal(dmi2, dmi)

})

