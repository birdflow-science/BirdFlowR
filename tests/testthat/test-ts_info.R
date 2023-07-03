test_that("ts_info works", {
  # Forward loop
  a <- ts_info(c(1:5, 1))
  expect_true(a$loops)
  expect_equal(a$direction,  "forward")

  # Backward loop
  a <- ts_info(c(5:1, 5))
  expect_true(a$loops)
  expect_equal(a$direction, "backward")

  # Forward
  a <- ts_info(1:5)
  expect_false(a$loops) # Forward
  expect_equal(a$direction, "forward")

  # Backward
  a <- ts_info(5:1)
  expect_false(a$loops)
  expect_equal(a$direction, "backward")

  # Single timestep
  a <- ts_info(1)
  expect_false(a$loops)
  expect_true(is.na(a$direction))

})
