test_that("Build collection index works", {
  bf <- BirdFlowModels::amewoo
  bf1 <- truncate_birdflow(bf, start = 1, end = 3)
  bf2 <- truncate_birdflow(bf, start = 10, end = 13)

  # Create test directory and promise to delete it
  dir <- local_test_dir("collection_test")

  # Turn off messages (to not clutter up tests)
  local_quiet()

  # Write test files
  saveRDS(bf1, file.path(dir, "amewoo1-3.Rds"))
  saveRDS(bf2, file.path(dir, "amewoo10-13.Rds"))

  # Create fake html files
  writeLines(text = "test", file.path(dir, "amewoo1-3.html"))
  writeLines(text = "test", file.path(dir, "amewoo10-13.html"))

  collection_url <- "https://fake-url.com"

  expect_no_error(build_collection_index(dir, collection_url))

  expect_true(file.exists(file.path(dir, "index.html")))
  expect_true(file.exists(file.path(dir, "logo.png")))
  expect_true(file.exists(file.path(dir, "index.Rds")))
  expect_true(file.exists(file.path(dir, "index_md5.txt")))

})
