
test_that("distribution_performance works", {


  bf <- BirdFlowModels::amewoo
  bf <- truncate_birdflow(bf, start = 5, end = 7)


  expect_no_error(stats <- distribution_performance(bf))
  stats2 <- distribution_performance(bf,
                                     metrics = c("min_distr_cor",
                                                 "mean_distr_cor"))
  expect_equal(stats$min_distr_cor, stats2$min_distr_cor)
  expect_equal(stats$mean_distr_cor, stats2$mean_distr_cor)

  # subset while running
  bf <- BirdFlowModels::amewoo
  expect_no_error(
    stats3 <- distribution_performance(bf, start = 5, end = 7)
  )
  expect_equal(stats, stats3)

  a <- cbind(unlist(stats)) |> as.data.frame() |> round(4)
  colnames(a) <- c("")
  expect_snapshot(a)

})

test_that("distribution_performance reproduces end_traverse_cor metric", {

  # Modified version of evaluate_performance used by @slager
  evaluate_performance_route <- function(x, season = "all") {
    if (!BirdFlowR::has_dynamic_mask(x))
      x <- BirdFlowR::add_dynamic_mask(x)
    season_timesteps <- BirdFlowR::lookup_season_timesteps(x, season)
    start <- season_timesteps[1]
    end <- utils::tail(season_timesteps, 1)

    start_distr_ebirdst <- BirdFlowR::get_distr(x, start,
                                                from_marginals = FALSE)
    start_distr_marginals <- BirdFlowR::get_distr(x, start,
                                                  from_marginals = TRUE)
    start_dm <- BirdFlowR::get_dynamic_mask(x, start)
    start_cor <- stats::cor(start_distr_ebirdst[start_dm],
                            start_distr_marginals[start_dm])
    end_distr_ebirdst <- BirdFlowR::get_distr(x, end, from_marginals = FALSE)
    projected <- stats::predict(x, distr = start_distr_marginals, start = start,
                                end = end, direction = "forward")
    end_dm <- BirdFlowR::get_dynamic_mask(x, end)
    end_traverse_cor <- stats::cor(end_distr_ebirdst[end_dm],
                                   projected[end_dm, ncol(projected)])
    list(start_cor = start_cor,
         end_traverse_cor = end_traverse_cor)
  }


  bf <- BirdFlowModels::amewoo |> truncate_birdflow(start = 10, end = 13)

  # On truncated model
  a <- evaluate_performance_route(bf, season = "all")
  b <- distribution_performance(bf, season = "all")
  expect_equal(a$end_traverse_cor, b$md_traverse_cor)


})

test_that("distribution_performance works accross year boundary", {
  bf <- BirdFlowModels::rewbla
  expect_no_error(a <- distribution_performance(bf, start = 51, end = 2))
})


test_that("distribution_performance works with individual metrics", {
  bf <- BirdFlowModels::amewoo
  all <- distribution_performance(bf, metrics = "md_traverse_cor",
                                   start = 1, end = 2)
  for (metric in names(all)) {
    expect_no_error(
      a <- distribution_performance(bf, metrics = metric,
                                    start = 1, end = 2))
    expect_equal(names(a), metric)
    expect_equal(a[[metric]], all[[metric]])
  }
})
