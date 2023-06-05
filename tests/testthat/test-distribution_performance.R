
test_that("distribution_performance works", {

  # Full year run
  bf <- BirdFlowModels::amewoo
  expect_no_error(stats <- distribution_performance(bf))

  stats2 <- distribution_performance(bf,
                                     metrics = c("min_distr_cor",
                                                 "mean_distr_cor"))
  expect_equal(stats$min_distr_cor, stats2$min_distr_cor)
  expect_equal(stats$mean_distr_cor, stats2$mean_distr_cor)

  # Migration season
  expect_no_error(
    stats3 <- distribution_performance(bf,start = "prebreeding_migration"))

  a <- cbind(unlist(stats), unlist(stats3)) |> as.data.frame() |> round(4)
  colnames(a) <- c("year", "spring-migration")
  expect_snapshot(a)

})

test_that("distribution_performance reproduces end_traverse_cor metric", {

  # Modified version of evaluate_performance used by @slager
  evaluate_performance_route <- function (x, season = 'all')
  {
    if (!BirdFlowR::has_dynamic_mask(x))
      x <- BirdFlowR::add_dynamic_mask(x)
    season_timesteps <- BirdFlowR::lookup_season_timesteps(x, season)
    start <- season_timesteps[1]
    end <- utils::tail(season_timesteps, 1)

    start_distr_ebirdst <- BirdFlowR::get_distr(x, start, from_marginals = FALSE)
    start_distr_marginals <- BirdFlowR::get_distr(x, start, from_marginals = TRUE)
    start_dm <- BirdFlowR::get_dynamic_mask(x, start)
    start_cor <- stats::cor(start_distr_ebirdst[start_dm], start_distr_marginals[start_dm])
    end_distr_ebirdst <- BirdFlowR::get_distr(x, end, from_marginals = FALSE)
    projected <- stats::predict(x, distr = start_distr_marginals, start = start,
                                end = end, direction = "forward")
    end_dm <- BirdFlowR::get_dynamic_mask(x, end)
    end_traverse_cor <- stats::cor(end_distr_ebirdst[end_dm], projected[end_dm, ncol(projected)])
    # get_distr(bf, start, TRUE) |> rasterize_distr(bf) |> terra::plot()
    # get_distr(bf, start, FALSE) |> rasterize_distr(bf) |> terra::plot()
    # get_distr(bf, end, TRUE) |> rasterize_distr(bf) |> terra::plot()
    # get_distr(bf, end, FALSE) |> rasterize_distr(bf) |> terra::plot()
    list(start_cor = start_cor,
         end_traverse_cor = end_traverse_cor)
  }

  # Note currently distribution_performance()

  bf <- BirdFlowModels::amewoo

  # Full model
  a <- evaluate_performance_route(bf, season = "all")
  b <- distribution_performance(bf, start = "all")
  expect_equal(a$end_traverse_cor, b$md_traverse_cor)

  # prebreeding_migration only
  a  <- evaluate_performance_route(bf, season = "prebreeding_migration")
  b <- distribution_performance(bf, start = "prebreeding_migration")
  expect_equal(a$end_traverse_cor, b$md_traverse_cor)

})
