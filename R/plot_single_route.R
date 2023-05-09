
if(FALSE){
  bf <- BirdFlowModels::amewoo
  rts <- route_migration(bf, 5)

  p <- plot_single_route(rts$points, bf)

  bf <- a
  rts <- route_migration(bf, 100)

  p <-  plot_single_route(rts$points, bf, route = 4)

  p + geom_sf(data = rts$lines, inherit.aes = FALSE, aes(linewidth = 0.1)) +
    scale_linewidth_identity()

  pts <- rts$points

  p + geom_path(data = pts[pts$route %in% 1:10, ],
                mapping = aes(x = x, y = y, color = timestep),
                linewidth = .5,
                lineend = "round",
                alpha = 0.7) + viridis::scale_color_viridis()

  for(i in 1:10)
    print(plot_single_route(pts, bf, route = i))


}


plot_single_route <- function(points, bf, route, max_stay_len){

  if(missing(max_stay_len))
    max_stay_len <- max(points$stay_len)

  if(max_stay_len == 2)
    max_stay_len <- 3

  range_stay_len <- c(2, max_stay_len)

  # Defualt to first route
  if(missing(route))
    route <- unique(points$route[1])

  points <- points[points$route == route, , drop = FALSE]

  rast <- rasterize_distr(get_distr(bf, 1), bf, format = "data.frame")
  rast$value <- !is.na(rast$distr)

  # The stops, unique locations where they stayed more than a week
  the_stops <- points[points$stay_len > 1, ]
  the_stops <- the_stops[ !duplicated(the_stops[, c("route", "stay_id")]), ]


  p <- ggplot(data = points,
              aes(x = x, y = y)) +
    theme_void() +
    theme(axis.title = element_blank()) +
    guides(fill="none") +
    scale_fill_manual(values=c(`TRUE`=gray(.7), `FALSE` = gray(0.9)) ) +
    geom_raster(data = rast,
                aes(fill=value))  +

    geom_point(data=the_stops,
               aes(size=stay_len,
                   color=timestep),
               alpha=.6) +
    geom_path(aes(color=timestep), #weekno),
              linewidth=1,
              lineend = "round",
              alpha=.7) +

    viridis::scale_color_viridis() +
    scale_size_continuous(limits=range_stay_len,
                          breaks=c(2,5,10,max_stay_len))  +
    theme(strip.background = element_blank()) +
    coord_fixed(ratio =1)

  # Add coast as an overlay I couldn't figure out how to prevent the
  # coastline from expanding the extent of the plot so I'm clipping it
  # to the bf extent precisely, before I plot it.

  coast <- get_coastline(bf)
  bb <- sf::st_bbox(ext(bf))
  sf::st_crs(bb) <- crs(bf)
  coast <- sf::st_crop(coast, bb)

  p <- p + geom_sf(data = coast$geometry, inherit.aes = FALSE,
              mapping = aes(linewidth = 0.3)) + scale_linewidth_identity()

  return(p)
}
