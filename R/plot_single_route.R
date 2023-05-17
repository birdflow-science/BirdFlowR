
if(FALSE){
  bf <- BirdFlowModels::amewoo
  rts <- route_migration(bf, 5)
  p <- plot_single_route(rts$points, bf)
}


#' plot_single_route()
#'
#' Function to plot a single route while using color to indicate when the bird
#' moved.
#'
#' This function is provisional and might change or be dropped.
#'
#' @param points The points component of a list returned by [route()]
#' or [route_migration()]
#' @param bf A BirdFlow object
#' @param route The route number to plot
#' @param max_stay_len This is used to determine the scaling of the circles
#' that indicate the stay length. To plot many routes consistently set this
#' to the maximum observed value across all routes.
#'
#' @return a ggplot object.  Use [print()] to display it.
#' @export
#' @examples
#'bf <- BirdFlowModels::amewoo
#'rts <- route_migration(bf, 5)
#'p <- plot_single_route(rts$points, bf)
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

  rast <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
  rast$value <- !is.na(rast$density)

  # The stops, unique locations where they stayed more than a week
  the_stops <- points[points$stay_len > 1, ]
  the_stops <- the_stops[ !duplicated(the_stops[, c("route", "stay_id")]), ]


  p <- ggplot(data = points,
              aes(x = x, y = y)) +
    theme_void() +
    theme(axis.title = element_blank()) +
    guides(fill="none") +
    scale_fill_manual(values=c(`TRUE`=gray(.8), `FALSE` = gray(0.95)) ) +
    geom_raster(data = rast,
                aes(fill=value))  +

    geom_point(data=the_stops,
               aes(size=stay_len,
                   color=timestep),
               alpha=.6) +
    geom_path(aes(color=timestep),
              linewidth=1,
              lineend = "round",
              alpha=.7) +

    viridis::scale_color_viridis() +
    scale_size_continuous(limits=range_stay_len,
                          breaks=c(2,5,10,max_stay_len))  +
    theme(strip.background = element_blank()) +
    coord_fixed(ratio =1)


  coast <- get_coastline(bf)

  p <- p + geom_sf(data = coast$geometry, inherit.aes = FALSE,
              mapping = aes(linewidth = 0.3)) + scale_linewidth_identity()

  return(p)
}
