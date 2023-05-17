
#' Plot routes
#'
#' Plot routes as lines with color indicating the passage of time and dot size
#' indicating the length of stay at each stop.
#'
#' @param points The points component of the list returned by [route()] or
#' [route_migration()]
#' @param bf A BirdFlow object
#' @param facet if TRUE then use [ggplot2::facet_wrap()] to show each route
#' out into a separate subplot.
#' @param max_stay_len Used symbolize the stay length dots.  If omitted it will
#' be set to the maximum "stay_len" value in `points`.  Set it manually to
#' keep the dot scaling consistent across multiple plots.
#'
#'
#' @return a ggplot object.  Use [print()] to display it.
#' @export
#'
#' @examples
#'bf <- BirdFlowModels::amewoo
#'rts <- route_migration(bf, 4)
#'points <- rts$points
#'plot_routes(points, bf)
#'
#'plot_routes(points, bf, facet = TRUE)
plot_routes <- function(points, bf, facet = FALSE, max_stay_len = NULL){

  points$date <- lubridate::as_date(points$date)

  if(is.null(max_stay_len))
    max_stay_len <- max(points$stay_len)

  if(max_stay_len == 2) #
    max_stay_len <- 3

  stay_len_range <- c(2, max_stay_len)
  stay_len_breaks <- sort(c(2,5,10, max_stay_len))
  stay_len_breaks <- stay_len_breaks[stay_len_breaks <= max_stay_len]

  # The stops, unique locations where they stayed more than a week
  the_stops <- points[points$stay_len > 1, ]
  the_stops <- the_stops[ !duplicated(the_stops[, c("route", "stay_id")]), ]


  # Make raster showing which cells are active in the model
  rast <- rasterize_distr(get_distr(bf, 1), bf, format = "dataframe")
  rast$value <- !is.na(rast$density)

  coast <- get_coastline(bf)

  format_date <- function(x, format = birdflow_options("time_format")){
    x <- lubridate::as_date(x)

    if(format == "month_day"){
      return( paste(lubridate::month(x, label = TRUE, abbr = FALSE),
                    lubridate::day(x) ) )
    }

    if(format == "date"){
      return(as.character(x))
    }
  }

  p <- ggplot(data = points,
              aes(x = x, y = y)) +
   # theme_void() +
    theme(axis.title = element_blank()) +
    guides(fill="none") +
    scale_fill_manual(values=c(`TRUE`= rgb(.85, .85, .85, .5),
                               `FALSE`=rgb(.95, .95 ,.95, .5))) +
    geom_raster(data = rast,
                aes(fill=value)) +

    geom_point(data=the_stops,
               aes(size=stay_len,
                   color=date),
               alpha=.6) +

    geom_path(aes(color=date, group = route),
              linewidth =.75,
              lineend = "round",
              alpha=.8) +

      viridis::scale_color_viridis(labels = format_date, name = "Date") +

    scale_size_continuous(limits=stay_len_range,
                          range=c(.4,3),
                          breaks=stay_len_breaks,
                          name = "Stay Length") +

    theme(strip.background = element_blank()) +
    # coord_fixed(ratio =1) +
    geom_sf(data = coast,
            inherit.aes = FALSE,
            linewidth = .25,
            color = grey(.5))+
    coord_sf(expand = FALSE)


  p <- p + ggplot2::ggtitle(species(bf))


  if(facet){
    p <- p +
      facet_wrap(~route) +
      theme(axis.text.x=element_blank(), #remove x axis labels
            axis.ticks.x=element_blank(), #remove x axis ticks
            axis.text.y=element_blank(),  #remove y axis labels
            axis.ticks.y=element_blank()  #remove y axis ticks
      )
  }

  return(p)
}
