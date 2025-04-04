#' Print a BirdFlowIntervals Object
#'
#' @description Print method for `BirdFlowIntervals` objects,
#' summarizing interval data
#' and metadata, including temporal and spatial ranges.
#'
#' @param x A `BirdFlowIntervals` object to print.
#' @param ... other arguments not used by this method.
#'
#' @return Invisibly returns the input `birdflow_intervals` object.
#' @method print BirdFlowIntervals
#' @export
#'
#' @examples
#' # Create a BirdFlowIntervals object
#' interval_df <- data.frame(
#'     interval_id = 1:3,
#'     route_id = c("route1", "route1", "route2"),
#'     lon1 = c(-90, -89, -88),
#'     lon2 = c(-89, -88, -87),
#'     lat1 = c(40, 41, 42),
#'     lat2 = c(41, 42, 43),
#'     x1 = c(1000, 1100, 1200),
#'     x2 = c(1100, 1200, 1300),
#'     y1 = c(500, 600, 700),
#'     y2 = c(600, 700, 800),
#'     i1 = as.integer(c(1, 2, 3)),
#'     i2 = as.integer(c(2, 3, 4)),
#'     date1 = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'     date2 = as.Date(c("2024-01-02", "2024-01-03", "2024-01-04")),
#'     timestep1 = as.integer(c(1, 2, 3)),
#'     timestep2 = as.integer(c(2, 3, 4)),
#'     route_type = c("tracking", "tracking", "banding")
#' )
#' bf <- BirdFlowModels::amewoo
#' birdflow_intervals <- BirdFlowIntervals(interval_df,
#'     species = bf$species,
#'     metadata = NULL, geom = bf$geom, dates = get_dates(bf)
#' )
#'
#' print(birdflow_intervals)
#'
print.BirdFlowIntervals <- function(x, ...) {
    stopifnot(inherits(x, "BirdFlowIntervals"))
    crossline <- "---------------------------------------------"
    cat(crossline, "\n")
    cat(class(x)[1], "Object", "\n\n")


    pad_width <- 21


    species <- unlist(x$species[c(
        "common_name", "scientific_name",
        "species_code"
    )])
    if (!all(is.na(species))) {
        species <- species[!is.na(species)]
        cat(format("Species: ", width = pad_width),
            paste(species, collapse = " / ", sep = ""),
            "\n",
            sep = ""
        )
    }
    types <- unique(x$data$route_type)
    type_label <- ifelse(length(types) > 1, "Types: ", "Type:")


    cat(format("Number of intervals:", width = pad_width), nrow(x$data),
        "\n",
        sep = ""
    )
    cat(format("Number of routes:", width = pad_width),
        length(unique(x$data$route_id)), "\n",
        sep = ""
    )
    cat(format("Date range:", width = pad_width),
        format(min(x$data$date1, x$data$date2)), ", ",
        format(max(x$data$date1, x$data$date2)), "\n",
        sep = ""
    )
    cat(format("Longitude range:", width = pad_width),
        paste(range(x$data$lon1, x$data$lon2) |> round(6), collapse = ", "),
        "\n",
        sep = ""
    )
    cat(format("Latitude range:", width = pad_width),
        paste(range(x$data$lat1, x$data$lat2) |> round(6), collapse = ", "),
        "\n",
        sep = ""
    )

    cat(format("Min. interval size:", width = pad_width),
        min(as.numeric(x$data$date2 - x$data$date1, units = "days")),
        " days / ", min(x$data$timestep2 - x$data$timestep1), " timesteps",
        "\n",
        sep = ""
    )
    cat(format("Max. interval size:", width = pad_width),
        max(as.numeric(x$data$date2 - x$data$date1, units = "days")),
        " days / ",
        max(x$data$timestep2 - x$data$timestep1), " timesteps", "\n",
        sep = ""
    )
    cat(crossline, "\n")

    print_type_breakdown(x, crossline)

    print_source(x, crossline)

    # Print the data.frame part
    cat("Data:\n")
    print(utils::head(x$data, 5))
    if (nrow(x$data) > 5) {
        cat("(", nrow(x$data) - 5, " lines omitted)\n", sep = "")
    }
    cat(crossline, "\n")

    invisible(x)
}
