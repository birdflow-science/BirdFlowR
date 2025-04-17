#' Print Routes and BirdFlowRoutes objects
#'
#' @description Print a summary of a `Routes` and `BirdFlowRoutes` objects
#' @aliases print.BirdFlowRoutes
#' @param x A `Routes` or `BirdFlowRoutes` object to print.
#' @param ... other arguments not used by this method.
#'
#' @return Invisibly returns the input object.
#' @method print Routes
#' @export
#'
#' @examples
#' # Create a Routes object
#' route_df <- data.frame(
#'   route_id = c("001", "001", "001", "001", "001",
#'   "003", "003", "003", "004"),
#'   date = as.Date(c("2025-01-01", "2025-01-08", "2025-01-15", "2025-01-21",
#'   "2025-02-10", "2025-03-01", "2025-05-01", "2025-06-01", "2025-05-01")),
#'   lon = c(-75.0060, -75.0060, -74.0060, -87.6298, -87.6298, -87.6298,
#'   -89.6298, -85.6298, -95.3698),
#'   lat = c(39.7128, 39.7128, 40.7128, 41.8781, 41.8781, 41.8781,
#'   42.8781, 40.8781, 29.7604),
#'   route_type = c("tracking", "tracking", "tracking", "tracking",
#'   "tracking", "motus", "motus", "motus", "motus")
#' )
#' routes <- Routes(route_df, species = list(common_name = "American Woodcock"))
#'
#' print(routes)
#'
#' # BirdFlowRoutes
#' bf <- BirdFlowModels::amewoo
#' bf_routes <- as_BirdFlowRoutes(routes, bf)
#'
print.Routes <- function(x, ...) {
  stopifnot(inherits(x, "Routes")) # TRUE for BirdFlowRoutes
  crossline <- "---------------------------------------------"
  cat(crossline, "\n")
  cat(sprintf("%s Object", class(x)[1]), "\n\n")

  pad_width <- 18
  species <- unlist(x$species[c(
    "common_name", "scientific_name",
    "species_code"
  )])
  if (!all(is.na(species))) {
    species <- species[!is.na(species)]
    cat(format("Species:", width = pad_width),
      paste(species, collapse = " / "), "\n",
      sep = ""
    )
  }

  types <- unique(x$data$route_type)
  type_label <- ifelse(length(types) > 1, "Types: ", "Type: ")
  pad <- function(x) format(x, width = pad_width)
  cat(format(type_label, width = pad_width), paste(types, collapse = ", "),
    "\n",
    sep = ""
  )
  cat(pad("Number of routes:"), length(unique(x$data$route_id)), "\n", sep = "")
  cat(pad("Number of points: "), nrow(x$data), "\n", sep = "")
  cat(pad("Date range:"),
    format(min(x$data$date)), ", ", format(max(x$data$date)), "\n",
    sep = ""
  )
  cat(pad("Longitude range:"),
    range(x$data$lon) |> format(nsmall = 4) |> paste(collapse = ", "),
    "\n",
    sep = ""
  )
  cat(pad("Latitude range:"),
    range(x$data$lat) |> format(nsmall = 4) |> paste(collapse = ", "),
    "\n",
    sep = ""
  )


  if (inherits(x, "BirdFlowRoutes")) {
    cat(pad("Dimensions:"), x$geom$nrow, ", ", x$geom$ncol,
      "  (nrow, ncol)\n",
      sep = ""
    )
    cat(pad("Resolution:"), paste(x$geom$res, collapse = ", "),
      " m (x, y)\n",
      sep = ""
    )
  }

  cat(crossline, "\n")

  print_type_breakdown(x, crossline)

  print_source(x, crossline)



  # Print the data.frame part
  print(utils::head(x$data, 5))
  if (nrow(x$data) > 5) {
    cat("(", nrow(x$data) - 5, " lines omitted)\n", sep = "")
  }

  invisible(x)
}




# ------------------------------------------------------------------------------
#  Helper functions for printing routes and intervals
# ------------------------------------------------------------------------------

#' Print summary of route types, their count, and number of points
#'
#' This internal helper function is for use in the print methods for the
#' route and interval data classes.
#'
#' It only prints the breakdown of routes by type if there is more than one type
#' of route in `x`.  When there's only one type the output would be redundant.
#'
#' @param x A `Routes`, `BirdFlowROutes` or `BirdFlowIntervals` object.
#' @param crossline Characters to be printed after printing the summary
#'
#' @returns invisibly returns the table
#' @keywords internal
print_type_breakdown <- function(x, crossline) {
  stopifnot(inherits(x, "Routes") || inherits(x, "BirdFlowIntervals"))

  # Only print type breakdown if there is more than one type
  if (length(unique(x$data$route_type)) == 1) {
    return()
  }

  summary <- x$data |>
    dplyr::group_by(.data[["route_type"]]) |>
    dplyr::summarize(
      Routes = dplyr::n_distinct(.data[["route_id"]]),
      Points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(Type = "route_type") |>
    as.data.frame()

  print(summary, row.names = FALSE)
  cat(crossline, "\n")

  invisible(summary)
}

# Helper function to print the source information from route and interval
# objects
print_source <- function(x, crossline) {
  stopifnot(inherits(x, "Routes") || inherits(x, "BirdFlowIntervals"))
  # source
  if (!is.null(x$source) && is.character(x$source) && !all(is.na(x$source))) {
    cat("Source", ifelse(length(x$source) > 1, "s", ""), ":\n", sep = "")
    cat(x$source, sep = "\n")
    cat(crossline, "\n")
  }
}
