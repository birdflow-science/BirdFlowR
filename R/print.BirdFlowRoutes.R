#' Print BirdFlow routes
#'
#' This is a print method for BirdFlowRoutes
#'  objects.
#
#' @param x A BirdFlowRoutes object.
#' @param ... arguments passed from other methods
#'
#' @return `x` is returned invisibly and unchanged.
#' @method print BirdFlowRoutes
#' @export
print.BirdFlowRoutes <- function(x, ...) {

  species <- attr(x, "species")$common_name
  if (!is.null(species) && !is.na(species)) {
    cat(species, " ", sep = "")
  }
  type <- attr(x, "metadata")$route_type
  if (!is.null(type) && !is.na(type)) {
    cat("Synthetic ", "BirdFlow Routes", sep = "")
  }
  cat("\n")

  print(as.data.frame(x))
  invisible(x)
}
