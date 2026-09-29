#' Print BirdFlow models
#'
#' @description Print a summary of a `BirdFlow` object: species, raster
#' dimensions, resolution, number of active cells, and in-memory object
#' size.
#' @param x A BirdFlow object.
#' @param ... arguments passed from other methods
#'
#' @return `x` returned invisibly and unchanged.
#' @method print BirdFlow
#' @export
#' @examples
#' \donttest{
#' bf <- BirdFlowModels::amewoo
#' print(bf)
#' }
print.BirdFlow <- function(x, ...) {
  if (!is.na(x$species$common_name)) {
    cat(x$species$common_name, " ", sep = "")
  }
  cat("BirdFlow model\n")

  nc <- 15 # n characters before colon
  pad <- function(x) format(x, width = nc)
  cat(pad("  dimensions"), ": ",
    paste(nrow(x), ncol(x), n_timesteps(x), sep = ", "),
    "  (nrow, ncol, ntimesteps)\n",
    sep = ""
  )
  cat(pad("  resolution"), ": ", paste(res(x), collapse = ", "),
    "  (x, y)\n",
    sep = ""
  )
  cat(pad("  active cells"), ": ", n_active(x), "\n", sep = "")
  cat(pad("  size"), ": ", format(utils::object.size(x), unit = "auto"),
    "\n",
    sep = ""
  ) # drop size before package release
  invisible(x)
}
