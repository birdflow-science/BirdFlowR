

#' Print BirdFlow models
#'
#' This is a print method for BirdFlow objects.
#
#' @param x A BirdFlow object
#' @param ... arguments passed from other methods
#'
#' @return x is returned invisibly and unchanged
#' @export
print.BirdFlow <- function(x, ...){
  if(!is.na(x$spmd$species)){
    cat(x$spmd$species, " ", sep ="")
  }
  cat("BirdFlow model\n")

  nc <- 15 # n characters before colon
  pad <- function(x) stringr::str_pad(x, width = nc, side = "right", pad = " " )
  cat(pad("  dimensions"), ": ",
      paste(x$geom$nrow, x$geom$ncol, nrow(x$dates), sep = ", "),
      "  (nrow, ncol, ntimesteps)\n", sep = "")
  cat(pad("  resolution"), ": ", paste(x$geom$res, collapse = ", "),
      "  (x, y)\n", sep ="")
  cat(pad("  active cells"), ": ", sum(x$geom$m), "\n", sep = "")
  cat(pad("  size"), ": " , format(utils::object.size(x), unit =  "auto"),
      "\n", sep = "")  # drop size before package release
  invisible(x)
}
