
#' look up ebirdst version
#'
#' @return ebirdst version or NA if not installed
#' @keywords internal
ebirdst_pkg_ver <- function() {
  res <- tryCatch(packageVersion("ebirdst"), error = identity)
  if (inherits(res, "error"))
    return(NA)
  res
}


#' Lookup up resolution label for installed ebirdst package version
#'
#' @param res A resolution label. One of "lr", "mr", "hr", "27km", "9km", or
#' "3km
#'
#' @return The resolution label in the format used by the currently installed
#' version of ebirdst.  With the 3.2002.0 they switched from the the
#' abbreviation versions  ("lr") to the numeric resolution versions ("3km")
#'
#' @examples
#'   res_label("hr")
#'   res_label(c("lr", "mr", "hr"))
res_label <- function(res) {

  crosswalk <- data.frame("v2021" = c("lr", "mr", "hr"),
                          "v2022" = c("27km", "9km", "3km"))

  if (ebirdst_pkg_ver() < "3.2022.0") {
    alt_col <- "v2022"
    valid_col <- "v2021"
  } else {
    alt_col <- "v2021"
    valid_col <- "v2022"
  }
    sv <- res %in% crosswalk[[alt_col]]
    res[sv] <- crosswalk[[valid_col]][match(res[sv], crosswalk[[alt_col]])]
    if (!all(res %in% crosswalk[[valid_col]]))
      stop("res should be one of ", paste(c(crosswalk[[alt_col]],
                                            crosswalk[[valid_col]]),
                                          collapse = ", "))

  res
}


#' lookup ebirst weeks from dates
#'
#' This is a slightly modified copy of ebirdst::date_to_st_week() that
#' allows calculating weeks from dates without depending on ebirdst.
#'
#' @param dates a vector of dates that can be processed by as.POSIXlt()
#' @param version What version of the ebirdst date scheme to use. 2021 for
#' the older, and 2022 for the newer version first used in
#' ebirdst v 3.2022.0  All other values will be reset to whichever version
#' is closer.
#'
#' @return A vector of weeks (1 to 52) corresponding with `dates`
#' @keywords internal
date_to_week <- function(dates, version = 2022) {
  stopifnot(is.numeric(version),
            length(version) == 1,
            !is.na(version))

  # Old scheme 2021 and earlier ebirdst_version_years
  if (version <= 2021.5) {
    dv <- seq(from = 0, to = 1, length.out = 52 + 1)
    days <- (as.POSIXlt(dates)$yday + 0.5) / 366
    return(findInterval(days, dv))
  }

  # New ebirdst v 3.2022.0 scheme, 2022 and later ebirdst_version_years
  breaks <- c(-Inf, seq(7.5, 357.5, 7), Inf)
  jd <- as.POSIXlt(dates)$yday + 1
  return(findInterval(jd, breaks))
}


ebirdst_example <- function(){
   ifelse(ebirdst_pkg_ver() < "3.2022.0",
                          "example_data",
                          "yebsap-example")
}

