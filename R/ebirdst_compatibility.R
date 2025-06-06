#' @title ebirdst version compatibility functions:
#'
#' @description
#' Internal functions to facilitate working with both multiple versions of
#' of \pkg{ebirdst} despite the changes to the API between 2021 and 2022.
#'
#' @name ebirdst-compatibility
NULL

#' @section `ebirdst_pkg_ver()`:
#' `ebirdst_pkg_ver()` Look up the version of the currently installed
#' \pkg{ebirdst}.
#' @return `ebirdst_pkg_ver()`: The installed \pkg{ebirdst} package version or
#' `NA` if none.
#' @keywords internal
#' @rdname ebirdst-compatibility
ebirdst_pkg_ver <- function() {
  res <- tryCatch(utils::packageVersion("ebirdst"), error = identity)
  if (inherits(res, "error"))
    return(NA)
  res
}

#' @section `ebirdst_ver_supported()`:
#' `ebirdst_ver_supported()` Check whether the installed \pkg{ebirdst} is
#' supported by \pkg{BirdFlowR} for a particular use.
#' @param use The particular use that is being checked against. `package` is
#' whether all package functions are supported fully.  `preprocess_species`
#' checks that function which has stricter requirements than most functions.
#' `lookup_species_metadata` checks that function which is called by
#' `Routes()`.
#' @param throw_error if `TRUE` an error will be thrown if \pkg{ebirdst} is not
#' supported by `use`.
#' @return `ebirdst_ver_supported`: `TRUE` if the specified `use` is supported,
#'  `FALSE` otherwise.
#' @keywords internal
#' @rdname ebirdst-compatibility
ebirdst_ver_supported <- function(use = "package", throw_error = FALSE) {

  ver <- ebirdst_pkg_ver()

  earliest_supported <- "2.2021.0"

  # Set valid version ranges for different ebirdst use cases
  switch(use,
         package = { # (Entire package supported including preprocessing)
           not_yet_supported <- NA # NA for no limit
         },
         preprocess_species = {
           not_yet_supported <- NA# "3.2023.0"  |> as.package_version()
         },
         lookup_species_metadata = {
           not_yet_supported <-  NA
         },
         stop("Unsupported use argument for ebirdst_ver_supported(): ", use)
         )

  supported <- !is.na(ver) && ver >= earliest_supported &&
                  (is.na(not_yet_supported) || ver < not_yet_supported)

  if (throw_error && !supported) {
    stop("ebirdst version >= ", earliest_supported,
         ifelse(is.na(not_yet_supported), "",
                paste0(" and < ", not_yet_supported)),
         " required to use ", use, sep = "")
  }

  return(supported)
}




#' @section `res_label()`:
#' Convert resolution labels so they are appropriate for the
#' installed \pkg{ebirdst}
#'
#' \pkg{ebirdst} 3.2022.0 switched from "lr", "mr", and "hr" to
#'  "27km", "9km", and "3km" to indicate low, medium, and high resolution
#'  versions of the raster data in function arguments.
#'  [preprocess_species()] uses the older two letter
#'  versions but runs them through this function before calling \pkg{ebirdst}
#'  functions.
#'
#' @param res A resolution label. One of "lr", "mr", "hr", "27km", "9km", or
#' "3km
#'
#' @return `res_label()`: resolution labels appropriate for installed version
#' of \pkg{ebirdst}.
#' @rdname ebirdst-compatibility
#' @keywords internal
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

#' @section `date_to_week()`:
#'
#' This is a slightly modified copy of `ebirdst::date_to_st_week()` that
#' allows calculating weeks from dates without depending on \pkg{ebirdst}.
#'
#' @param dates a vector of dates that can be processed by `as.POSIXlt()`
#' @param version A numeric (year) version of the eBird date scheme to use.
#' 2021 for the older or 2022 for the newer; other values will be snapped to
#' the closest of those two. The output of
#' `ebirdst::ebirdst_version()$version_year` or
#' `get_metadata(bf, "ebird_version_year")` is appropriate.
#' @rdname ebirdst-compatibility
#' @return `date_to_week()`: A vector of week numbers associated with `dates`
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

#' @section `ebirdst_example_species()`:
#' Lookup the example species name that is appropriate for the
#' installed \pkg{ebirdst}.  The example species changed
#' from `"example_data"` to `"yebsap-example"` in version 3.2022.0.
#' @return `ebirdst_example_species()`: The example species name for
#'  \pkg{ebirdst}
#' @keywords internal
#' @rdname ebirdst-compatibility
ebirdst_example_species <- function() {
   ifelse(ebirdst_pkg_ver() < "3.2022.0",
                          "example_data",
                          "yebsap-example")
}
