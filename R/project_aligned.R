#' Project a SpatRaster onto a grid aligned to multiples of a target resolution
#'
#' `project_aligned()` projects `x` into the target `crs` at resolution
#' `res_m`, with cells aligned to multiples of `res_m` (i.e. origin at 0,
#' 0). It is a workaround for terra >= 1.9-25, which errors with
#' `[write] unknown option(s): xscale,yscale` whenever `res` or `origin`
#' is passed alongside a CRS-string target in [terra::project()]. The
#' template-raster path used here is unaffected.
#'
#' Internally it projects the source extent (cheap), snaps outward to
#' multiples of `res_m` with one cell of buffer on each side (to absorb
#' the difference between corner-only extent projection and the true
#' bounding box of a non-rectangular reprojected raster), constructs an
#' empty template SpatRaster with that grid, then projects `x` against
#' the template.
#'
#' @param x A SpatRaster.
#' @param crs Target coordinate reference system as accepted by
#'   [terra::crs()].
#' @param res_m Target cell size in the units of `crs` (typically meters).
#' @param method Resampling method passed to [terra::project()].
#' @return A SpatRaster in `crs` with `res(...) == c(res_m, res_m)` and
#'   cells aligned to multiples of `res_m`.
#' @keywords internal
project_aligned <- function(x, crs, res_m, method) {
  e <- terra::project(terra::ext(x), from = terra::crs(x), to = crs)

  ext_snapped <- terra::ext(
    floor((as.numeric(e$xmin) - res_m) / res_m) * res_m,
    ceiling((as.numeric(e$xmax) + res_m) / res_m) * res_m,
    floor((as.numeric(e$ymin) - res_m) / res_m) * res_m,
    ceiling((as.numeric(e$ymax) + res_m) / res_m) * res_m
  )

  template <- terra::rast(crs = crs,
                          extent = ext_snapped,
                          resolution = res_m)
  terra::project(x, template, method = method)
}
