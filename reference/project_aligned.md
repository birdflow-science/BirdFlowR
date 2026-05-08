# Project a SpatRaster onto a grid aligned to multiples of a target resolution

`project_aligned()` projects `x` into the target `crs` at resolution
`res_m`, with cells aligned to multiples of `res_m` (i.e. origin at 0,
0). It is a workaround for terra \>= 1.9-25, which errors with
`[write] unknown option(s): xscale,yscale` whenever `res` or `origin` is
passed alongside a CRS-string target in
[`terra::project()`](https://rspatial.github.io/terra/reference/project.html).
The template-raster path used here is unaffected.

## Usage

``` r
project_aligned(x, crs, res_m, method)
```

## Arguments

- x:

  A SpatRaster.

- crs:

  Target coordinate reference system as accepted by
  [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

- res_m:

  Target cell size in the units of `crs` (typically meters).

- method:

  Resampling method passed to
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html).

## Value

A SpatRaster in `crs` with `res(...) == c(res_m, res_m)` and cells
aligned to multiples of `res_m`.

## Details

Internally it projects the source extent (cheap), snaps outward to
multiples of `res_m` with one cell of buffer on each side (to absorb the
difference between corner-only extent projection and the true bounding
box of a non-rectangular reprojected raster), constructs an empty
template SpatRaster with that grid, then projects `x` against the
template.
