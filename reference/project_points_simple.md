# Project points onto line segments (Euclidean/planar geometry)

Vectorized computation of the distance from each point in `P` to each
line segment defined by `S` (starts) and `E` (ends), and the distance
along the segment to the projection of the point.

## Usage

``` r
project_points_simple(s, e, p, m, n, v, vv, clamp = FALSE, tol = 0)
```

## Arguments

- s:

  An n x 2 matrix of segment start coordinates.

- e:

  An n x 2 matrix of segment end coordinates.

- p:

  An m x 2 matrix of point coordinates.

- m:

  Number of points (`nrow(p)`).

- n:

  Number of line segments (`nrow(s)`).

- v:

  `e - s`, precomputed segment vectors.

- vv:

  `rowSums(v^2)`, precomputed squared segment lengths.

- clamp:

  If `TRUE` constrain the projected fraction to `[0, 1]`. If `FALSE`
  (default) projections falling outside the segment are set to `NA`.

- tol:

  Segments with squared length at or below this threshold are treated as
  having zero length (projected fraction set to 0).

## Value

A list with `dist_to_line` and `dist_along`, each a vector of length
`m * n` (point index varying fastest).
