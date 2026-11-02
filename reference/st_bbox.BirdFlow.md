# Get the bounding box of a BirdFlow model

[`st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
method for `BirdFlow` objects. Returns the spatial extent of the model's
raster geometry as an `sf` bounding box, with the model's coordinate
reference system attached.

## Usage

``` r
# S3 method for class 'BirdFlow'
st_bbox(obj, ...)
```

## Arguments

- obj:

  A `BirdFlow` object.

- ...:

  Not used.

## Value

An
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
object (class `bbox`) with the coordinate reference system of `obj`
attached.

## Examples

``` r
# \donttest{
bf <- BirdFlowExamples::amewoo
sf::st_bbox(bf)
#>     xmin     ymin     xmax     ymax 
#>  4950000  9150000  8250000 12150000 
# }
```
