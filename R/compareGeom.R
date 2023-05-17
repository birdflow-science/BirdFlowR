# This implements compareGeom methods where one or both of the first
# two arguments is a BirdFlow obect, possible with the other argument
# being a SpatRaster.  They are all implemented by converting the
# BirdFlow objects to SpatRasters and then calling terra::compareGeom
#  this is inefficient as it's reformatting the data in the BirdFlow
# rather than using it as is, but it easy and clean and fast enough.



#  setOldClass("BirdFlow") allows S4 dispatch on S3 BirdFlow objects.
methods::setOldClass("BirdFlow")




#' @rdname compareGeom-BirdFlow
#' @aliases compareGeom-BirdFlow compareGeom
#' @name compareGeom
#' @title BirdFlow compareGeom methods
#'
#' @description
#' These are methods for [terra::compareGeom()] that work when one or both
#' of arguments are BirdFlow objects.
#'
#' @param x  A BirdFlow or SpatRaster object
#' @param y  A BirdFlow or SpatRaster object
#' @inheritDotParams terra::compareGeom
#' @importMethodsFrom terra compareGeom
#' @export
setMethod("compareGeom", signature(x = "BirdFlow", y = "BirdFlow"),
          function (x, y, ...) {
            x <- rast(x)
            y <- rast(y)
            return(terra::compareGeom(x, y, ...))
          }
)


#' @rdname compareGeom-BirdFlow
#' @export
setMethod("compareGeom", signature(x= "SpatRaster", y = "BirdFlow"),
          function (x, y, ...) {
            y <- rast(y)
            return(terra::compareGeom(x, y, ...))
          }
)

#' @rdname compareGeom-BirdFlow
#' @export
setMethod("compareGeom", signature(x = "BirdFlow", y = "SpatRaster"),
          function (x, y, ...) {
            x <- rast(x)
            return(terra::compareGeom(x, y, ...))
          }
)
