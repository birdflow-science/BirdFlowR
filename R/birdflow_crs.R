#' Define the coordinate reference system
#'
#' This defines the coordinate reference system (CRS, AKA projection)
#' used by default for BirdFlow models. It is a customized
#' Mollweide projection, with the longitude of origin set to -90 deg.
#' centering the western hemisphere.  St Louis, Missouri is very close to
#' a longitude of -90 deg. The Mollweide projection preserves area, shape
#' distortion increases with distance from the longitude of origin.
#'
#' The projection is similar to:
#'  [SR-ORG:7399](https://spatialreference.org/ref/sr-org/7399/) and
#'  [ESRI:54009](https://epsg.io/54009) which
#'   have a longitude of origin of 0, centered on Greenwich, England.
#' @export
#' @format This is a string defining a custom Mollweide projection
#' centered on the western hemisphere with well known text.
#' @references https://epsg.io/54009
#' @examples
#' cat(birdflow_crs)
#' crs(birdflow_crs, proj = TRUE)
#'
birdflow_crs <-
'PROJCRS["Western Mollweide",
    BASEGEOGCRS["WGS 84",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["Degree",0.0174532925199433]]],
    CONVERSION["Western Mollweide",
        METHOD["Mollweide"],
        PARAMETER["Longitude of natural origin",-90,
            ANGLEUNIT["Degree",0.0174532925199433],
            ID["EPSG",8802]],
        PARAMETER["False easting",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8807]]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["metre",1]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["metre",1]],
    USAGE[
        SCOPE["Not known."],
        AREA["World."],
        BBOX[-90,-180,90,180]]]'
