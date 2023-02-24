* Updated get_naturalearth() so that it handles some cases in which it previously
failed.  In particular extents that span more than 180 deg of longitude, and 
extents that cross the 180 deg. meridian that defines the edge of the WGS84 
projection both now work. The function still doesn't handle polar projections or
global extents in most projections. Fixes [#14](https://github.com/birdflow-science/BirdFlowR/issues/14) 

# BirdFlowR 0.0.0.9001   2023-02-21
* Switched order of package installation in readme and added installation of
rnaturalearthdata to vignette [#11](https://github.com/birdflow-science/BirdFlowR/issues/11) 
* Added a `NEWS.md` file to track changes to the package.
