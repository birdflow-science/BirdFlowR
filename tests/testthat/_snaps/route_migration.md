# route_migration() works with n = 1

    Code
      rts
    Output
      $points
                 x       y route timestep       date
      1   714921.5 4222081     1        2 2019-01-11
      2   714921.5 4222081     1        3 2019-01-18
      3   714921.5 4222081     1        4 2019-01-25
      4   794917.3 4382073     1        5 2019-02-01
      5   794917.3 4382073     1        6 2019-02-08
      6   794917.3 4622060     1        7 2019-02-15
      7   954908.9 4862048     1        8 2019-02-22
      8   954908.9 4862048     1        9 2019-03-01
      9   954908.9 4862048     1       10 2019-03-08
      10  954908.9 4862048     1       11 2019-03-15
      11  954908.9 4862048     1       12 2019-03-22
      12  954908.9 4862048     1       13 2019-03-29
      13  954908.9 4862048     1       14 2019-04-05
      14  954908.9 4862048     1       15 2019-04-12
      15  954908.9 4862048     1       16 2019-04-19
      16  954908.9 4862048     1       17 2019-04-26
      17  954908.9 4862048     1       18 2019-05-03
      18 1194896.2 5742002     1       19 2019-05-10
      19 1194896.2 5742002     1       20 2019-05-17
      
      $lines
      Simple feature collection with 1 feature and 1 field
      Geometry type: LINESTRING
      Dimension:     XY
      Bounding box:  xmin: 714921.5 ymin: 4222081 xmax: 1194896 ymax: 5742002
      CRS:           PROJCRS["unknown",
          BASEGEOGCRS["GCS_unknown",
              DATUM["D_Unknown_based_on_WGS84_ellipsoid",
                  ELLIPSOID["WGS 84",6378137,298.257223563,
                      LENGTHUNIT["metre",1],
                      ID["EPSG",7030]]],
              PRIMEM["Greenwich",0,
                  ANGLEUNIT["Degree",0.0174532925199433]]],
          CONVERSION["unnamed",
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
                  LENGTHUNIT["metre",1,
                      ID["EPSG",9001]]],
              AXIS["(N)",north,
                  ORDER[2],
                  LENGTHUNIT["metre",1,
                      ID["EPSG",9001]]]]
        route                       geometry
      1     1 LINESTRING (714921.5 422208...
      

# route_migration() works with n > 1

    Code
      novel_points
    Output
                   x       y route timestep       date
      1   -85036.287 5502014     1       37 2019-09-13
      3    -5040.507 5342023     1       39 2019-09-27
      9  -325023.626 4222081     1       45 2019-11-08
      13 -245027.847 4062089     1       49 2019-12-06
      16   74955.273 5502014     2       37 2019-09-13
      22   -5040.507 5422019     2       43 2019-10-25
      23   74955.273 3982093     2       44 2019-11-01
      27  154951.053 3982093     2       48 2019-11-29
      31 -325023.626 5742002     3       37 2019-09-13
      32 -325023.626 5821998     3       38 2019-09-20
      36  234946.832 4382073     3       42 2019-10-18

