# Routes() -> as_BirdFlowRoutes() -> as_BirdFlowIntervals() works

    Code
      my_routes
    Output
      --------------------------------------------- 
      Routes Object: 
      
        route_id       date      lon     lat route_type
      1      001 2025-01-01 -75.0060 39.7128   tracking
      2      001 2025-01-08 -75.0060 39.7128   tracking
      3      001 2025-01-15 -74.0060 40.7128   tracking
      4      001 2025-01-21 -87.6298 41.8781   tracking
      5      001 2025-02-10 -87.6298 41.8781   tracking
      6      003 2025-03-01 -87.6298 41.8781      motus
      7      003 2025-05-01 -89.6298 42.8781      motus
      8      003 2025-06-01 -85.6298 40.8781      motus
      9      004 2025-05-01 -95.3698 29.7604      motus
      
      Number of routes:  3 
      Number of points:  9 
      Date range:        2025-01-01 to 2025-06-01 
      Longitude range:   -95.3698 -74.006 
      Latitude range:    29.7604 42.8781 
      --------------------------------------------- 
      Route Type: motus
      Unique Routes: 2; Unique Points: 4
      
      Route Type: tracking
      Unique Routes: 1; Unique Points: 5
       
      --------------------------------------------- 
      Species:
      aa
      
      --------------------------------------------- 
      Source:
      $a
      [1] "1"
      
      $b
      [1] "2"
      
      --------------------------------------------- 

---

    Code
      my_bfroutes
    Output
      --------------------------------------------- 
      BirdFlowRoutes Object: 
      
        route_id          x         y   i      lon     lat timestep       date
      1      001  862606.04  109290.4 204 -75.0060 39.7128        1 2025-01-01
      2      001  862606.04  109290.4 204 -75.0060 39.7128        2 2025-01-08
      3      001  933778.96  229776.2 189 -74.0060 40.7128        3 2025-01-15
      4      001 -210524.73  304656.8 163 -87.6298 41.8781        6 2025-02-10
      5      003 -210524.73  304656.8 163 -87.6298 41.8781        9 2025-03-01
      6      003 -370583.77  422128.3 162 -89.6298 42.8781       18 2025-05-01
      7      003  -45168.77  190782.0 182 -85.6298 40.8781       22 2025-06-01
      8      004 -994397.19 -988364.6 307 -95.3698 29.7604       18 2025-05-01
        route_type stay_id stay_len
      1   tracking       1        7
      2   tracking       1        7
      3   tracking       2        0
      4   tracking       3        0
      5      motus       1        0
      6      motus       2        0
      7      motus       3        0
      8      motus       1        0
      
      Number of routes:  3 
      Number of points:  8 
      Date range:        2025-01-01 to 2025-06-01 
      Longitude range:   -95.3698 -74.006 
      Latitude range:    29.7604 42.8781 
      --------------------------------------------- 
      Route Type: motus
      Unique Routes: 2; Unique Points: 4
      
      Route Type: tracking
      Unique Routes: 1; Unique Points: 4
       
      --------------------------------------------- 
      Species: amewoo / Scolopax minor / American Woodcock 
       
      
      --------------------------------------------- 
      Source:
      $a
      [1] "1"
      
      $b
      [1] "2"
      
      --------------------------------------------- 

---

    Code
      my_intervals
    Output
      --------------------------------------------- 
      BirdFlowIntervals Object: 
      
        interval_id route_id        x1         x2       y1       y2  i1  i2     lon1
      1  interval_1      001  862606.0  862606.04 109290.4 109290.4 204 204 -75.0060
      2  interval_2      001  862606.0  933778.96 109290.4 229776.2 204 189 -75.0060
      3  interval_3      001  933779.0 -210524.73 229776.2 304656.8 189 163 -74.0060
      4  interval_4      001  862606.0 -210524.73 109290.4 304656.8 204 163 -75.0060
      5  interval_5      001  862606.0  933778.96 109290.4 229776.2 204 189 -75.0060
      6  interval_6      001  862606.0 -210524.73 109290.4 304656.8 204 163 -75.0060
      7  interval_7      003 -210524.7  -45168.77 304656.8 190782.0 163 182 -87.6298
      8  interval_8      003 -370583.8  -45168.77 422128.3 190782.0 162 182 -89.6298
      9  interval_9      003 -210524.7 -370583.77 304656.8 422128.3 163 162 -87.6298
            lon2    lat1    lat2      date1      date2 timestep1 timestep2 route_type
      1 -75.0060 39.7128 39.7128 2025-01-01 2025-01-08         1         2   tracking
      2 -74.0060 39.7128 40.7128 2025-01-08 2025-01-15         2         3   tracking
      3 -87.6298 40.7128 41.8781 2025-01-15 2025-02-10         3         6   tracking
      4 -87.6298 39.7128 41.8781 2025-01-08 2025-02-10         2         6   tracking
      5 -74.0060 39.7128 40.7128 2025-01-01 2025-01-15         1         3   tracking
      6 -87.6298 39.7128 41.8781 2025-01-01 2025-02-10         1         6   tracking
      7 -85.6298 41.8781 40.8781 2025-03-01 2025-06-01         9        22      motus
      8 -85.6298 42.8781 40.8781 2025-05-01 2025-06-01        18        22      motus
      9 -89.6298 41.8781 42.8781 2025-03-01 2025-05-01         9        18      motus
      
      Number of intervals:  9 
      Number of routes:  2 
      Date range:        2025-01-01 to 2025-06-01 
      Longitude range:   -89.6298 -74.006 
      Latitude range:    39.7128 42.8781 
      Minimum interval size:  7 days /  1 timesteps 
      MAximum interval size:  92 days /  13 timesteps 
      --------------------------------------------- 
      Route Type: motus
      Unique Routes: 1; Unique Points: 3
      
      Route Type: tracking
      Unique Routes: 1; Unique Points: 6
       
      --------------------------------------------- 
      Species: amewoo / Scolopax minor / American Woodcock 
       
      
      --------------------------------------------- 
      Source:
      $a
      [1] "1"
      
      $b
      [1] "2"
      
      --------------------------------------------- 

# If no intervals can be sampled, return NULL

    Code
      my_routes
    Output
      --------------------------------------------- 
      Routes Object: 
      
        route_id       date lon lat route_type
      1        1 2024-01-01 -90  40   tracking
      2        2 2024-01-02 -89  41    banding
      3        3 2024-01-03 -88  42    unknown
      
      Number of routes:  3 
      Number of points:  3 
      Date range:        2024-01-01 to 2024-01-03 
      Longitude range:   -90 -88 
      Latitude range:    40 42 
      --------------------------------------------- 
      Route Type: banding
      Unique Routes: 1; Unique Points: 1
      
      Route Type: tracking
      Unique Routes: 1; Unique Points: 1
      
      Route Type: unknown
      Unique Routes: 1; Unique Points: 1
       
      --------------------------------------------- 
      Species:
      aa
      
      --------------------------------------------- 
      Source:
      $a
      [1] "1"
      
      $b
      [1] "2"
      
      --------------------------------------------- 

---

    Code
      my_bfroutes
    Output
      --------------------------------------------- 
      BirdFlowRoutes Object: 
      
        route_id         x        y   i lon lat timestep       date route_type
      1        1 -418677.1 104481.4 196 -90  40        1 2024-01-01   tracking
      2        2 -328543.5 211299.2 180 -89  41        1 2024-01-02    banding
      3        3 -240792.8 319118.5 163 -88  42        1 2024-01-03    unknown
        stay_id stay_len
      1       1        0
      2       1        0
      3       1        0
      
      Number of routes:  3 
      Number of points:  3 
      Date range:        2024-01-01 to 2024-01-03 
      Longitude range:   -90 -88 
      Latitude range:    40 42 
      --------------------------------------------- 
      Route Type: banding
      Unique Routes: 1; Unique Points: 1
      
      Route Type: tracking
      Unique Routes: 1; Unique Points: 1
      
      Route Type: unknown
      Unique Routes: 1; Unique Points: 1
       
      --------------------------------------------- 
      Species: amewoo / Scolopax minor / American Woodcock 
       
      
      --------------------------------------------- 
      Source:
      $a
      [1] "1"
      
      $b
      [1] "2"
      
      --------------------------------------------- 

---

    Code
      my_intervals
    Output
      NULL

