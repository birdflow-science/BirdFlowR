# Routes() -> as_BirdFlowRoutes() with different aggregations works

    Code
      my_bfroutes$data[1:10, c("route_id", "i", "timestep")]
    Output
         route_id   i timestep
      1         1 244       10
      2         1 244       11
      3         1 105       12
      4         1 105       13
      5         1 105       14
      6         1 105       15
      7         2 204       10
      8         2 152       11
      9         2 152       12
      10        2  92       13

