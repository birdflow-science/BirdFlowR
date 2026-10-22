# as_routes() -> as_birdflow_routes() with different aggregations works

    Code
      my_bfroutes$data[1:10, c("route_id", "i", "timestep")]
    Output
         route_id   i timestep
      1         1 126       10
      2         1 126       11
      3         1 126       12
      4         1 109       13
      5         1  71       14
      6         1  71       15
      7         2 124       10
      8         2 124       11
      9         2 124       12
      10        2  89       13

