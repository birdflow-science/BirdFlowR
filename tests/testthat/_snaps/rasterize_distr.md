# rasterize_distr() with data.frame output

    Code
      hdf
    Output
                   x       y    i      time      density
      110 -1364968.8 4862048  936 January 4 5.290266e-07
      155 -1284973.0 4862048  937 January 4 9.646747e-06
      394  -884994.1 3742106 1442 January 4 3.851673e-07

---

    Code
      hdf
    Output
      # A tibble: 3 x 5
                x        y     i time           density
            <dbl>    <dbl> <int> <chr>            <dbl>
      1 -1364969. 4862048.   936 January 4  0.000000529
      2 -1364969. 4862048.   936 January 11 0.000000537
      3 -1364969. 4862048.   936 January 18 0.000000666

