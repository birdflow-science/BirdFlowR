# rasterize_distr() with data.frame output

    Code
      hdf
    Output
               x       y   i     label        value order
      18 5025000 9525000 259 January 4 3.411255e-06     1
      19 5025000 9375000 272 January 4 1.634350e-07     1
      35 5175000 9975000 220 January 4 6.770740e-06     1

---

    Code
      hdf
    Output
      # A tibble: 3 x 6
              x       y     i label             value order
          <dbl>   <dbl> <int> <ord>             <dbl> <dbl>
      1 5025000 9525000   259 January 4  0.00000341       1
      2 5025000 9525000   259 January 11 0.0000000772     2
      3 5025000 9525000   259 January 18 0.000000592      3

# rasterize_distr() to dataframe works

    Code
      d
    Output
               x       y   i     label        value order
      18 5025000 9525000 259 January 4 3.411255e-06     1
      19 5025000 9375000 272 January 4 1.634350e-07     1
      35 5175000 9975000 220 January 4 6.770740e-06     1

