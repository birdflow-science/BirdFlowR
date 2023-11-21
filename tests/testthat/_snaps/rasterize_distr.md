# rasterize_distr() with data.frame output

    Code
      hdf
    Output
                 x       y   i     label        value order
      79  -2025000 -225000 221 January 4 1.295109e-05     1
      101 -1875000 -225000 222 January 4 6.840829e-05     1
      119 -1725000  375000 154 January 4 8.941192e-05     1

---

    Code
      hdf
    Output
      # A tibble: 3 x 6
               x       y     i label          value order
           <dbl>   <dbl> <int> <ord>          <dbl> <dbl>
      1 -2025000 -225000   221 January 4  0.0000130     1
      2 -2025000 -225000   221 January 11 0.0000111     2
      3 -1875000 -225000   222 January 4  0.0000684     1

# rasterize_distr() to dataframe works

    Code
      d
    Output
                 x       y   i     label        value order
      79  -2025000 -225000 221 January 4 1.295109e-05     1
      101 -1875000 -225000 222 January 4 6.840829e-05     1
      119 -1725000  375000 154 January 4 8.941192e-05     1

