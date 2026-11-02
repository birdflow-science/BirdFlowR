# preprocess_species runs on test dataset

    Code
      df
    Output
           i     density
      1  176 4.64745e-06
      2  177 2.61422e-04
      3  178 5.60980e-04
      4  179 2.49322e-04
      5  183 1.94363e-04
      6  184 7.47251e-05
      7  187 5.38633e-04
      8  188 2.49201e-03
      9  189 4.69849e-03
      10 190 2.50841e-03
      11 191 1.93854e-04
      12 192 1.87247e-04

---

    Code
      ext(a)
    Output
      SpatExtent : 630000, 1350000, 660000, 1380000 (xmin, xmax, ymin, ymax)

---

    Code
      res(a)
    Output
      [1] 30000 30000

# preprocess_species() works with clip

    Code
      ext(b)
    Output
      SpatExtent : 810000, 1350000, 660000, 1320000 (xmin, xmax, ymin, ymax)
    Code
      res(b)
    Output
      [1] 30000 30000

