# preprocess_species runs on test dataset

    Code
      df
    Output
           i      density
      1  293 1.818734e-03
      2  294 5.461779e-03
      3  295 7.139737e-03
      4  296 3.648285e-03
      5  300 7.825097e-07
      6  301 6.058527e-04
      7  302 1.867900e-03
      8  303 1.879675e-03
      9  304 8.367624e-04
      10 305 2.113106e-03
      11 306 9.011818e-03
      12 307 2.274691e-02

---

    Code
      ext(a)
    Output
      SpatExtent : 690000, 1500000, 690000, 1410000 (xmin, xmax, ymin, ymax)

---

    Code
      res(a)
    Output
      [1] 30000 30000

# preprocess_species() works with clip

    Code
      ext(b)
    Output
      SpatExtent : 810000, 1500000, 690000, 1320000 (xmin, xmax, ymin, ymax)
    Code
      res(b)
    Output
      [1] 30000 30000

