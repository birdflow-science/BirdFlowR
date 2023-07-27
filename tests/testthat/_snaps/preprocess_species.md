# preprocess_species runs on test dataset

    Code
      df
    Output
           i      density
      1  268 0.0005224262
      2  269 0.0001803513
      3  273 0.0002856485
      4  274 0.0006623656
      5  275 0.0005602940
      6  282 0.0007482459
      7  283 0.0051824762
      8  284 0.0092532966
      9  285 0.0068716447
      10 286 0.0020618592
      11 288 0.0002184808
      12 289 0.0012916981

---

    Code
      ext(a)
    Output
      SpatExtent : 750000, 1590000, 660000, 1380000 (xmin, xmax, ymin, ymax)

---

    Code
      res(a)
    Output
      [1] 30000 30000

# preprocess_species() works with clip

    Code
      ext(b)
    Output
      SpatExtent : 750000, 1575000, 675000, 1350000 (xmin, xmax, ymin, ymax)
    Code
      res(b)
    Output
      [1] 75000 75000

