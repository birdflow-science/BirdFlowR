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

# preprocess_species runs with pre-set resolution and matches prior results

    Code
      b
    Output
      Yellow-bellied Sapsucker BirdFlow model
        dimensions   : 15, 15, 53  (nrow, ncol, ntimesteps)
        resolution   : 50000, 50000  (x, y)
        active cells : 108
        size         : 234.4 Kb

---

    Code
      df
    Output
           i     density
      1   78 0.020664370
      2   82 0.007168279
      3   85 0.023160431
      4   86 0.010406209
      5   87 0.004526905
      6   88 0.004736658
      7   89 0.009630820
      8   92 0.010154381
      9   94 0.012368245
      10  98 0.122366289
      11  99 0.221452686
      12 100 0.047254256
      13 104 0.023645934
      14 105 0.104935763
      15 106 0.181844564
      16 107 0.121305027
      17 108 0.074379183

# preprocess_species() works with clip

    Code
      ext(b)
    Output
      SpatExtent : 810000, 1560000, 660000, 1320000 (xmin, xmax, ymin, ymax)
    Code
      res(b)
    Output
      [1] 30000 30000

