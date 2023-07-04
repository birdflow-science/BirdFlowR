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

