# preprocess_species runs with pre-set resolution and matches prior results

    Code
      b
    Output
      Yellow-bellied Sapsucker BirdFlow model
        dimensions   : 14, 15, 53  (nrow, ncol, ntimesteps)
        resolution   : 50000, 50000  (x, y)
        active cells : 106
        size         : 229.6 Kb

---

    Code
      df
    Output
           i     density
      1   80 0.060617695
      2   83 0.006551663
      3   84 0.009915038
      4   88 0.014393686
      5   89 0.006261535
      6   91 0.013321181
      7   94 0.040346037
      8   95 0.014045362
      9   97 0.017107539
      10  99 0.024529981
      11 100 0.217636672
      12 101 0.391725535
      13 102 0.149254750
      14 103 0.034293327

# preprocess_species() works with clip

    Code
      ext(b)
    Output
      SpatExtent : 810000, 1560000, 660000, 1320000 (xmin, xmax, ymin, ymax)
    Code
      res(b)
    Output
      [1] 30000 30000

