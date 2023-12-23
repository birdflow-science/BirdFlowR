# export_rasters() works with GeoTIFFs

    Code
      cat(files[c(1:8, (n - 4):n)], sep = "\n")
    Output
      amewoo_crs.txt
      amewoo_distr_01.tif
      amewoo_distr_02.tif
      amewoo_distr_03.tif
      amewoo_distr_04.tif
      amewoo_distr_05.tif
      amewoo_distr_06.tif
      amewoo_distr_07.tif
      amewoo_mask_48.tif
      amewoo_mask_49.tif
      amewoo_mask_50.tif
      amewoo_mask_51.tif
      amewoo_mask_52.tif

# export_rasters() works with PNG and reprojection

    Code
      cat(png_export_files[c(1:8, (n - 4):n)], sep = "\n")
    Output
      amewoo_crs.txt
      amewoo_distr_01.png
      amewoo_distr_01.png.aux.xml
      amewoo_distr_02.png
      amewoo_distr_02.png.aux.xml
      amewoo_distr_03.png
      amewoo_distr_03.png.aux.xml
      amewoo_distr_04.png
      amewoo_mask_50.png.aux.xml
      amewoo_mask_51.png
      amewoo_mask_51.png.aux.xml
      amewoo_mask_52.png
      amewoo_mask_52.png.aux.xml

