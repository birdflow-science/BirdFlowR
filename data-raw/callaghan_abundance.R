# Format bird data for package

# Data from

# Callaghan, Corey T., Shinichi Nakagawa, and William K. Cornwell. "Global
#   abundance estimates for 9,700 bird species." Proceedings of the National
#   Academy of Sciences 118.21 (2021): e2023170118.

# https://www.pnas.org/doi/full/10.1073/pnas.2023170118

# Supplimental data downloaded from:
# https://www.pnas.org/doi/suppl/10.1073/pnas.2023170118/suppl_file/pnas.2023170118.sd01.xlsx  # nolint
# and saved from excel as .csv

# Read abundance data from paper
a <- readr::read_csv("data-raw/pnas.2023170118.sd01.csv",
                     col_types = readr::cols()) |> as.data.frame()

# Clean up names
n <- names(a) |> tolower()
n <- gsub("[[:blank:]]", "_", n)
n <- gsub("95%_", "", n)
names(a) <- n

# Note with the 2022 version year ebirdst only fit about half the species
# focusing mostly on North American species.

# Get eBird codes from auk
t <- auk::ebird_taxonomy

# Based on scientific name
mv <- match(a$scientific_name, t$scientific_name)
a$species_code <- t$species_code[mv]

# Based on common name
unmatched <- is.na(a$species_code)

a$species_code[unmatched] <- t$species_code[match(a$common_name[unmatched], t$common_name)]


# Determine which ones are in the current eBird version
ebird_ver <- ebirdst::ebirdst_version()$version_year
r <- ebirdst::ebirdst_runs
a[[paste0("in_ebird_", ebird_ver)]] <- a$species_code %in% r$species_code

callaghan_abundance <- a

usethis::use_data(callaghan_abundance, overwrite = TRUE)
