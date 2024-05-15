
#' Abundance Estimates from Callaghan et. al. 2021
#'
#' The Callaghan et. al. (2021) population estimates and 95% confidence
#' interval for 9700 species.
#'
#' @format A data frame with 9,700 rows and 11 columns:
#' \describe{
#'   \item{`common_name`}{Common name}
#'   \item{`scientific_name`}{Scientific name}
#'   \item{`order`, `family`}{Taxonomic order and family}
#'   \item{`lower_ci`}{The lower 95% confidence interval}
#'   \item{`abundance_estimate`}{Estimated population size}
#'   \item{`upper_ci`}{The upper 95% confidence interval}
#'   \item{`range_adjusted`}{`TRUE` indicates that the area of the species
#'   range from a source other than the eBird pixels was used to adjust the
#'   abundance estimate.  If an independent range area was not available than
#'   the area of the eBird cells was used.  See Callaghan et. al. 2021"}
#'   \item{`training_species`}{`TRUE` if the species was used to train the
#'   abundance model (Callaghan et. al. 2021)}
#'   \item{`species_code`}{The eBird species code derived by joining
#'   `scientific_name` to the `ebird_taxonomy` in the **auk** package.
#'   `NA` indicates that the scientific name from Callaghan et. al. did not
#'   match any scientific names in the eBird taxonomy table.}
#'   \item{`in_ebird_2022`}{Is the `species_code` in `ebirds::ebirdst_runs` from
#'   the 2022 version of the eBird package. See: [ebirdst::ebirdst_version()]
#'   and [ebirdst::ebirdst_runs]. Starting with the 2022 version year only
#'   a subset of the species were fit by eBird in each year.}
#' }
#' @source
#' Callaghan, Corey T., Shinichi Nakagawa, and William K. Cornwell. "Global
#'   abundance estimates for 9,700 bird species." Proceedings of the National
#'   Academy of Sciences 118.21 (2021): e2023170118.
#' <https://doi.org/10.1073/pnas.2023170118>
#'
"callaghan_abundance"



#' Return the population estimate for a Bird Flow model or species
#'
#' @param x Either a single BirdFlow model, or
#' one or more species eBird codes, scientific names, or common names.
#' @param what Indicates what information will be returned. Options are:
#' \describe{
#'   \item{`"df"`}{A data frame with relevant rows from [callaghan_abundance]}
#'   \item{`"abundance"`}{The abundance estimate.}
#'   \item{`"lower_ci"`}{The lower 95 percent confidence interval}
#'   \item{`"upper_ci"`}{The upper 95 percent confidence interval}
#' }
#' @return See `what`. In all cases the values correspond to `x` and will be
#' `NA` when `x` cannot be cross walked to [`callaghan_abundance`].
#' @references
#'  Callaghan, Corey T., Shinichi Nakagawa, and William K. Cornwell. "Global
#'   abundance estimates for 9,700 bird species." Proceedings of the National
#'   Academy of Sciences 118.21 (2021): e2023170118.
#'  <https://doi.org/10.1073/pnas.2023170118>
#'
#' @export
#' @examples
#' bf <- BirdFlowModels::amewoo
#' get_population(bf)
#' get_population("American Black Duck")
#' get_population("amewoo")
get_population <- function(x, what = "abundance") {
  stopifnot(what %in% c("abundance", "lower_ci", "upper_ci", "df"))

  if (inherits(x, "BirdFlow"))
    x <- species(x, what = "code")

  if (!inherits(x, "character"))
    stop("x should be a BirdFlow model or a character species name or code")

  p <- BirdFlowR::callaghan_abundance

  c1 <- p$species_code[match(x, p$common_name)]
  c2 <- p$species_code[match(x, p$scientific_name)]
  c3 <- p$species_code[match(x, p$species_code)]
  codes <- dplyr::coalesce(c1, c2, c3)

  # select the codes
  mv <- match(codes, p$species_code, incomparables = NA)
  p <- p[mv, , drop = FALSE]
  return(switch(what,
                df = p,
                abundance = p$abundance_estimate,
                lower_ci = p$lower_ci,
                upper_ci = p$upper_ci,
                stop("Unrecognized what argument.")))
}
