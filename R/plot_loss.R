
#' Get loss values for each step in the model fitting process
#'
#' `get_loss()` returns a data frame of loss values. Each row corresponds to
#' a step in the fitting process.
#' @param bf A BirdFlow object.
#'
#' @return A data frame with columns:
#' \item{dist}{The distance loss}
#' \item{ent}{The entropy loss}
#' \item{obs}{The observation loss}
#' \item{total}{The total weighted loss}
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' get_loss(bf)
#' @seealso [plot_loss()]
get_loss <- function(bf) {
  if (!"loss_values" %in% names(bf$metadata))
    stop("Missing loss values in the BirdFlow metadata. ",
         "Is this a fitted model?")
  bf$metadata$loss_values
}




#' Plot changes in component and total loss during model fitting
#'
#' Model fitting  - in [BirdFlowPy]() - attempts to minimize the total weighted
#' loss.  This plot shows four lines:
#' *  **Total loss** is the weighted sum of the three loss components. The
#' weighting may cause it to be lower than some of the components.
#' * **Observation loss** captures how well the model predicts the Status and
#' Trend distributions it was trained on.  Its weight is always set to 1 and its
#' relative weight is changed by adjusting the other to weights which are
#' usually much less than 1.
#' * **Distance loss** is lower when the routes encoded in the model are
#' shorter.
#' * **Entropy loss** is lower when the entropy in the model is higher.
#'
#'
#' @param bf A fitted Bird Flow model
#' @param transform Passed to [ggplot2::scale_y_continuous()] to set the y-axis
#' transformation. Reasonable values for this function include
#' "identity", "log", "log10", "log2", and "sqrt".
#' @return a **ggplot2** plot object.
#' @export
#' @examples
#'  bf <- BirdFlowModels::amewoo
#'  plot_loss(bf)
plot_loss <- function(bf, transform = "log10") {

  if (!all(c("loss_values", "hyperparameters") %in% names(bf$metadata)))
    stop("Missing loss or hyperparmers from metadata. Is this a fitted model?")
  loss <- get_loss(bf) |>
    dplyr::rename(Distance = "dist",
                  Entropy = "ent",
                  Observation = "obs",
                  Total = "total")
  loss$Step <- seq_len(nrow(loss))


  # Convert to long format
  d <- tidyr::pivot_longer(loss, cols = -ncol(loss), values_to = "Loss",
                           names_to = "Type")

  hp <-  bf$metadata$hyperparameters

  subtitle <- paste0("Weights: Observation:", hp$obs_weight,
                     " Distance: ", hp$dist_weight,
                     " Entropy: ", hp$ent_weight)

  ggplot2::ggplot(data = d,
                  ggplot2::aes(x = .data$Step,
                               y = .data$Loss,
                               color = .data$Type)) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::scale_y_continuous(transform = "log10") +
    ggplot2::ggtitle(paste0(species(bf), " Loss"), subtitle = subtitle)


}
