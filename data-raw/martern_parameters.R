# nolint start

# Note kl was originally l but that lead to warnings about partial argument
# matching ambiguity so I changed it to kl  for kernel length hyper parameter


# Define a function to plot the probability contour that contains 95% of
# the weight
plot_95 <- function(T, k, gamma, kl, add = TRUE) {
  # kl  <- 1/2 * T
  #  gamma <- 4 * T
  xs <- seq(0, T, length.out = 50)
  ys <- sqrt(calc_variance(xs, T, k = k, gamma = gamma, kl = kl)) * 1.96
  #  xs <- xs + 0.5 * T

  if (!add) {
    plot(NA, NA, xlim = range(xs), ylim = c(- max(ys), max(ys)),
         asp = 1, xlab = "", ylab = "")
  }
  polygon(x = c(xs, rev(xs)), y = c(ys, - rev(ys)))

}


# Set plot options
gamma <- 60000
kl <- 5000
k <- k_m3 # k_sq, k_m1, k_m3, k_m5


gamma <- 60000
kl <- 5000
lengths <- c(20000, 10000, 4000, 2000, 1000, 500, 200)

#
gamma <- 20000
kl <- 2000
lengths <- c(2000, 1000, 500, 200) # 20000, 10000, 4000,


# Plot
plot_95(lengths[1], k, gamma, kl, add = FALSE)
for (i in 2:length(lengths)) {
  plot_95(lengths[i], k, gamma, kl)
}

abline(v = seq(0, lengths[1], 100), col = rgb(0, 0, 0, 0.25))
abline(h = seq(-lengths[1], lengths[1], 100), col = rgb(0, 0, 0, 0.25))

# nolint end
