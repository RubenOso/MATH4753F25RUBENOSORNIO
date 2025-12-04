#' Draw N(mu, sigma^2), shade (-Inf, a], and return P(X <= a)
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation (> 0).
#' @param a Upper bound for the shaded area.
#' @param col Fill color for the shaded region. Default "red".
#' @param xlab X-axis label. Default "x".
#' @param ylab Y-axis label. Default "density".
#' @return A named list with elements `prob`, `mu`, `sigma`, and `a`.
#' @export
myncurve <- function(mu, sigma, a, col = "red", xlab = "x", ylab = "density") {
  stopifnot(is.numeric(mu), is.numeric(sigma), sigma > 0, is.numeric(a))

  # base density to plot
  xlim <- c(mu - 4 * sigma, mu + 4 * sigma)
  xs   <- seq(xlim[1], xlim[2], length.out = 512)
  ys   <- stats::dnorm(xs, mean = mu, sd = sigma)

  graphics::plot(xs, ys, type = "l", xlab = xlab, ylab = ylab)

  # shade (-Inf, a] within plotting window
  left  <- xlim[1]
  right <- min(a, xlim[2])
  if (right > left) {
    shade_x <- seq(left, right, length.out = 512)
    shade_y <- stats::dnorm(shade_x, mean = mu, sd = sigma)
    graphics::polygon(c(left, shade_x, right), c(0, shade_y, 0),
                      col = col, border = NA)
  }

  # probability and annotation
  p <- stats::pnorm(a, mean = mu, sd = sigma)
  graphics::text(x = mu,
                 y = stats::dnorm(mu, mean = mu, sd = sigma) * 0.85,
                 labels = paste0("P(X <= ", a, ") = ", round(p, 4)))

  invisible(list(prob = p, mu = mu, sigma = sigma, a = a))
}

