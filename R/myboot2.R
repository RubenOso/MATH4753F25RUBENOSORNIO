#' Bootstrap confidence intervals for a statistic
#'
#' This function performs a nonparametric bootstrap on a numeric vector `x`
#' to estimate the sampling distribution of a statistic (e.g. the mean),
#' and returns a bootstrap confidence interval along with a histogram.
#'
#' @param iter Integer. Number of bootstrap replications. Default is 10000.
#' @param x Numeric vector of data values.
#' @param fun Character string giving the name of the function to apply
#'   to each bootstrap sample. Default is "mean".
#' @param alpha Significance level for the confidence interval.
#'   Default is 0.05 (for a 95\% CI).
#' @param cx Numeric. Character expansion factor for text on the plot.
#'   Default is 1.5.
#' @param ... Additional graphical arguments passed to \code{\link{hist}}.
#'
#' @details
#' The data vector \code{x} is resampled with replacement \code{iter} times.
#' The statistic specified by \code{fun} is computed for each resample,
#' forming an empirical bootstrap distribution. A two-sided
#' \code{(1 - alpha)} confidence interval is constructed using the
#' corresponding quantiles of this distribution.
#'
#' A histogram of the bootstrap statistics is also plotted, showing
#' the bootstrap distribution of the statistic, the point estimate from
#' the original data, and the bootstrap confidence interval.
#'
#' @return
#' An invisible list with components:
#' \code{ci} (numeric vector of length 2 with the lower and upper
#' confidence limits), \code{fun} (the name of the statistic used),
#' and \code{x} (the original data vector).
#'
#' @examples
#' \dontrun{
#'   set.seed(39)
#'   x <- rnorm(25, mean = 25, sd = 10)
#'   myboot2(iter = 5000, x = x, fun = "mean", alpha = 0.05,
#'           xlab = "mean", col = "lightblue")
#' }
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)

  # Resample with replacement
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)

  # Bootstrap statistics
  xstat <- apply(rs.mat, 2, fun)

  # Confidence interval from quantiles
  ci <- stats::quantile(xstat, c(alpha/2, 1 - alpha/2))

  # Histogram of bootstrap statistics
  para <- graphics::hist(
    xstat,
    freq = FALSE,
    las = 1,
    main = paste(
      "Histogram of Bootstrap sample statistics", "\n",
      "alpha=", alpha, " iter=", iter, sep = ""
    ),
    ...
  )

  # Point estimate from original data
  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  graphics::abline(v = pte, lwd = 3, col = "black")
  graphics::segments(ci[1], 0, ci[2], 0, lwd = 4)
  graphics::text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "red", cex = cx)
  graphics::text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "red", cex = cx)
  graphics::text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x))
}


