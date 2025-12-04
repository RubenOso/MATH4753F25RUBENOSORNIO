#' Central Limit Theorem for Binomial distribution
#'
#' This function simulates \code{iter} samples of size \code{n} from a
#' \eqn{Binomial(n, p)} distribution, computes their sample means,
#' and displays the sampling distribution with a Normal overlay.
#'
#' @param n Integer; number of trials in the Binomial and sample size.
#' @param iter Integer; number of iterations (samples).
#' @param p Numeric; probability of success.
#' @param ... Additional graphical arguments passed to \code{hist()}.
#'
#' @return Invisibly returns the vector of sample means.
#' @examples
#' \dontrun{
#'   MATH4753F25RUBENOSORNIO::mycltb(n = 10, iter = 10000, p = 0.5)
#' }
#' @export
mycltb <- function(n, iter, p = 0.5, ...) {
  x <- NULL
  # Generate n * iter random Binomial(n,p) values
  y <- stats::rbinom(n * iter, size = n, prob = p)

  # Reshape into n × iter matrix; each column = one sample
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Apply mean across columns to get sample means
  w <- apply(data, 2, mean)

  # Determine histogram scaling
  param <- graphics::hist(w, plot = FALSE)
  ymax  <- 1.1 * max(param$density)

  # Plot histogram of sample means
  graphics::hist(
    w, freq = FALSE, ylim = c(0, ymax),
    main = paste("Histogram of Binomial sample means\nn =", n, ", iter =", iter, ", p =", p),
    xlab = "Sample mean", ...
  )

  # Theoretical Normal overlay for sample mean
  graphics::curve(
    stats::dnorm(x, mean = p, sd = sqrt(p * (1 - p) / n)),
    add = TRUE, col = "red", lty = 2, lwd = 3
  )

  invisible(w)
}
