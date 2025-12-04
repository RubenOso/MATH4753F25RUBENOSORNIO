#' Overbooking: optimal number of tickets to sell
#'
#' Computes the number of tickets to sell for a flight with N seats, show-up
#' probability p, and allowed overbooking probability gamma.
#' Returns both discrete (binomial) and normal-approximation solutions,
#' and produces two plots showing the objective function vs n.
#'
#' @param N Integer > 0, number of seats.
#' @param gamma Numeric in (0,1), allowed probability of true overbooking P(X>N).
#' @param p Numeric in (0,1), show-up probability per sold ticket.
#' @param n_range Optional integer vector for the range of n values to search.
#' @param show_plots Logical; whether to display the plots (default TRUE).
#'
#' @return A named list containing nd, nc, N, p, and gamma.
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#'
#' @export
ntickets <- function(N, gamma, p, n_range = NULL, show_plots = TRUE) {
  stopifnot(N > 0, 0 < p & p < 1, 0 < gamma & gamma < 1)

  # Objective per slides: 1 - gamma - F(N)
  obj_disc <- function(n, N, p, gamma) {
    1 - gamma - stats::pbinom(q = N, size = n, prob = p)
  }

  obj_cont <- function(n, N, p, gamma) {
    mu  <- n * p
    sig <- sqrt(n * p * (1 - p))
    z   <- (N + 0.5 - mu) / sig
    1 - gamma - stats::pnorm(z)
  }

  # default window near the crossing
  if (is.null(n_range)) {
    n_range <- seq(N - 5, N + 30)
  }

  # discrete solution = smallest n with f(n) >= 0
  f_vals <- obj_disc(n_range, N, p, gamma)
  feas   <- n_range[f_vals >= 0]
  if (length(feas) == 0) stop("No feasible n found. Expand n_range.")
  nd <- min(feas)

  # continuous solution = root of f_c(n) = 0
  nc <- uniroot(function(n) obj_cont(n, N, p, gamma),
                lower = min(n_range), upper = max(n_range))$root

  if (show_plots) {
    op <- graphics::par(mfrow = c(2, 1), mar = c(4, 5, 4, 2) + 0.1)

    # --- Discrete ---
    graphics::plot(n_range, f_vals, pch = 16, cex = 0.6,
                   xlab = "n", ylab = "Objective",
                   main = sprintf("Objective Vs n to find optimal tickets sold\n(%d) gamma= %.02f N=%d discrete",
                                  nd, gamma, N))
    graphics::lines(n_range, f_vals, lty = 2)
    graphics::abline(h = 0, col = "red", lwd = 2)
    graphics::abline(v = nd, col = "red", lwd = 2)

    # --- Continuous ---
    n_grid <- seq(min(n_range), max(n_range), length.out = 800)
    f_c    <- sapply(n_grid, obj_cont, N = N, p = p, gamma = gamma)
    graphics::plot(n_grid, f_c, type = "l",
                   xlab = "n", ylab = "Objective",
                   main = sprintf("Objective Vs n to find optimal tickets sold\n(%.10f) gamma= %.02f N=%d continuous",
                                  nc, gamma, N))
    graphics::abline(h = 0, col = "blue", lwd = 2)
    graphics::abline(v = nc, col = "blue", lwd = 2)

    graphics::par(op)
  }

  out <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(out); invisible(out)
}
