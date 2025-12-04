#' Probability of at least one shared birthday among x people
#'
#' Computes 1 - P(no match) using a numerically stable log form:
#' P(no match) = choose(365, x) * x! / 365^x.
#'
#' @param x Integer vector of group sizes (x >= 0).
#' @return Numeric vector of probabilities between 0 and 1, same length as `x`.
#' @examples
#' Birthday(23)        # ~0.5073
#' Birthday(20:25)     # vectorized
#' @export
Birthday <- function(x) {
  x <- as.integer(x)
  stopifnot(all(x >= 0))

  # initialize P(no match) per element
  p_no <- rep_len(NA_real_, length(x))
  p_no[x <= 1] <- 1          # 0 or 1 person -> no match
  p_no[x > 365] <- 0         # >365 people -> match certain (no leap year)

  idx <- which(x > 1 & x <= 365)
  if (length(idx)) {
    xi <- x[idx]
    # P(no match) on log scale for stability
    p_no[idx] <- exp(lchoose(365, xi) + lfactorial(xi) - xi * log(365))
  }

  1 - p_no
}
