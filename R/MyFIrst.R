#' My First Function
#'
#' @param x A Numeric Vector.
#'
#' @returns A list with component x and y where y is the square of 'x'.
#' @export
#'
#' @examples
#' myfirstfunction(1:10)
myfirstfunction <- function(x) {
  y <- x^2
  plot(y - x)
  list(x = x, y = y)
}

