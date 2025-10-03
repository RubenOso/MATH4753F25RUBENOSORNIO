#' Title birthday function
#'
#' @param x x as a vector
#'
#' @returns returns
#' @export
#'
#' @examples
#' Birthday(20:24)
#'
Birthday = function(x){
 1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
