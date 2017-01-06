#' returns the min, deciles, and max for a numeric vector
#'
#' \code{deciles} takes in a numeric vector and returns a named vector that
#' contains the min, 10, 20, 30, 40, 50, 60, 70, 80, and 90th percentiles,
#' and the max. Missing values are always removed
#'
#' @param x a numeric or integer vector
#' @return a numeric vector with names for the corresponding decile
#' @export
#' @examples
#' deciles(0:100)

deciles <- function(x) {
  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(class(x) == "integer" || class(x) == "numeric")
  assertthat::assert_that(length(x) > 1)
  
  
  v <- c(min(x, na.rm = T), quantile(x, probs = seq(.1, .9, .1), na.rm = T), max(x, na.rm = T))
  names(v)[c(1,11)] <- c("min", "max")
  return(v)
}
