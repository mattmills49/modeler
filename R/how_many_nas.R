#' Find out how many missing values are in each column of a data frame
#'
#' \code{how_many_nas} finds the number of missing values in the columns of a
#' data frame. You can also have it return the percentage of missing values
#' instead of the raw totals.
#'
#' @param x a data frame to examine
#' @param p a logical flag indicating if the results should be in percentages.
#' Defaults to \code{FALSE}
#' @return a named vector with the column names as the names and the number
#' or percentage of missing values as the values
#' @export
#'
#' @examples
#' set.seed(12345)
#' how_many_nas(mtcars)
#' how_many_nas(iris, p = T)
#'

how_many_nas <- function(x, p = F){
  if("data.frame" %in% class(x)){
    if(p){
      vapply(x, function(y) sum(is.na(y)), numeric(1)) / nrow(x)
    } else vapply(x, function(y) sum(is.na(y)), numeric(1))
  } else stop("x is not a data.frame")
}
