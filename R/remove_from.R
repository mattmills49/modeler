#' Removes values from a vector
#'
#' The \code{remove_} functions are convenient wrappers to help remove specific
#' values from a vector. \code{remove_from} will remove single or multiple
#' values from a given vector. In addition the \code{remove_null} and
#' \code{remove_na} functions will remove the corresponding missing values.
#'
#' @param vec the vector to remove values from
#' @param value the value to remove. The value can either be a single value or
#' a vector of values
#' @export
#' @return the original vector with the values removed
#'
#'

remove_from <- function(vec, value) vec[!(vec %in% value)]

#' @describeIn remove_from Removes null values
remove_null <- function(vec) vec[!is.null(vec)]

#' @describeIn remove_from Removes NA values
remove_na <- function(vec) vec[!is.na(vec)]
