#' sample from groups of variables
#'
#' \code{sample_groups} allows you to restrict the data to only a limited number
#' of groupings of variables. This is useful for when you have multilevel data
#' and would like to sample from multiple levels at once. This makes exploratory
#' plotting a little easier.
#'
#' @param df the data.frame you are sampling from
#' @param ... the variables you would like to group
#' @param n the number of groupings to return
#' @return a data frame containing the sampled groupings
#' @export
#' @import magrittr
#' @examples
#' sample_groups(mtcars, cyl, am)

sample_groups <- function(.data, ..., n = 6){
  stopifnot(is.data.frame(.data))
  
  merge_cols <- as.character(substitute(list(...)))[-1]
  
  distinct_values <- dplyr::distinct(.data, ...) %>%
    dplyr::sample_n(size = n) %>%
    dplyr::inner_join(.data, ., by = merge_cols)
  
  return(distinct_values)
}