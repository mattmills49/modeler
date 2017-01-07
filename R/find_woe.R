#' finds weight of evidence for independent variables
#' 
#' Measures and returns the Weight of Evidence (WOE) and Information Value (IV)
#' for selected independent variables against a specified binary dependent 
#' variable. 
#' 
#' @param .data a data frame containing the variables to analyze
#' @param ... columns to calculate woe for. Uses \code{dplyr}'s \code{select} rules
#' @param y the dependent binary variable
#' @return a data frame where each row lists a columns Information Value and a 
#' nested data frame containing the splits used and the Weight of Evidence for
#' each. 
#' @export
#' @import tidyr
#' @examples 
#' 
#' find_woe(mtcars, mpg:qsec, y = am)

find_woe <- function(.data, ..., y){
  stopifnot(is.data.frame(.data))
  
  x_vars <- dplyr::select(.data, ...)
  y_name <- as.character(substitute(list(y))[-1])
  y_var <- .data[[y_name]]
  
  if(length(unique(.data[[y_name]])) != 2) stop("Y variable is not binary")
  if(any(is.na(.data[[y_name]]))) stop("y must not contain any missing values")
  
  info <- dplyr::bind_rows(purrr::map(x_vars, ~ woe(x = .x, y = y_var)), .id = "variable")
  return(info)
}
  
  
#' Find Weight Of Evidence
#' 
#' This function finds the WOE and IV of a variable in regards to its 
#' relationship with the dependent variable. If x is a factor it will be 
#' converted to a character vector. 
#' @param x Independent variable
#' @param y Must be Categorical or only have two values. Also can't contain any \code{NA} values
#' @param bins The number of buckets to bin the independent variable (\code{x})
#' @param adj A small adjustment should be included if some buckets may only have 1 level of \code{y}. Defaults to .5
#' @param incl_NA should \code{NA} observations be included? Default = TRUE
#' @return A list containing:
#' @return \code{InformationValue} The information value from the WOE
#' @return \code{Bins} The cutpoints for the bins
#' @return \code{Plot} ggplot object with the WOE of each bin. 
#' @keywords WOE, IV
#' @export 
#' @examples
#' result <- WOE(x = 1:100, y = sample(c(0, 1), 100, replace = T))

woe <- function(x, y) UseMethod("woe")

#' @describeIn woe
woe.numeric <- function(x, y){
  if(length(unique(x)) < 10){
    return(woe.factor(factor(x), y))
  } else{
    breaks <- unique(quantile(x, probs = seq(0, 1, length.out = 11), na.rm = T))
    cuts <- cut(x, breaks = breaks, include.lowest = T)
    if(any(is.na(x))) cuts <- addNA(cuts)
    bins <- levels(cuts)
  
    return(calc_woe(cuts, y, bins))
  }
}

#' @describeIn woe
woe.factor <- function(x, y){
  if(any(is.na(x))) x <- addNA(x)
  bins <- levels(x)
  
  return(calc_woe(x, y, bins))
}

#' @describeIn woe
woe.character <- function(x, y){
  return(woe.factor(factor(x)), y)
}

#' @describeIn find_woe
calc_woe <- function(x, y, bins){
  n_bins <- length(unique(bins))
  levels <- sort(unique(y))
  prob_dist <- prop.table(tableNA(x, y == levels[2]) + .5, 2)
  weight <- log(prob_dist[, 2] / prob_dist[, 1])
  iv <- sum((prob_dist[, 2] - prob_dist[, 1]) * weight, na.rm = T)

  iv_data <- dplyr::data_frame(iv = iv, bins = factor(bins, levels = levels(x)), woe = weight[bins]) %>%
    tidyr::nest(bins:woe)
  
  return(iv_data)
}