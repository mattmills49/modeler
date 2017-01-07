#' transform high cardinality categorical variables
#' 
#' \code{impact_code} transforms a categorical variable into a continuous one
#' using Impact Coding described in this \href{http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf}{paper.}
#' 
#' @param .data the data frame the data is contained in
#' @param formula a formula specifying the dependent and independent variables to use
#' @param binary is the dependent variable a binary variable? Logical value
#' @export
#' @import magrittr
#' 
#' 

impact_code <- function(.data, formula, binary = T){
  stopifnot(is.data.frame(.data))
  y_var <- lazyeval::f_eval_lhs(formula, data = .data)
  stopifnot(all(!is.na(y_var)))
  
  x_names <- attr(terms(formula), "term.labels")
  x_vars <- dplyr::select_(.data, .dots = x_names)
  
  if(binary){
    codings <- smoother_binomial(x = x_vars, y = y_var, groups = x_names)
  } else codings <- smoother_gaussian(x = x_vars, y = y_var, groups = x_names)
  
  
  return(dplyr::select_(codings, .dots = c(x_names, "estimate")))
}

#' 

smoother_gaussian <- function(x, y, groups){
  impact_df <- dplyr::mutate(x, .y = y) %>%
    dplyr::mutate(y_var = var(.y), y_avg = mean(.y)) %>%
    dplyr::group_by_(.dots = c(groups, "y_var", "y_avg")) %>%
    dplyr::summarize(.count = n(), .avg = mean(.y), .var = var(.y)) %>%
    dplyr::mutate(.smooth = y_var / (.var / .count + y_var),
                  estimate = .smooth * .avg + (1 - .smooth) * .avg)
  
  return(impact_df)
}