#' transform high cardinality categorical variables
#' 
#' \code{impact_code} transforms a categorical variable into a continuous one
#' using Impact Coding described in this \href{http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf}{paper.}
#' 
#' @param .data the data frame the data is contained in
#' @param ... the variable to transform
#' @param y the dependent variable
#' @param numeric logical is the dependent variable a non-binary continous variable
#' @export
#' @import magrittr
#' 
#' 

impact_code <- function(.data, ..., y, numeric = F){
  stopifnot(is.data.frame(.data))
  y_name <- as.character(substitute(list(y)))[-1]
  stopifnot(all(!is.na(.data[[y_name]])))
  
  x_vars <- dplyr::select(.data, ...)
  
  if(numeric){
    codings <- smoother_gaussian(x = x_vars, y = .data[[y_name]])
  } else codings <- smoother_binomial(x = x_vars, y = .data[[y_name]])
  
  return(codings)
}

#' 

smoother_gaussian <- function(x, y){
  group_names <- sort(unique(x))
  group_counts <- vapply(group_names, function(gr) sum(x == gr), numeric(1))
  
  group_probs <- vapply(group_names, function(gr) mean(y[x == gr]), numeric(1))
  
  y_var <- var(y)
  y_prob <- mean(y)
  
  group_var <- vapply(group_names, function(gr) var(y[x == gr]), numeric(1))
  smoother <- rep(0, length(group_names))
  names(smoother) <- group_names
  for(gr in group_names){
    smoother[gr] <- y_var / (group_var[gr] / group_counts[gr] +  y_var)
  }
  posterior <- smoother * group_probs + (1 - smoother) * y_prob
}