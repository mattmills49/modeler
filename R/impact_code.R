#' transform high cardinality categorical variables
#' 
#' \code{impact_code} transforms a categorical variable into a continuous one 
#' using Impact Coding described in this
#' \href{http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf}{paper}.
#' 
#' Missing values in the categorical variable will be treated as another level. 
#' If you want to ignore missing values you will need to do so prior to calling
#' \code{impact_code}.
#' 
#' @param .data the data frame the data is contained in
#' @param formula a formula specifying the dependent and independent variables to use
#' @param binary is the dependent variable a binary variable? Logical value
#' @return a data frame that includes the levels of the categorical variable and the smoothed estimate of the dependent variable. 
#' @export
#' @importFrom magrittr %>%
#' @examples 
#' impact_code(mtcars, am ~ cyl)
#' impact_code(iris, Petal.Width ~ Species, binary = F)

impact_code <- function(.data, formula, binary = T){
  stopifnot(is.data.frame(.data))
  y_var <- lazyeval::f_eval_lhs(formula, data = .data)
  stopifnot(all(!is.na(y_var)))
  
  x_names <- attr(terms(formula), "term.labels")
  x_vars <- dplyr::select_(.data, .dots = x_names)
  
  if(binary){
    stopifnot(length(unique(y_var)) == 2)
    codings <- smoother_binomial(y = y_var, x = x_vars, groups = x_names)
  } else codings <- smoother_gaussian(y = y_var, x = x_vars, groups = x_names)
  
  impact_info <- dplyr::select_(codings, .dots = c(x_names, "estimate"))
  
  return(impact_info)
}

#' helper for continuous variables

smoother_gaussian <- function(y, x, groups){
  impact_df <- dplyr::mutate(x, .y = y) %>%
    dplyr::mutate(y_var = var(.y), y_avg = mean(.y)) %>%
    dplyr::group_by_(.dots = c(groups, "y_var", "y_avg")) %>%
    dplyr::summarize(.count = n(), .avg = mean(.y), .var = var(.y)) %>%
    dplyr::mutate(.smooth = y_var / (.var / .count + y_var),
                  estimate = .smooth * .avg + (1 - .smooth) * .avg) %>%
    dplyr::ungroup()
  
  return(impact_df)
}

#' helper functions for smoother_binomial

smoother_binomial <- function(y, x, groups) UseMethod("smoother_binomial")

#' @describeIn smoother_binomial factors

smoother_binomial.factor <- function(y, x, groups) smoother_binomial.numeric(1 * (y == levels(y)[1]), x, groups)

#' @describeIn smoother_binomial logicals

smoother_binomial.logical <- function(y, x, groups) smoother_binomial.numeric(1 * y, x, groups)

#' @describeIn smoother_binomial characters

smoother_binomial.character <- function(y, x, groups) smoother_binomial.numeric(1 * (y == y[1]), x, groups)

#' @describeIn smoother_binomial numerics

smoother_binomial.numeric <- function(y, x, groups){
  impact_df <- dplyr::mutate(x, .y = y) %>% 
    dplyr::group_by_(.dots = c(groups)) %>%
    dplyr::summarize(.count = n(), .yes = sum(.y)) %>% 
    dplyr::mutate(.probs = .yes / .count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.u = mean(.probs), 
                  .v = var(.probs),
                  .a = ((1 - .u) / .v - 1 / .u) * .u * .u,
                  .b = .a * (1 / .u - 1),
                  estimate = (.yes + .a) / (.a + .b + .count))
  
  return(impact_df)
}