#' Calculate Auto Correlations by Group
#' 
#' \code{acf_by_group} takes a data frame with a grouping column and returns a
#' data frame with the autocorrelation values for each group. 
#' 
#' Each row in the resulting data frame will refer to a group-lag
#' auto-correlation value. 
#' 
#' \code{white_noise} will return confidence intervals for the series
#' assuming the autocorrelations come from a white noise distribution. \code{ma} 
#' will apply the correction needed if the autocorrelations have some moving
#' average correlation. 
#' 
#' @param .data the data frame the data is contained in
#' @param .group_var the unquoted variable to be used as the grouping variable
#' @param .value_var the name of the time series value to use
#' @param ... extra parameters to pass to the \code{acf} function (see \code{?stats::acf})
#' @param .ci the type of confidence interval to use, defaults to \code{none}
#' @param .cl confidence level for the confidence intervals, defaults to .95
#' @return a data frame with the acf values by group and lag
#' 
#' @export
#' @examples
#' pres_ratings <- data.frame(approval = as.numeric(presidents), pre_1965 = c(rep(1, 60), rep(0, 60)))
#' ratings_acf <- acf_by_group(pres_ratings[!is.na(pres_ratings$approval), ], pre_1965, approval)
#' ## pass params to acf
#' ratings2_acf <- acf_by_group(pres_ratings, pre_1965, approval, na.action = na.pass)

acf_by_group <- function(.data, .group_var, .value_var, ..., .ci = c("none", "white_noise", "ma"), .cl = .95){
  group_var <- dplyr::enquo(.group_var)
  value_var <- deparse(substitute(.value_var))
  nest_cols <- names(.data)
  group_col <- deparse(substitute(.group_var))
  nest_cols <- nest_cols[nest_cols != group_col]
  
  ci <- match.arg(.ci)
  
  acf_results <- .data %>%
    tidyr::nest_(key_col = "data", nest_cols = nest_cols) %>%
    dplyr::mutate(acf_results = purrr::map(data, function(x) acf(x[[value_var]], plot = F, ...)),
                  acf_values = purrr::map(acf_results, ~ drop(.x$acf)))
  
  if(ci == "white_noise"){
    acf_results <- dplyr::mutate(acf_results, se = purrr::map_dbl(data, ~ qnorm((1 + .cl) / 2) / nrow(.x)))
  } else if(ci == "ma"){
    ma_results <- acf_results %>%
      dplyr::mutate(se = purrr::map2(data, acf_results, ~ cumsum(c(1, 2 * .y$acf[-1, 1, 1]^2)) * qnorm((1 + .cl) / 2) / nrow(.x))) %>%
      tidyr::unnest(acf_values, se) %>%
      dplyr::group_by(!!group_var) %>%
      dplyr::mutate(lag = seq(0, n() - 1)) %>%
      dplyr::ungroup()
    
    return(ma_results)
  }
  
  acf_results %>%
    tidyr::unnest(acf_values) %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::mutate(lag = seq(0, n() - 1)) %>%
    dplyr::ungroup()
}