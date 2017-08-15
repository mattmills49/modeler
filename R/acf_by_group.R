#' Calculate Auto Correlations by Group
#' 
#' \code{acf_by_group} takes a data frame with a grouping column and returns a
#' data frame with the autocorrelation values for each group. 
#' 
#' Each row in the resulting data frame will refer to a group-lag
#' auto-correlation value. 
#' 
#' @param .data the data frame the data is contained in
#' @param .group_var the unquoted variable to be used as the grouping variable
#' @param .value_var the name of the time series value to use
#' @param ... extra parameters to pass to the \code{acf} function (see \code{?stats::acf})
#' @return a data frame with the acf values by group and lag
#' 
#' @export
#' @examples
#' pres_ratings <- data.frame(approval = presidents, pre_1965 = c(rep(1, 60), rep(0, 60)))
#' ratings_acf <- acf_by_group(pres_ratings[!is.na(pres_ratings$approval), ], pre_1965, approval)
#' ## pass params to acf
#' ratings2_acf <- acf_by_group(pres_ratings, pre_1965, approval, na.action = na.pass)

acf_by_group <- function(.data, .group_var, .value_var, ...){
  group_var <- dplyr::enquo(.group_var)
  value_var <- deparse(substitute(.value_var))
  nest_cols <- names(.data)
  group_col <- deparse(substitute(.group_var))
  nest_cols <- nest_cols[nest_cols != group_col]

  .data %>%
    tidyr::nest_(key_col = "data", nest_cols = nest_cols) %>%
    dplyr::mutate(acf_results = purrr::map(data, function(x) acf(x[[value_var]], plot = F, ...)),
                  acf_values = purrr::map(acf_results, ~ drop(.x$acf))) %>%
    tidyr::unnest(acf_values) %>%
    dplyr::group_by(!!group_var) %>%
    dplyr::mutate(lag = seq(0, n() - 1)) %>%
    dplyr::ungroup()
}