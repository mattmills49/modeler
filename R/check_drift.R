#' Visualize trend in variables across another
#' 
#' Returns a plot showing the distribution of a variable by different levels of
#' another grouping variable. A common application in my work is making sure the
#' distribution doesn't change across different seasons. 
#' 
#' @param .data a data frame containing the data in question
#' @param formula a formula with the variable on the lhs and grouping variable 
#' on the rhs
#' @param bins the number of bins to make. If \code{NULL} then each variable 
#' value will be used. 
#' 
#' @export 
#' @example 
#' check_drift(iris, Petal.Width ~ Species, bins = 5)
#' check_drift(mtcars, cyl ~ am, bins = NULL)


check_drift <- function(.data, formula, bins = 10){
  x_var <- lazyeval::f_lhs(formula)
  x_char <- as.character(x_var)
  x_values <- lazyeval::f_eval_lhs(formula, data = .data)
  
  drift_var <- lazyeval::f_rhs(formula)
  drift_char <- as.character(drift_var)
  drift_values <- lazyeval::f_eval_rhs(formula, data = .data)
  
  drift_check(x_values = x_values, x_name = x_char, drift_values = drift_values, drift_name = drift_char, .data = .data, bins = bins)
  
}

drift_check <- function(x_values, x_name, drift_values, drift_name, .data, bins) UseMethod("drift_check")

drift_check.numeric <- function(x_values, x_name, drift_values, drift_name, .data, bins){
  
  if(is.null(bins)){
    
    plot_data <- dplyr::data_frame(x_groups = x_values, drift = as.character(drift_values))
      
  } else {
    
    break_locs <- unique(quantile(x_values, probs = seq(0, 1, length.out = bins + 1)))
    
    plot_data <- dplyr::data_frame(x = x_values, drift = as.character(drift_values)) %>%
      dplyr::mutate(x_groups = cut(x, breaks = break_locs, include.lowest = T))
    
  }
  
  plot_data %>%
    dplyr::group_by(drift, x_groups) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::mutate(perc = n / sum(n)) %>%
    tidyr::complete(drift, x_groups) %>%
    ggplot2::ggplot(ggplot2::aes(x = x_groups, y = perc, fill = drift)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(), na.rm = T) +
    ggplot2::xlab(x_name) +
    ggplot2::ylab("Percentage of Observations") +
    ggplot2::ggtitle(stringr::str_c("Change in ", x_name, " by ", drift_name))
  
}

drift_check.numeric <- function(x_values, x_name, drift_values, drift_name, .data, bins){
  
  if(is.null(bins)){
    
    plot_data <- dplyr::data_frame(x_groups = x_values, drift = as.character(drift_values))
    
  } else {
    
    break_locs <- unique(quantile(x_values, probs = seq(0, 1, length.out = bins + 1)))
    
    plot_data <- dplyr::data_frame(x = x_values, drift = as.character(drift_values)) %>%
      dplyr::mutate(x_groups = cut(x, breaks = break_locs, include.lowest = T))
    
  }
  
  plot_data %>%
    dplyr::group_by(drift, x_groups) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::mutate(perc = n / sum(n)) %>%
    tidyr::complete(drift, x_groups) %>%
    ggplot2::ggplot(ggplot2::aes(x = x_groups, y = perc, fill = drift)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(), na.rm = T) +
    ggplot2::xlab(x_name) +
    ggplot2::ylab("Percentage of Observations") +
    ggplot2::ggtitle(stringr::str_c("Change in ", x_name, " by ", drift_name))
  
}

# .data %>%
#   dplyr::mutate_(.dots = setNames(list(~ as.character(drift_name)), drift_name)) %>%
#   dplyr::group_by_(.dots = c(drift_name, x_name)) %>%
#   dplyr::summarize(n = n()) %>%
#   dplyr::mutate(perc = n / sum(n)) %>%
#   tidyr::complete_(cols = c(drift_name, x_name)) %>%
#   ggplot2::ggplot(ggplot2::aes_string(x = x_name, y = "perc", fill = drift_name)) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(), na.rm = T) +
#   ggplot2::xlab(x_name) +
#   ggplot2::ylab("Percentage of Observations") +
#   ggplot2::ggtitle(stringr::str_c("Change in ", x_name, " by ", drift_name))
# 
# .data %>%
#   dplyr::mutate(num_breaks = cut(x_values, breaks = break_locs, include.lowest = T)) %>%
#   dplyr::group_by_(.dots = c(drift_name, "num_breaks")) %>%
#   dplyr::summarize(n = n()) %>% 
#   dplyr::mutate(perc = n / sum(n)) %>%
#   tidyr::complete_(cols = c(drift_name, "num_breaks")) %>%
#   ggplot2::ggplot(ggplot2::aes_string(x = "num_breaks", y = "perc", fill = drift_name)) +
#   ggplot2::geom_col(position = ggplot2::position_dodge(), na.rm = T) +
#   ggplot2::xlab(x_char) +
#   ggplot2::ylab("Percentage of Observations") +
#   ggplot2::ggtitle(stringr::str_c("Change in ", x_char, " by ", drift_name))