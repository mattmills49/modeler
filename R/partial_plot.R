#' provides partial plots for fitted \code{gam} models
#' 
#' \code{partial_plot} accepts a fitted \code{gam} object and the name of the
#' variable you wish to view the partial regression plot of as a character
#' string. It returns a \code{ggplot} object with the values of the
#' independent variable plotted against the spline predictions plus the 
#' intercept of the model. 
#' 
#' @param fitted_model a complete \code{gam} model object
#' @param variable the name of the independent variable as a character string
#' @param response logical indicating if the plot should be on the linear 
#' prediction scale or the response scale. Defaults to \code{FALSE}
#' @param se logical indicating if standard errors should be plotted
#' @return a \code{ggplot2} object of the partial regression plot
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2
#' @examples
#' library(mgcv)
#' car_gam <- gam(mpg ~ s(hp), data = mtcars)
#' partial_plot(car_gam, "hp")

partial_plot <- function(fitted_model, variable, response = F, se = F) {
  
  stopifnot("gam" %in% class(fitted_model))
  
  pred_matrix <- predict(fitted_model, type="lpmatrix")
  variable_values <- fitted_model$model[[variable]]
  model_coefs <- coef(fitted_model)
  
  coefficient_terms <- stringr::str_replace_all(stringr::str_extract_all(names(model_coefs), "\\([^)]*\\)"), c("\\(" = "", "\\)" = ""))
  variable_coefs <- model_coefs[coefficient_terms == variable]
  variable_linpred <- pred_matrix[, coefficient_terms == variable]
  
  smooth_predictions <- variable_linpred %*% variable_coefs %>% drop
  plot_values <- dplyr::data_frame(variable = variable_values, smooth_curve = smooth_predictions) %>% dplyr::distinct()
  if (response) {
    if (fitted_model$family$family == "gaussian") {
      plot_values$predictions <- plot_values$smooth_curve + model_coefs["(Intercept)"]
    } else if (fitted_model$family$family == "binomial") {
      plot_values$predictions <- 1 / (1 + exp(-(plot_values$smooth_curve + model_coefs["(Intercept)"])))
    }
  } else {
    plot_values$predictions <- plot_values$smooth_curve + model_coefs["(Intercept)"]
  }
  
  yname <- attr(attr(fitted_model$pterms, "dataClasses"), "names")
  ylabel <- stringr::str_c("Prediction for ", yname)
  if (response) {
    ylabel <- stringr::str_c(ylabel, "\n(Response Scale)")
  } else {
    ylabel <- stringr::str_c(ylabel, "\n(Linear Prediction Scale)")
  }
  
  partial_p <- ggplot() + 
    geom_line(aes(x = variable, y = predictions), data = plot_values, color = "blue", size = 2) + 
    ylab(ylabel) +
    xlab(variable) +
    ggtitle(stringr::str_c("Partial Regression Plot for ", variable))
  
  if(se){
    type_var <- ifelse(response, "response", "link")
    standard_errors <- dplyr::as_data_frame(predict(fitted_model, type = type_var, se.fit = T)) %>% dplyr::mutate(x = variable_values) %>% dplyr::distinct()
    partial_p <- partial_p + geom_ribbon(aes(x = x, ymax = fit + 2 * se.fit, ymin = fit - 2 * se.fit), data = standard_errors, fill = "grey80", alpha = .7)
    partial_p$layers <- list(partial_p$layers[[2]], partial_p$layers[[1]])
    
  }
  
  return(partial_p)
}