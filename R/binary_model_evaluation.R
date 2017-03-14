#' Calculate Binary Model Performance Metrics
#' 
#' Accepts a data frame containing the model predictions and actual values. The
#' following classification variables are calculated on the predictions:
#' 
#' \itemize{
#'   \item AUC - Area Under the Receiver Operating Curve 
#'   \item KS - Kolmogorov-Smirnov statistic
#'   \item MSE - Mean Square Error
#'   \item TPR - True Positive Rate
#'   \item TNR - True Negative Rate
#'   \item LSR - Logistic Scoring Rule (Log Loss)
#'   \item Bias - Direction of Bias
#' }
#' 
#' @param .data a data frame
#' @param prediction_formula a formula specifiying the dependent binary variable
#'   (lhs) and the probability predictions (rhs)
#' @param group_var an unquoted variable name to calculate performance measures
#'   on each subset of the data - e.g., training/testing splits
#' @return a data frame containing the metrics
#' @export
#' @examples
#' iris_ <- iris
#' iris_$setosa <- ifelse(iris_$Species == "setosa", 1, 0)
#' iris_$pred <- iris_$Sepal.Length
#' iris_$pred <- (max(iris_$pred) - iris_$pred) / (max(iris_$pred) - min(iris_$pred) + 1)
#' iris_$splits <- sample(c("Test", "Train"), size = nrow(iris_), replace = T)
#' binary_model_evaluation(iris_, setosa ~ pred)
#' binary_model_evaluation(iris_, setosa ~ pred, group_var = splits)

binary_model_evaluation <- function(.data, prediction_formula, group_var = NULL){
  group_var <- match.call()$group_var
  stopifnot(is.data.frame(.data))
  # check for no NAs
  # check for prob < 1 > 0
  # check for y = numeric
  dependent_name <- lazyeval::f_lhs(prediction_formula)
  pred_name <- lazyeval::f_rhs(prediction_formula)
  
  if(is.null(group_var)){
    .data$data <- "results"
    group_var <- quote(data)
  }
  
  auc_value <- .data %>%
    group_by_(group_var) %>%
    arrange_(.dots = lazyeval::interp(~ desc(preds), preds = pred_name)) %>%
    mutate_(.dots = list(TPR = lazyeval::interp(~ cumsum(actual) / sum(actual), actual = dependent_name),
                         FPR = lazyeval::interp(~ cumsum(1 - actual) / sum(1 - actual), actual = dependent_name))) %>%
    summarise(AUC = sum(diff(FPR) * na.omit(dplyr::lead(TPR) + TPR)) / 2,
              KS = max(TPR - FPR))
  
  acc_value <- .data %>%
    group_by_(group_var) %>%
    summarize_(.dots = list(MSE = lazyeval::interp(~ mean((preds - actual)^2), preds = pred_name, actual = dependent_name),
                         TPR = lazyeval::interp(~ mean(preds > .5 & actual == 1), preds = pred_name, actual = dependent_name),
                         TNR = lazyeval::interp(~ mean(preds <= .5 & actual == 0), preds = pred_name, actual = dependent_name),
                         LSR = lazyeval::interp(~ mean(actual * log(preds) + (1 - actual) * log(1 - preds)), preds = pred_name, actual = dependent_name)))
  
  n_breaks <- (nrow(.data) > 2000) * nrow(.data) / 100 + (nrow(.data) <= 2000) * nrow(.data) / 20
  
  cal_value <- .data %>%
    group_by_(group_var) %>%
    mutate_(.dots = list(pred_group = lazyeval::interp(~ cut(preds, breaks = floor(n_breaks), include.lowest = T), preds = pred_name, n_breaks = n_breaks))) %>%
    group_by_(.dots = list(group_var, as.symbol("pred_group"))) %>%
    summarize_(.dots = list(mean_pred = lazyeval::interp(~ mean(preds), preds = pred_name),
                            mean_actual = lazyeval::interp(~ mean(actual), actual = dependent_name))) %>%
    summarize(Bias = mean(mean_pred - mean_actual))
  
  dplyr::inner_join(auc_value, acc_value, by = as.character(group_var)) %>%
    dplyr::inner_join(cal_value, by = as.character(group_var))

}
