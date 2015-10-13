#' Binary Model Validation
#' 
#' This function finds the KS Statistic, AUC, and RMSE value for a given 
#' predictor and binary response. In addition it returns the AUC, 
#' Calibration, KS, and Density plots for the predictor and response. 
#' @param preds a vector of values between 0 and 1 containing the predictions
#' @param Y the binary response variable. 
#' @param print_summary should the KS, AUC, and RMSE be printed on the console, logical value.
#' @keywords eda
#' @return A list containing the AUC plot, Calibration plot, KS plot, Distribution plot, a plot containing all plots on one page, and a data frame containing the validation measures. 
#' @importFrom magrittr %>%
#' @export
#' @examples 
#' set.seed(924516)
#' predictions <- runif(1000)
#' y_val <- 1 * (runif(1000) <= predictions + rnorm(1000, mean = 0, sd = .5))
#' val_plots <- BinaryModelValidation(preds = predictions, Y = y_val, print_summary = F)

BinaryModelValidation <- function(preds, Y, print_summary = T){
  vals <- dplyr::data_frame(Predictions = preds, Actual = Y)
  
  vals$Groups <- cut(x = vals$Predictions, breaks = seq(0, 1, .05), include.lowest = T)
  cal_data <- vals %>% dplyr::group_by(Groups) %>% dplyr::summarize(Mean_Prediction = mean(Predictions), Mean_Actual = mean(Actual))
  cal_plot <- ggplot2::ggplot(ggplot2::aes(x = Mean_Prediction, y = Mean_Actual), data = cal_data) + ggplot2::geom_line(size = 2, color = "blue") + ggplot2::geom_abline(a = 1, b = 0, linetype = "dashed") + ggplot2::ggtitle("Calibration Plot") + ggplot2::xlab("Predicted Probability") + ggplot2::ylab("Actual Probability") + ggplot2::scale_y_continuous(labels = function(x) paste0(x*100, "%")) + ggplot2::scale_x_continuous(labels = function(x) paste0(x*100, "%"))
  
  roc_info <- pROC::roc(response = Y, predictor = preds)
  roc_data <- dplyr::data_frame(Sensitivity = roc_info$sensitivities, Specificity = roc_info$specificities)
  auc_plot <- ggplot2::ggplot(ggplot2::aes(x = 1 - Sensitivity, y = Specificity), data = roc_data) + ggplot2::geom_line(size = 2, color = "blue") + ggplot2::geom_abline(a = 1, b = 0, linetype = "dashed", color = "grey") + ggplot2::annotate("text", x = .75, y = .25, label = paste0("AUC = ", round(roc_info$auc[1], 2))) + ggplot2::ggtitle("AUC Plot")
  
  values <- preds %>% round(3) %>% table %>% rev %>% names %>% as.numeric
  pct_pos <- sapply(values,function(x) sum(Y[preds >= x] == 1)/(sum(Y == 1)))
  pct_neg <- sapply(values,function(x) sum(Y[preds >= x] == 0)/(sum(Y == 0)))
  nums <- preds %>% round(3) %>% table %>% rev %>% cumsum
  info <- data.frame(Values = values,
                     Number_of_Values = nums/max(nums),
                     Pct_of_Yes = pct_pos,
                     Pct_of_No = pct_neg)
  k_s <- max(info$Pct_of_Yes - info$Pct_of_No)*100
  where <- which.max(info$Pct_of_Yes - info$Pct_of_No)
  locsdata <- with(info, dplyr::data_frame(a = Number_of_Values[where], b = Pct_of_Yes[where], c = Pct_of_No[where]))
  
  ks_plot <- ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = Number_of_Values, y = Pct_of_Yes), data = info, color = "blue", size = 2) + ggplot2::geom_line(ggplot2::aes(x = Number_of_Values, y = Pct_of_No), data = info, color = "red", size = 2) + ggplot2::geom_segment(ggplot2::aes(x = a, xend = a, y = b, yend = c), linetype = "dashed", data = locsdata) + ggplot2::xlab("Percentage of Population") + ggplot2::ylab("Percentage of Yes") + ggplot2::ggtitle("Gain Plot") + ggplot2::scale_x_continuous(labels = function(x) paste0(x*100, "%"), breaks = seq(0, 1, .1)) + ggplot2::scale_y_continuous(labels = function(x) paste0(x*100, "%")) + ggplot2::geom_text(ggplot2::aes(x = a, y = b/2 + c/2), data = locsdata, label = paste0("K-S = ", round(k_s,2)))
  
  dist_plot <- ggplot2::ggplot(ggplot2::aes(x = Predictions), data = vals) + ggplot2::geom_density(ggplot2::aes(fill = as.factor(Actual)), alpha = .8) + ggplot2::xlab("Predictied No Show Probability") + ggplot2::ggtitle("Distribution of Scores") + ggplot2::theme(legend.position = "top") + ggplot2::scale_fill_discrete(name = "No Show")
  
  scores <- dplyr::data_frame(Metric = c("AUC", "RMSE", "K-S"), Value = round(c(roc_info$auc, sqrt(mean((vals$Predictions - vals$Actual)^2)), k_s),3))
  if(print_summary) print(scores)
  all_plot <- gridExtra::grid.arrange(cal_plot, gridExtra::arrangeGrob(auc_plot, ks_plot, nrow = 1), gridExtra::arrangeGrob(dist_plot, gridExtra::tableGrob(scores), nrow = 1))
  return(list(auc_plot, cal_plot, ks_plot, dist_plot, all_plot, scores))
}