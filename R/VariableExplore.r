#' Variable Exploration
#' 
#' This function produces a histogram of the independent variable, 
#' summary statics, missing data information, and estimates of the trend
#' between it and the dependent variable. 
#' @param x independent variable, must be a vector
#' @param y dependent variable, must be a vector and can't contain missing values
#' @param var_name The name of the dependent variable
#' @param plot_save logical indicating if plot should be saved, default is FALSE
#' @param fname the name of the file to save the pdf output
#' @keywords eda
#' @return A list containing a plot showing all the info as well as the information that makes it up
#' @export
#' @examples VariableExplore(1:100, sample(c(0, 1), 100, replace = T))
#' @importFrom magrittr "%>%"
#' 

VariableExplore <- function(x, y, var_name, plot_save = F, fname = "none"){
  if(!is.vector(x)) stop("x must be a vector")
  if(!is.vector(y)) stop("y must be a vector")
  if(length(x) != length(y)) stop("x and y are not the same length")
  if(any(is.na(y))) stop("y must not contain any missing values. ")
  if(length(unique(y)) == 1) stop("y only contains one unique value")
  missing_log <- is.na(x)
  num_nas <- sum(missing_log)
  hist_plot <- ggplot2::qplot(x = x[!missing_log], xlab = "Dependent Variable", main = "Distribution of Dependent Variable")
  if(class(x) %in% c("numeric", "integer")){
    summary_vals <- dplyr::data_frame(Measure = c("Min", "Q1", "Median", "Q3", "Max", "SD", "% NA's"), Value = c(min(x, na.rm = T), quantile(x, probs = c(.25, .5, .75), na.rm = T), max(x, na.rm = T), sd(x, na.rm = T), mean(missing_log)))
  } else {
    summary_vals <- dplyr::data_frame(Value = rownames(table(x, useNA = "ifany")), Pct = round(as.vector(table(x, useNA = "ifany"))/length(x)*100, 2)) %>% dplyr::arrange(desc(Pct)) %>% head(7)
  }
  binary <- F
  if(length(unique(y)) == 2){
    binary <- T
    y_levels <- sort(unique(y))
  }
  if(binary){
    missing_tab <- dplyr::data_frame(X = x, Y = y) %>% dplyr::mutate(X_NA = is.na(X)) %>% dplyr::group_by(X_NA) %>% dplyr::summarize(Y_Mean = mean(Y)) %>% ggplot2::qplot(data = ., x = X_NA, y = Y_Mean, geom = "bar", stat = "identity", xlab = "Independent Variable is NA", ylab = "Mean of Dependent Variable") + ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(x*100, "%"))
    IVinfo <- WOE(x = x, y = y, incl_NA = T)
    trend_plot <- IVinfo$Plot
    ks_val <- KS(x = x, y = y)
  }
  if(!binary){
    missing_tab <- dplyr::data_frame(X = x, Y = y) %>% dplyr::mutate(X_NA = is.na(X)) %>% ggplot2::qplot(data = ., x = X_NA, y = Y, geom = "boxplot", xlab = "Independent Variable is NA", ylab = "Distribution of Dependent Variable")
    if(class(x) %in% c("numeric", "integer")){
      trend_plot <- ggplot2::ggplot(aes(x = x[!missing_log], y = y[!missing_log])) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "gam", formula = y ~ s(x)) + ggplot2::xlab("Dependent Variable") + ggplot2::ylab("Independent Variable") + ggplot2::ggtitle("Relationship between X and Y")
      cor_val <- cor(x = x, y = y, use = "complete.obs")
    }
    if(class(x) %in% c("factor", "character")){
      trend_plot <- dplyr::data_frame(X = x[!missing_log], Y = y[!missing_log]) %>% dplyr::group_by(X) %>% dplyr::summarize(Y_Mean = mean(Y)) %>% ggplot2::ggplot(aes(x = X, y = Y_Mean)) + ggplot2::geom_bar(stat = "identity") + ggplot2::xlab("Independent Variable") + ggplot2::ylab("Mean of \nDependent Variable") + ggplot2::ggtitle("Relationship between X and Y")
      cor_val <- data_frame(X = x[!missing_log], Y = y[!missing_log]) %>% lm(Y ~ X, data = .) %>% summary %>% extract2("r.squared") %>% sqrt
    }
  }
  summary_tbl <- gridExtra::tableGrob(summary_vals, rows = NULL)
  if(binary){ 
    pred_val <- dplyr::data_frame(Metric = c("IV", "K-S"), Value = round(c(IVinfo$InformationValue, ks_val), 4))
  } else {
    pred_val <- dplyr::data_frame(Metric = "Correlation", Value = round(cor_val, 4))
    
  }
  pred_tbl <- gridExtra::tableGrob(pred_val, rows = NULL)
  fullplot <- gridExtra::arrangeGrob(gridExtra::arrangeGrob(hist_plot, summary_tbl, widths = c(6, 3), nrow = 1), trend_plot, gridExtra::arrangeGrob(missing_tab, pred_tbl, nrow = 1, widths = c(6, 3)), top = var_name)
  if(plot_save){
    pdf(file = fname)
    gridExtra::grid.arrange(gridExtra::arrangeGrob(hist_plot, summary_tbl, widths = c(6, 3), nrow = 1), trend_plot, gridExtra::arrangeGrob(missing_tab, pred_tbl, nrow = 1, widths = c(6, 3)), top = var_name)    
    dev.off()
  }
  return(list(all_info = fullplot, hist_plot = hist_plot, summary_info = summary_vals, trend_plot = trend_plot, missing_plot = missing_tab, pred_info = pred_val))
}