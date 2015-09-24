#' Find KS Statistic of an Independent - Dependent Variable Combo
#' 
#' This function calculates the KS value associated with a linear model made from a single predictor on a binary dependent variable
#' @param y Must be Categorical or only have two values. Also can't contain any \code{NA} values
#' @keywords ks
#' @return The KS value for the independent variable
#' @export
#' @examples
#' KS(x = 1:100, y = sample(c(0, 1), 100, replace = T))

KS <- function(x, y){
  if(length(unique(y)) != 2) stop("Y variable is not binary")
  if(typeof(x) == "list" | typeof(y) == "list") stop("x and y must be vectors not lists")
  if(length(x) != length(y)) stop("x and y are not the same length")
  na_log <- is.na(x)
  x <- x[!na_log]
  y <- y[!na_log]
  levels <- sort(unique(y))
  lin_model <- glm(factor(y) ~ x, family = "binomial")
  preds <- predict(lin_model, type = "response")
  values <- quantile(preds, probs = seq(min(preds), max(preds), length.out = 20))
  pct_pos <- sapply(values,function(x) sum(y[preds <= x] == levels[2])/(sum(y == levels[2])))
  pct_neg <- sapply(values,function(x) sum(y[preds <= x] == levels[1])/(sum(y == levels[1])))
  info <- data.frame(Values = values,
                     Pct_of_Values = seq(min(preds), max(preds), length.out = 20),
                     Pct_of_1 = pct_pos,
                     Pct_of_0 = pct_neg)
  k_s <- max(abs(info$Pct_of_1 - info$Pct_of_0))*100
  return(k_s)
}
