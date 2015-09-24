#' Find Weight Of Evidence
#' 
#' This function finds the WOE and IV of a variable in regards to its relationship with the dependent variable
#' @param x Independent variable
#' @param y Must be Categorical or only have two values. Also can't contain any \code{NA} values
#' @param bins The number of buckets to bin the independent variable (\code{x})
#' @param adj A small adjustment should be included if some buckets may only have 1 level of \code{y}. Defaults to .5
#' @param incl_NA should \code{NA} observations be included? Default = TRUE
#' @return A list containing:
#' @return \code{InformationValue} The information value from the WOE
#' @return \code{Bins} The cutpoints for the bins
#' @return \code{Plot} ggplot object with the WOE of each bin. 
#' @keywords WOE, IV
#' @export 
#' @examples
#' result <- WOE(x = 1:100, y = sample(c(0, 1), 100, replace = T))

WOE <- function(x, y, bins = 10, adj = .5, incl_NA = T){
  if(length(unique(y)) != 2) stop("Y variable is not binary")
  if(typeof(x) == "list" | typeof(y) == "list") stop("x and y must be vectors not lists")
  if(length(x) != length(y)) stop("x and y are not the same length")
  if(any(is.na(y))) stop("y must not contain any missing values")
  na_log <- is.na(x)
  if(class(x) == "numeric" | class(x) == "integer"){
    if(!incl_NA){
      cuts <- cut(x[!na_log], breaks = unique(quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = T)), include.lowest = T)
      x_bins <- as.character(cuts)
      y <- y[!na_log]
    } else {
      x_bins <- rep("NA", length(x))
      cuts <- cut(x[!na_log], breaks = unique(quantile(x, probs = seq(0, 1, length.out = bins), na.rm = T)), include.lowest = T)
      x_bins[!na_log] <- as.character(cuts)
      #names(x_bins)[!na_log] <- as.character(cut(x[!na_log], breaks = unique(quantile(x, probs = seq(0, 1, length.out = bins), na.rm = T)), include.lowest = T))
    }
  }
  if(class(x) == "character" | class(x) == "factor"){
    if(!incl_NA){
      cuts <- factor(x[!na_log], levels = unique(as.character(x[!na_log])))
      x_bins <- as.character(x[!na_log])
      y <- y[!na_log]
    } else {
      cuts <- factor(x, levels = unique(as.character(x)))
      x_bins <- rep("NA", length(x))
      x_bins[!na_log] <- as.character(x)
    }
  }
  bins <- length(unique(x_bins))
  levels <- sort(unique(y))
  x_tab <- prop.table(table(x_bins, y == levels[2]) + adj, 2)
  x_woe <- log(x_tab[, 2] / x_tab[, 1])
  iv <- sum((x_tab[, 2] - x_tab[, 1]) * x_woe)
  if(incl_NA){
    if(sum(na_log) > 0) plotdata <- data.frame(X = factor(c(levels(cuts), "NA"), levels = c(levels(cuts), "NA")), Y = x_woe[c(levels(cuts), "NA")], stringsAsFactors = F)
    else plotdata <- data.frame(X = factor(c(levels(cuts)), levels = c(levels(cuts))), Y = x_woe[c(levels(cuts))], stringsAsFactors = F)
  } else plotdata <- data.frame(X = factor(c(levels(cuts)), levels = c(levels(cuts))), Y = x_woe[c(levels(cuts))], stringsAsFactors = F)
  woeplot <- ggplot2::qplot(x = X, y = Y, data = plotdata, xlab = paste("IV =", round(iv, 4)), ylab = "WOE", geom = "bar", stat = "identity", main = "Larger Values have more Y = 1") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  return(list(InformationValue = iv, Bins = rownames(x_tab), Plot = woeplot))
}