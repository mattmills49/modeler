#' NA Replace
#' 
#' This function takes in an x and y vector and attempts to replace missing values. Replacement can be done with the mean or median value of the non-missing values or using a regression replace technique. The regression technique first finds the predicted response value of y for the non missing elements of x. The average value of y for the missing values is then used to find the x-value that most closely aligns with this average value of y for the missing values.
#' @param x independent variable, must be a vector. If x is a factor it will be converted to a character vector
#' @param y dependent variable, must be a vector and can't contain missing values
#' @param replace The type of \code{NA} replacement method. Options include \code{mean}, \code{median}, \code{linear}, and \code{smooth}. 
#' @keywords eda
#' @return A list containing
#' \code{x_orig} The original x vector
#' \code{x_replace} The new vector with missing values filled in
#' \code{na_indicator} A dummy variable (0 or 1) with that tells you the indices of missing values
#' \code{replacement_val} The value that the means were replaced with
#' @export
#' @examples 
#' y <- c(sample(c(0, 1), 25, replace = T, prob = c(.25, .75)), sample(c(0, 1), 25, replace = T, prob = c(.4, .6)), sample(c(0, 1), 25, replace = T, prob = c(.6, .4)), sample(c(0, 1), 25, replace = T, prob = c(.75, .25)))
#' set.seed(924516)
#' x_real <- (seq(1, 10, length.out = 100))^2 + runif(100, 0, 5)
#' x <- x_real
#' x[1:50][runif(50) < .33] <- NA
#' nafix <- NAReplace(x, y, replace = "mean")
#' nafix$replacement_val
#' # [1] 45.56847
#' nafix <- NAReplace(x, y, replace = "linear")
#' nafix$replacement_val
#' # [1] 11.80703
#' x_char <- c(sample(letters[1:10], 50, replace = T), sample(letters[1:5], 50, replace = T))
#' x_char[50:100][runif(50) < .25] <- NA
#' nafix <- NAReplace(x_char, y, replace = "median")
#' nafix$replacement_val
#' # [1] "c"
#' nafix <- NAReplace(x_char, y, replace = "linear")
#' nafix$replacement_val
#' # [1] "d"

NAReplace <- function(x, y, replace = "linear"){
  if(is.factor(x)) x <- as.character(x)
  if(!is.vector(x)) stop("x must be a vector")
  if(!is.vector(y)) stop("y must be a vector")
  if(any(is.na(y))) stop("y can't contain missing values")
  na_log <- is.na(x)
  x_na <- x
  if(class(x) == "numeric" | class(x) == "integer"){
    if(replace == "mean"){
      na_val <- mean(x, na.rm = T)
    }
    if(replace == "median"){
      na_val <- median(x, na.rm = T)
    }
    if(replace == "linear"){
      if(length(unique(y)) == 2){
        na_model <- glm(y[!na_log] ~ x[!na_log], family = "binomial")
        preds <- predict(na_model, type = "response")
      } else {
        na_model <- lm(y[!na_log] ~ x[!na_log])
        preds <- predict(na_model)
      }
      y_na_mean <- mean(y[na_log])
      na_val <- x[!na_log][which.min(abs(preds - y_na_mean))[1]]
    }
    if(replace == "smooth"){
      if(length(unique(y)) == 2){
        na_model <- gam::gam(y[!na_log] ~ gam::s(x[!na_log]), family = "binomial")
        preds <- predict(na_model, type = "response")
      } else {
        na_model <- gam::gam(y[!na_log] ~ gam::s(x[!na_log]), family = "gaussian")
        preds <- predict(na_model)
      }
      y_na_mean <- mean(y[na_log])
      na_val <- x[!na_log][which.min(abs(preds - y_na_mean))[1]]
    }
  } else {
    if(replace == "mean"){
      na_val <- attr(sort(table(x), decreasing = T)[1], "names")
    }
    if(replace == "median"){
      na_val <- attr(sort(table(x), decreasing = T)[1], "names")
    }
    if(replace == "linear"){
      if(length(unique(y)) == 2){
        na_model <- glm(y[!na_log] ~ x[!na_log], family = "binomial")
        preds <- predict(na_model, type = "response")
      } else {
        na_model <- lm(y[!na_log] ~ x[!na_log])
        preds <- predict(na_model)
      }
      y_na_mean <- mean(y[na_log])
      na_val <- x[!na_log][which.min(abs(preds - y_na_mean))[1]]
    }
  }
    x_na[na_log] <- na_val
    return(list(x_orig = x, x_replace = x_na, na_indicator = na_log*1, replacement_val = na_val))
}