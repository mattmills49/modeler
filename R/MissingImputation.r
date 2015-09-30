#' Missing Imputation
#' 
#' This function completes regression-based missing imputation. The function accepts a data frame containing variables with missing values. The data frame can contain variables with no missing values if you would not like to include them in replacing the values with missing variables. 
#' @param missing_df A data frame containing at least some columns with missing values
#' @param num_iter Number of iterations to perform
#' @keywords eda
#' @return A list containing
#' \code{complete_obs} a data frame with the missing values replaced through regression imputation
#' \code{change} The differences between each iteration for each variable
#' @export
#' @examples 
#' iris_df <- iris
#' set.seed(123456)
#' na1 <- runif(nrow(iris_df)) < .25
#' na2 <- runif(nrow(iris_df)) < .33
#' iris_df[["Species"]][na1] <- NA
#' iris_df[["Sepal.Length"]][na2] <- NA
#' iris_complete <- MissingImputation(iris_df, num_iter = 5)
#' difs <- data.frame(Species.Orig = as.character(iris[["Species"]][na1]), Species.Replace = iris_complete$complete_obs[["Species"]][na1], Sepal.Orig = iris[["Sepal.Length"]][na1], Sepal.Replace = iris_complete$complete_obs[["Sepal.Length"]][na1], stringsAsFactors = F)
#' \dontrun{View(difs[[1]], difs[[2]])}
#' \dontrun{ggplot2::qplot(x = Sepal.Orig, y = Sepal.Replace, data = difs)}



MissingImputation <- function(missing_df, num_iter = 10){
  if(!("data.frame" %in% class(missing_df))) stop("missing_df must be a data frame")
  if(num_iter < 1) stop("number of iterations must be strictly positive")
  missing_log <- unlist(lapply(missing_df, function(x) any(is.na(x))))
  colorder <- names(missing_df)
  if(sum(missing_log) == 0) stop("data frame must contain some missing values")
  if(all(missing_log)) complete_df <- NULL
  if(!all(missing_log)) complete_df <- missing_df[, !missing_log]
  missing_df <- missing_df[, missing_log]
  factrs <- unlist(lapply(missing_df, is.factor))
  if(any(factrs)) missing_df[, factrs] <- lapply(missing_df[, factrs, drop = F], as.character)
  replace_df <- as.data.frame(lapply(missing_df, function(x){
    na_log <- is.na(x)
    x_na <- sample(x[!na_log], size = sum(na_log), replace = T)
    x[na_log] <- x_na
    return(x)
  }), stringsAsFactors = F)
  cols <- ncol(missing_df)
  change <- matrix(0, nrow = num_iter, ncol = cols)
  for(i in seq_len(num_iter)){
    for(j in seq_len(cols)){
      na_log <- is.na(missing_df[[j]])
      n_unique <- length(unique(missing_df[[j]][!na_log]))
      if(is.null(complete_df)) reg_data <- replace_df
      if(!is.null(complete_df)) reg_data <- cbind(replace_df, complete_df)
      if(class(missing_df[[j]]) == "character"){
reg_data[[names(missing_df)[j]]] <- factor(reg_data[[names(missing_df)[j]]])
        level <- levels(reg_data[[names(missing_df)[j]]])
        if(n_unique == 1){
          replace_df[[j]][na_log] <- unique(missing_df[[j]][!na_log])
        } else if(n_unique == 2){
          missing_glm <- glm(formula = paste(names(missing_df)[j], "~ ."), data = reg_data, family = "binomial")
          preds <- ifelse(predict(missing_glm, type = "response") < .5, level[1], level[2])
          change[i, j] <- sum(replace_df[[j]][na_log] != preds[na_log])
          replace_df[[j]][na_log] <- preds[na_log]
        } else {
          pred_matr <- matrix(0, nrow = length(na_log), ncol = length(level))
          for(l in seq_len(length(level))){
            reg_data$Y <- (reg_data[[names(missing_df)[j]]] == level[l])*1
            factor_glm <- glm(paste0("Y ~ .-", names(missing_df)[j]), data = reg_data, family = "binomial")
            pred_matr[, l] <- predict(factor_glm, type = "response")
          }
          preds <- apply(pred_matr, 1, function(x) level[which.max(x)[1]])
          change[i, j] <- sum(replace_df[[j]][na_log] != preds[na_log])
          replace_df[[j]][na_log] <- preds[na_log]
        }
      } else {
        if(n_unique == 1){
          replace_df[[j]][na_log] <- unique(missing_df[[j]][!na_log])
        } else {
          missing_lm <- lm(formula = paste(names(missing_df)[j], "~ ."), data = reg_data)
          preds <- rnorm(length(na_log), mean = predict(missing_lm), sd = summary(missing_lm)$sigma)
          change[i, j] <- mean(abs(replace_df[[j]][na_log] - preds[na_log]))
          replace_df[[j]][na_log] <- preds[na_log]
        }
      }
    }
  }
  if(is.null(complete_df)) return(list(complete_obs = replace_df, change_amounts = change))
  if(!is.null(complete_df)) return(list(complete_obs = cbind(replace_df, complete_df)[, colorder], change_amounts = change))
}
# load("~/Documents/Data/Magic/sth_attn_1415.rdata")
# complete_df <- sth_attn_1415[, !unlist(lapply(sth_attn_1415, function(x) any(is.na(x))))]
# missing_df <- sth_attn_1415[, unlist(lapply(sth_attn_1415, function(x) any(is.na(x))))]
# complete_df <- complete_df[, c("acct_id", "Section", "row_name", "seat_number", "Seats", "full_price", "ticket_type", "ticket_status", "sold_status", "plan_event_name", "Price_Name")]
# missing_df <- missing_df[, 1:10]