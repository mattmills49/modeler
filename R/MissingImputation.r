#' Missing Imputation
#' 
#' This function completes regression-based missing imputation. The function accepts a data frame containing variables with missing values. The data frame can contain variables with no missing values if you would not like to include them in replacing the values with missing variables. However the data frame must have at least one column with at least one missing value. The function will look for strictly positive values and percentage values so those same patterns hold with replaced values. If you want to override this you need to include at least one observation where that logic is broken. In addition the function can also use a percentage of values to build the predictive models using \code{sample_frac} option. This is to speed up the missing imputations however not all fractions are faster than just using all observations. I would recomend using a value less than .5. 
#' @param missing_df A data frame containing at least some columns with missing values
#' @param num_iter Number of iterations to perform
#' @param progress A logical indicator to print the number of completed interactions
#' @param sample_frac A number between 0 and 1 indicating the fraction of observations to use to build the predictive models. Default is 1 which will use all observations. 
#' @keywords eda
#' @return A list containing
#' 
#' \code{complete_obs} a data frame with the missing values replaced through regression imputation
#' 
#' \code{change}       The differences between each iteration for each variable
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

MissingImputation <- function(missing_df, num_iter = 10, progress = F, sample_frac = 1){
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
  positive_values <- unlist(lapply(missing_df, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", all(x[!is.na(x)] >= 0), F)))
  pct_values <- unlist(lapply(missing_df, function(x) ifelse(class(x) == "numeric" | class(x) == "integer", all(x[!is.na(x)] <= 1 & x[!is.na(x)] >= 0), F)))
  replace_df <- as.data.frame(lapply(missing_df, function(x){
    na_log <- is.na(x)
    x_na <- sample(x[!na_log], size = sum(na_log), replace = T)
    x[na_log] <- x_na
    return(x)
  }), stringsAsFactors = F)
  cols <- ncol(missing_df)
  change <- matrix(0, nrow = num_iter, ncol = cols)
  colnames(change) <- names(missing_df)
  rownames(change) <- 1:num_iter
  for(i in seq_len(num_iter)){
    if(progress) print(i)
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
          if(sample_frac == 1){
            missing_glm <- glm(formula = paste(names(missing_df)[j], "~ ."), data = reg_data, family = "binomial")
            preds <- vapply(predict(missing_glm, type = "response"), function(x){
              if(is.na(x)){
                print(c(i, j))
                return(sample(level, size = 1))
              } else sample(level, size = 1, prob = c(1 - x, x))}, character(1))
          } else {
            samp <- runif(nrow(reg_data)) <= sample_frac
            reg_matr <- data.frame(model.matrix(formula(paste0(names(missing_df)[j], " ~ .-1")), data = reg_data), stringsAsFactors = F)
            reg_matr$Y <- reg_data[[names(missing_df)[j]]]
            missing_glm <- glm(Y ~ ., data = reg_matr[samp, ], family = "binomial")
            preds <- suppressWarnings(vapply(predict(missing_glm, newdata = reg_matr, type = "response"), function(x){
              if(is.na(x)){
                print(names(replace_df)[j])
                return(sample(level, size = 1))
              } else sample(level, size = 1, prob = c(1 - x, x))}, character(1)))
          }
          change[i, j] <- sum(replace_df[[j]][na_log] != preds[na_log])
          replace_df[[j]][na_log] <- preds[na_log]
        } else {
          pred_matr <- matrix(0, nrow = nrow(reg_data), ncol = length(level) - 1)
          if(sample_frac < 1){
            reg_matr <- data.frame(model.matrix(formula(paste0(names(missing_df)[j], " ~ .-1")), data = reg_data), stringsAsFactors = F)
            samp <- runif(nrow(reg_data)) <= sample_frac
          }
          for(l in seq_len(length(level) - 1)){
            if(sample_frac == 1){
              reg_data$Y <- (reg_data[[names(missing_df)[j]]] == level[l])*1
              factor_glm <- glm(paste0("Y ~ .-", names(missing_df)[j]), data = reg_data, family = "binomial")
              pred_matr[, l] <- predict(factor_glm)
            } else {
              reg_matr$Y <- (reg_data[[names(missing_df)[j]]] == level[l])*1
              factor_glm <- glm(Y ~ ., data = reg_matr[samp, ], family = "binomial")
              pred_matr[, l] <- suppressWarnings(predict(factor_glm, newdata = reg_matr))
            }
          }
          probs <- cbind(exp(pred_matr), 1) / (1 + rowSums(exp(pred_matr)))
          preds <- apply(probs, 1, function(x){
            if(any(is.na(x))){
              print(names(replace_df)[j])
              return(sample(level, size = 1))
            } else sample(level, size = 1, prob = x)})
          change[i, j] <- sum(replace_df[[j]][na_log] != preds[na_log])
          replace_df[[j]][na_log] <- preds[na_log]
        }
      } else {
        if(n_unique == 1){
          replace_df[[j]][na_log] <- unique(missing_df[[j]][!na_log])
        } else {
          if(sample_frac == 1){
            missing_lm <- lm(formula = paste(names(missing_df)[j], "~ ."), data = reg_data)
            preds <- rnorm(length(na_log), mean = predict(missing_lm), sd = summary(missing_lm)$sigma)
          } else {
            samp <- runif(nrow(reg_data)) <= sample_frac
            reg_matr <- data.frame(model.matrix(formula(paste0(names(missing_df)[j], " ~ .-1")), data = reg_data), stringsAsFactors = F)
            reg_matr$Y <- reg_data[[names(missing_df)[j]]]
            missing_lm <- lm(Y ~ ., data = reg_matr[samp, ])
            preds <- suppressWarnings(rnorm(length(na_log), mean = predict(missing_lm), sd = summary(missing_lm)$sigma))
          }
          if(positive_values[j]) preds <- ifelse(preds < 0, 0, preds)
          if(pct_values[j]) preds <- ifelse(preds > 1, 1, preds)
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
# gbm_data <- select(sth_attn_1415, Attended, event_day, block_num_seats, full_price, Section, Price_Name, WinPct, avg_attn_ly, avg_attn_ty, attn_avg, OppWinPct, DivOpp, ConfOpp, ticket_type_category, ticket_category, price_code_name, tenure, account_gender, account_age, income_est, household_size, job, education_level, marriage, distance_to_stadium) %>% mutate(education_level = as.character(education_level))
# missing_data <- select(gbm_data, -Attended, -event_day, -WinPct, -avg_attn_ly, -avg_attn_ty, -OppWinPct, -DivOpp, -ConfOpp)
