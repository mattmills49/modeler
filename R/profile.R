#' Profile variables
#' 
#' \code{profile} takes in a vector and, depending on the class of the vector,
#' will return summary information on that vector returned as a data.frame. 
#' @param element a vector you wish to profile. 
#' @export
#' @examples 
#' set.seed(123456)
#' sample_variable <- sample(letters, size = 100, replace = T)
#' profile(sample_variable)
#' 

profile <- function(element) UseMethod("profile")

#' @describeIn profile profile numeric vectors
#' @export

profile.numeric <- function(element){
  num_missing <- sum(is.na(element))
  num_unique <- length(unique(element))
  if(min(element, na.rm = T) > 0 & max(element, na.rm = T) < 1) {
    type <- "Percentage"  
  } else if(num_unique - (num_missing > 0) == 2) {
    type <- "Binary"
  } else type <- "Numeric"
  
  summry <- summary(element)[1:6]
  std_dev <- sd(element, na.rm = T)
  terms = c("Class", "Type", "Mean", "Median", "Min", "Max", "First_Quartile", "Third_Quartile", "SD", "Num_Missing", "Num_Unique") 
  values = c("Numeric", type, unname(summry)[c(4, 3, 1, 6, 2, 5)], std_dev, num_missing, num_unique)
  var_info <- dplyr::data_frame(Term = terms, Value = values) %>%
    tidyr::spread(Term, Value)
  return(var_info)
}
  
#' @describeIn profile profile character vectors
#' @export

profile.character <- function(element) {
  num_missing <- sum(is.na(element))
  num_unique <- length(unique(element))
  
  if(num_unique - (num_missing > 0) == 2) {
    type <- "Binary"
  } else type <- "Character"
  
  top_5 <- head(sort(table(element), decreasing = T, na.last = NA), 5)
  terms <- c("Class", "Type", names(top_5), "Num_Unique", "Num_Missing")
  values <- c("Character", type, unname(top_5), num_unique, num_missing)
  var_info <- dplyr::data_frame(Term = terms, Value = values) %>%
    tidyr::spread(Term, Value)
  return(var_info)
}

#' @describeIn profile profile factor vectors
#' @export 

profile.factor <- function(variable) {
  factor_info <- profile.character(as.character(variable))
  factor_info$Value[factor_info$Term == "Class"] <- "Factor"
  return(factor_info)
}

#' @describeIn profile profile factor vectors
#' @export 

profile.logical <- function(variable) {
  log_info <- profile.character(as.numeric(variable))
  log_info$Value[log_info$Term == "Class"] <- "Logical"
  return(log_info)
}

