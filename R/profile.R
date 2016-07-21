#' Profile variables
#' 
#' \code{profile} takes in a vector and, depending on the class of the vector,
#' will return summary information on that vector returned as a data.frame. If
#' given a data frame \code{profile} will apply the profile function to each
#' column and return a data.frame of the summary info with an additional 
#' column for the variable name associated with that row. 
#' @param variable a vector you wish to profile. Supply a data frame if you 
#' wish to apply the \code{profile} function to each column 
#' @export
#' @examples 
#' set.seed(123456)
#' sample_variable <- sample(letters, size = 100, replace = T)
#' profile(sample_variable)
#' 
#' ## Profile entire data frame
#' profile(iris)

profile <- function(variable, ...) UseMethod("profile")

#' @describeIn profile profile numeric vectors
#' @export

profile.numeric <- function(variable) {
  num_missing <- sum(is.na(variable))
  summry <- summary(variable)[1:6]
  std_dev <- sd(variable, na.rm = T)
  terms = c("Mean", "Median", "Min", "Max", "1st_Q", "3rd_Q", "SD", "Num_Missing") 
  values = c(unname(summry)[c(4, 3, 1, 6, 2, 5)], std_dev, num_missing)
  var_info <- dplyr::data_frame(Term = terms, Value = values)
  return(var_info)
}
  
#' @describeIn profile profile character vectors
#' @export

profile.character <- function(variable) {
  num_missing <- sum(is.na(variable))
  top_5 <- head(sort(table(variable), decreasing = T, na.last = NA), 5)
  num_unique <- length(unique(variable))
  terms <- c(names(top_5), "Num_Unique", "Num_Missing")
  values <- c(unname(top_5), num_unique, num_missing)
  var_info <- dplyr::data_frame(Term = terms, Value = values)
  return(var_info)
}

#' @describeIn profile profile factor vectors
#' @export 

profile.factor <- function(variable) {
  profile.character(as.character(variable))
}

#' @describeIn profile profile entire data frame
#' @export

profile.data.frame <- function(variable) {
  prof_each <- lapply(variable, profile)
  var_info <- dplyr::bind_rows(prof_each, .id = "Variable")
  return(var_info)
}

