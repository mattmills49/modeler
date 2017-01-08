#' Preserves all factor levels when creating a model matrix
#' 
#' In some cases it's easier to not have a reference level for dummy variables.
#' This function will preserve all levels of character and factor variables when
#' using model.matrix. 
#' 
#' @param .data a data frame
#' @param object the formula for creating the model
#' @return a numeric matrix to be used for modeling purposes
#' @export
#' @examples 
#' head(model_matrix(iris, Petal.Width ~ Petal.Length + Species))

model_matrix <- function(object, .data){
  chars <- vapply(.data, is.character, logical(1))
  .data[, chars] <- lapply(.data[, chars], as.factor)
  fctrs <- vapply(.data, is.factor, logical(1))
  
  model <- model.matrix(object, 
                        .data,
                        contrasts.arg = lapply(.data[, fctrs, drop = F], contrasts, contrasts = FALSE))
  
  return(model)
}
                        
  
