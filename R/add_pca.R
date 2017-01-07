#' add pca variables to a data frame
#' 
#' \code{add_pca} takes a data frame and the columns that you wish to compute 
#' the pca values for. You can use \code{dplyr}'s select rules to pick the
#' columns you want or omit the ones you don't want to include. Only numeric
#' columns are allowed and any rows with missing values will be returned with
#' missing values for the PCA columns. It will append these new pca loadings to
#' the data frame as additional columns. 
#' 
#' @param .data A data frame
#' @param ... Comma separated list of unquoted expressions. You can treat 
#'   variable names like they are positions. Use positive values to select 
#'   variables; use negative values to drop variables.
#' @param new_column the initial name of the new columns to append. If left null
#'   then variables will just be returned with .pc and the number for the
#'   loading.
#' @param n the number of princple components to return. If left null then all 
#'   loadings will be returned.
#' @return the original data frame with the new principle component columns
#' @export
#' @examples 
#' add_pca(mtcars, mpg:wt, new_column = "car_specs", n = 3) %>% head(3)

add_pca <- function(.data, ..., new_column = NULL, n = NULL){
  
  stopifnot(is.data.frame(.data))
  
  if(length(substitute(list(...))) == 1L){
    pca_data <- .data
  } else pca_data <- dplyr::select(.data, ...)
  all_numeric <- purrr::map_lgl(pca_data, is.numeric)
  stopifnot(all_numeric)
  
  missing_obs <- apply(pca_data, 1, function(x) any(is.na(x)))
  
  pca_info <- prcomp(x = pca_data[!missing_obs, ], scale. = T)
  pca_trans <- as.data.frame(pca_info$x, row.names = F)
  
  rot_data <- data.frame(matrix(NA, nrow = nrow(pca_data), ncol = ncol(pca_trans)))
  names(rot_data) <- names(pca_trans)
  rot_data[!missing_obs, ] <- pca_trans 
  
  if(!is.null(n)){
    if(n <= 0){ 
      stop("well, you've got to pick *some* pcas to include")
    } else if(n > ncol(pca_data)){
      stop("n is larger than the number of variables")
    } else rot_data <- rot_data[, seq_len(n), drop = F]
  }
  
  if(is.null(new_column)){
    names(rot_data) <- stringr::str_c(".", "pc", seq_along(names(rot_data)))
  } else {
    names(rot_data) <- stringr::str_c(new_column, ".", "pc", seq_along(names(rot_data)))
  }
  
  new_data <- dplyr::bind_cols(.data, rot_data)
  return(new_data)
}
