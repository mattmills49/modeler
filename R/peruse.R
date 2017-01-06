#' Peruse a Data Frame
#' 
#' \code{peruse} examines the variables in a data frame and returns basic 
#' summary info about the individual variables. Different values will be 
#' returned for different types of columns so the function returns a data frame 
#' with the underlying data nested.
#' @param data a data frame
#' @export
#' @examples 
#' cars_summary <- peruse(mtcars)
#' numeric_info <- cars_summary %>%
#'   dplyr::filter(Class == "Numeric") %>%
#'   tidyr::unnest(data)
#' 

peruse <- function(df){
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(nrow(df) > 0)
  perused <- df %>%
    purrr::map(profile) %>%
    purrr::map(tidyr::nest, -Class, -Type, -Num_Missing, -Num_Unique) %>%
    dplyr::bind_rows(.id = "Variable") %>%
    dplyr::select(Variable, Class, Type, dplyr::everything())
  return(perused)
}
