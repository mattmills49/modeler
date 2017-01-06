#' Uses the base \code{table} function but always checks for NAs
#'
#' @param ... the object you want to use the \code{table} function on
#' @return a \code{table}
#' @export
#' @examples
#' car_copy <- mtcars
#' car_copy$cyl[c(2, 7, 15)] <- NA
#' table(car_copy$cyl)
#' tableNA(car_copy&cyl)

tableNA <- function(...){
  table(..., useNA = c("ifany"))
}
