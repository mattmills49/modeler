#' returns the min, deciles, and max for a numeric vector
#'
#' \code{deciles} takes in a numeric vector and returns a named vector that
#' contains the min, 10, 20, 30, 40, 50, 60, 70, 80, and 90th percentiles,
#' and the max. Missing values are always removed
#'
#' @param x a numeric or integer vector
#' @return a numeric vector with names for the corresponding decile
#' @export
#' @examples
#' deciles(0:100)

deciles <- function(x) {
  assertthat::assert_that(is.vector(x))
  assertthat::assert_that(class(x) == "integer" || class(x) == "numeric")
  assertthat::assert_that(length(x) > 1)
  
  
  v <- c(min(x, na.rm = T), quantile(x, probs = seq(.1, .9, .1), na.rm = T), max(x, na.rm = T))
  names(v)[c(1,11)] <- c("min", "max")
  return(v)
}

#' Find out how many missing values are in each column of a data frame
#'
#' \code{how_many_nas} finds the number of missing values in the columns of a
#' data frame. You can also have it return the percentage of missing values
#' instead of the raw totals.
#'
#' @param x a data frame to examine
#' @param p a logical flag indicating if the results should be in percentages.
#' Defaults to \code{FALSE}
#' @return a named vector with the column names as the names and the number
#' or percentage of missing values as the values
#' @export
#'
#' @examples
#' set.seed(12345)
#' how_many_nas(mtcars)
#' how_many_nas(iris, p = T)
#'

how_many_nas <- function(x, p = F){
  if("data.frame" %in% class(x)){
    if(p){
      vapply(x, function(y) sum(is.na(y)), numeric(1)) / nrow(x)
    } else vapply(x, function(y) sum(is.na(y)), numeric(1))
  } else stop("x is not a data.frame")
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
#' plots multiple ggplot plots in a grid
#'
#' Copied directly from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#'
#' @param ... list of ggplot2 objects to plot
#' @param plotlist A list of plots to add to
#' @param cols the number of columns in the resulting plot grid
#' @param layout custom layout design If the layout is something like
#' matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper
#' left, 2 will go in the upper right, and 3 will go all the way across the
#' bottom.
#' @return plots the ggplots
#' @export
#
multiplot <- function(..., plotlist=NULL, cols = 1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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

#' Prints samples from data structre
#'
#' \code{view} randomly samples from the input and prints the selected values
#' to the console. This helps see values that \code{head} and \code{tails} may
#' not reach.
#'
#' @param x A data frame or vector
#' @param n the number of rows or values to print
#' @return the printed values
#' @export
#' @examples
#' view(letters)
#' view(mtcars, n = 4)
#'


view <- function(x, n = 6) {
  if ("data.frame" %in% class(x)) {
    if (n > nrow(x)) {
      stop("n must be smaller than the number of rows in data frame")
    } else {
      print(dplyr::tbl_df(dplyr::sample_n(x, n)))
    }
  }
  else if (is.vector(x) & !is.list(x)) {
    if (n > length(x)) {
      stop("n must be smaller than the length of the vector")
    } else print(sample(x, size = n))
  } else stop("x must be a vector or data.frame")
}

#' Removes values from a vector
#'
#' The \code{remove_} functions are convenient wrappers to help remove specific
#' values from a vector. \code{remove_from} will remove single or multiple
#' values from a given vector. In addition the \code{remove_null} and
#' \code{remove_na} functions will remove the corresponding missing values.
#'
#' @param vec the vector to remove values from
#' @param value the value to remove. The value can either be a single value or
#' a vector of values
#' @export
#' @return the original vector with the values removed
#'
#'

remove_from <- function(vec, value) vec[!(vec %in% value)]

#' @describeIn remove_from Removes null values
remove_null <- function(vec) vec[!is.null(vec)]

#' @describeIn remove_from Removes NA values
remove_na <- function(vec) vec[!is.na(vec)]