context("impact_code")

test_that("impact_code data handling errors", {
  bad_iris <- iris
  bad_iris$Pedal.Width[20] <- NA
  
  expect_error(impact_code(bad_iris, Petal.Width ~ Species, binary = F), info = "Y can't have missing values")
  expect_error(impact_code(iris$Species, Petal.Width ~ Species, binary = F), info = "only works on data frames")
  expect_error(impact_code(mtcars, cyl ~ am), info = "Y can only have two values")
})


#' test for variable names, 
#' small groups, 
#' 1 and multiple variables, 
#' missing values
#' more than 3 values

test_that("impact_code works on continous variables", {
  expect_is(impact_code(iris, Petal.Width ~ Species, binary = F), "data.frame")
})

test_that("impact_code works on different classes of variables", {
  expect_is(impact_code(mtcars, am ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, factor(am) ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, am == 1 ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, as.character(am) ~ cyl), "data.frame")
})