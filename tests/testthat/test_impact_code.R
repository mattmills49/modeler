context("impact_code")

test_that("impact_code data handling errors", {
  bad_iris <- iris
  bad_iris$Petal.Width[20] <- NA
  
  expect_error(impact_code(bad_iris, Petal.Width ~ Species, binary = F), info = "Y can't have missing values")
  expect_error(impact_code(iris$Species, Petal.Width ~ Species, binary = F), info = "only works on data frames")
  expect_error(impact_code(mtcars, cyl ~ am), info = "Y can only have two values")
})

test_that("impact_code handles messy data", {
  bad_mtcars <- mtcars
  bad_mtcars$cyl[c(10, 20, 30)] <- NA
  
  messy_mtcars <- dplyr::bind_rows(mtcars, dplyr::data_frame(cyl = 7, hp = 100))
  
  expect_equal(nrow(impact_code(bad_mtcars, am ~ cyl)), 4, info = "impact_code keeps NA levels")
  expect_equal(ncol(impact_code(mtcars, am ~ cyl + gear)), 3, info = "impact_code handles more than 1 independent variable")
  #expect_equal(ncol(impact_code(messy_mtcars, hp ~ cyl, binary = F)))
})


#' test for variable names, 
#' small groups

test_that("impact_code works on continous variables", {
  expect_is(impact_code(iris, Petal.Width ~ Species, binary = F), "data.frame")
})

test_that("impact_code works on different classes of variables", {
  expect_is(impact_code(mtcars, am ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, factor(am) ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, am == 1 ~ cyl), "data.frame")
  expect_is(impact_code(mtcars, as.character(am) ~ cyl), "data.frame")
})