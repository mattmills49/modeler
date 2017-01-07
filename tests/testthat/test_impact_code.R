context("impact_code")

test_that("impact_code only accepts data frames", {
  expect_error(impact_code(iris$Species, Species, y = test))
})

test_that("impact_code stops if missing variables in the y variable", {
  bad_iris <- iris
  bad_iris$Pedal.Width[20] <- NA
  expect_error(impact_code(bad_iris, Species, y = Petal.Width))
})