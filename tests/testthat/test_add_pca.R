library(stringr)
context("add_pca")

test_that("add_pca adds the correct amount of columns", {
  expect_equal(ncol(mtcars), sum(str_count(names(add_pca(mtcars)), "\\.pc[0-9]{1}")))
  expect_equal(3, sum(str_count(names(add_pca(mtcars, n = 3)), "\\.pc[0-9]{1}")))
  expect_error(add_pca(mtcars, n = 0))
  expect_error(add_pca(mtcars, n = 12))
})

test_that("add_pca includes correct new names", {
  expect_equal(str_c(".", "pc", 1:11), names(add_pca(mtcars))[12:22])
  expect_equal(str_c("pca_test.", "pc", 1:11), names(add_pca(mtcars, new_column = "pca_test"))[12:22])
})

test_that("add_pca only works on numeric columns", {
  expect_error(add_pca(iris))
})

test_that("add_pca only works on data frames", {
  expect_error(add_pca("test"))
})

test_that("add_pca handles missing values", {
  mtcars_missing <- mtcars
  mtcars_missing$hp[c(1, 3, 5)] <- NA
  
  expect_equal(nrow(mtcars), nrow(add_pca(mtcars_missing, n = 3)))
}
