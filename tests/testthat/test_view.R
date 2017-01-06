context("Test that view works on the proper data types")

test_that("view returns samples correctly", {
  expect_true(is.vector(view(letters)))
  expect_true(is.data.frame(view(mtcars)))
})

test_that("view accepts data frames, data.tables, and tbls", {
  expect_output(view(mtcars))
  expect_output(view(data.table::data.table(mtcars)))
  expect_output(view(dplyr::as_data_frame(mtcars)))
})

test_that("view accepts all vector types", {
  expect_is(view(letters), "character")
  expect_is(view(1:20), "integer")
  expect_is(view(sample(c(T, F), size = 10, rep = T)), "logical")
})

test_that("view rejects lists, matrices, and arrays", {
  test_matrix <- matrix(1:9, nrow = 3)
  test_list <- list(1:5)
  test_array <- array(1:27, dim = 3)

  expect_error(view(test_matrix, n = 1), "x must be a vector or data.frame")
  expect_error(view(test_list, n = 1), "x must be a vector or data.frame")
  expect_error(view(test_array, n = 1), "x must be a vector or data.frame")
})

test_that("view won't print when n is greater than the size", {
  expect_error(view(letters, n = 50), "n must be smaller than the length of the vector")
  expect_error(view(mtcars, n = 35), "n must be smaller than the number of rows in data frame")
})
