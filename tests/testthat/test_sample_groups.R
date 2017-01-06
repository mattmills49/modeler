context("sample_groups")

test_that("sample_groups returns all columns", {
  expect_equal(ncol(mtcars), ncol(sample_groups(mtcars, cyl, n = 1)))
  expect_equal(names(mtcars), names(sample_groups(mtcars, cyl, n = 1)))
})

test_that("returns only certain groupings", {
  expect_equal(6, nrow(dplyr::distinct(sample_groups(mtcars, cyl, am, gear), cyl, am, gear)))
})
