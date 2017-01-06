context("Verify Deciles returns proper values and handles proper data types")

test_that("deciles returns all values necessary", {
  expect_equivalent(deciles(0:100), seq(0, 100, by = 10))
})

test_that("deciles handles invalid data types", {
  expect_error(deciles(letters))
  expect_error(deciles(c(T, T, F, F)))
  expect_error(deciles(list("1", 3, "a", T)))
  expect_error(deciles(mtcars))
})

test_that("deciles handles edge cases", {
  expect_equivalent(deciles(c(1, 2, 3)), seq(1, 3, by = .2))
  expect_error(deciles(1))
})

test_that("deciles handles Nas", {
  expect_false(any(is.na(deciles(c(1:100, NA)))))
})

