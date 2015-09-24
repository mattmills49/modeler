context("KS Tests")

test_that("Perfect Colinearity", {
  expect_equal(KS(1:100, sort(rep(c(0, 1), 50))), 100)
}

test_that("Order doesn't matter", {
  expect_equal(KS(1:100, rep(c(rep(0, 5), rep(1, 5)), 10)), KS(1:100, rep(c(rep(1, 5), rep(0, 5)), 10)))
  expect_equal(KS(1:100, rep(c(rep(0, 5), rep(1, 5)), 10)), KS(100:1, rep(c(rep(0, 5), rep(1, 5)), 10)), KS(1:100, rep(c(rep(1, 5), rep(0, 5)), 10)))
})

test_that("Y can be factor, numeric, or character", {
  expect_is(KS(1:100, sample(c(0, 1), 100, replace = T)), "numeric")
  expect_is(KS(1:100, sample(c("a", "b"), 100, replace = T)), "numeric")  
}