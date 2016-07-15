library(mgcv)
context("Partial Plot")

test_that("partial plot accepts multiple smoothing parameters", {
  s_gam <- gam(mpg ~ s(hp), data = mtcars)
  te_gam <- gam(mpg ~ te(hp), data = mtcars)
  ti_gam <- gam(mpg ~ ti(hp), data = mtcars)
  t2_gam <- gam(mpg ~ t2(hp), data = mtcars)
  
  expect_is(partial_plot(s_gam, "hp"), "ggplot")
  expect_is(partial_plot(te_gam, "hp"), "ggplot")
  expect_is(partial_plot(ti_gam, "hp"), "ggplot")
  expect_is(partial_plot(t2_gam, "hp"), "ggplot")
})