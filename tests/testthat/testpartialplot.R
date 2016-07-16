library(mgcv)
context("Partial Plot")
lin_gam <- gam(mpg ~ s(hp), data = mtcars)


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

test_that("partial plot handles when variables have similar names", {
  test_cars <- mtcars
  test_cars$hpgreaterthan200 <- factor(mtcars$hp > 200)
  test_gam <- gam(mpg ~ s(hp) + hpgreaterthan200, data = test_cars)
  
  expect_is(partial_plot(test_gam, "hp"), "ggplot")
})

test_that("response parameter doesn't impact linear models", {
  lin_plot <- partial_plot(lin_gam, "hp", response = F)
  response_plot <- partial_plot(lin_gam, "hp", response = T)
  expect_equal(lin_plot$data, response_plot$data)
})

test_that("se parameter plots additional layer", {
  lin_plot <- partial_plot(lin_gam, "hp", response = F)
  se_plot <- partial_plot(lin_gam, "hp", response = F, se = T)
  
  expect_true(length(lin_plot$layers) == 1)
  expect_true(length(se_plot$layers) == 2)
  expect_true(class(se_plot$layers[[1]]$geom)[1] == "GeomRibbon")
})