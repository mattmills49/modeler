context("acf_by_group")

sample_data <- dplyr::data_frame(group = sample(c("a", "b", "c"), size = 100, replace = T), value = sample.int(30, size = 100, replace = T)) 

test_that("acf_by_group returns a data.frame", {
  expect_is(acf_by_group(sample_data, group, value), "data.frame")
})

test_that("acf_by_group accepts acf parameters", {
  expect_is(ratings_acf <- acf_by_group(pres_ratings, pre_1965, approval, na.action = na.pass), "data.frame")
  expect_equal(nrow(acf_by_group(sample_data, group, value, lag.max = 4)), 15)
})

test_that("acf_by_group handles missing/bad arguments", {
  expect_error(acf_by_group(sample_data, .group_var = group))
  #expect_error(acf_by_group(sample_data, .value_var = value))
  expect_error(acf_by_group(sample_data, group2, value))
  expect_error(acf_by_group(sample_data, group, value2))
  expect_error(acf_by_group(sample_data, group, NULL))
})

test_that("acf_by_group returns proper standard error values", {
  expect_equal(ncol(acf_by_group(sample_data, group, value, .ci = "none")), 3)
  expect_equal(ncol(acf_by_group(sample_data, group, value, .ci = "white")), 4)
  expect_equal(ncol(acf_by_group(sample_data, group, value, .ci = "ma")), 4)
  expect_error(acf_by_group(sample_data, group, value, .ci = "testhis"))
  expect_true(length(unique(acf_by_group(sample_data, .value_var = value, .ci = "white")$se)) == 1)
  expect_true(length(unique(acf_by_group(sample_data, .value_var = value, .ci = "ma")$se)) > 1)
})