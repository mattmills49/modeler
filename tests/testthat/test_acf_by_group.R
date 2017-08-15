context("acf_by_group")

sample_data <- dplyr::data_frame(group = sample(c("a", "b", "c"), size = 100, replace = T), value = sample.int(30, size = 100, replace = T)) 

test_that("acf_by_group returns a data.frame", {
  expect_is(acf_by_group(sample_data, group, value), "data.frame")
})

test_that("acf_by_group accepts acf parameters", {
  expect_equal(nrow(acf_by_group(sample_data, group, value, lag.max = 4)), 15)
})

test_that("acf_by_group handles missing/bad arguments", {
  expect_error(acf_by_group(sample_data, .group_var = group))
  #expect_error(acf_by_group(sample_data, .value_var = value))
  expect_error(acf_by_group(sample_data, group2, value))
  expect_error(acf_by_group(sample_data, group, value2))
  expect_error(acf_by_group(sample_data, group, NULL))
})