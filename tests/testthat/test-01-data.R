# Test that package datasets load correctly

library(testthat)
library(dml.sensemakr)

test_that("pension dataset loads and has expected structure", {
  data("pension", package = "dml.sensemakr")
  expect_true(is.data.frame(pension))
  expect_equal(nrow(pension), 9915)
  expect_true("net_tfa" %in% names(pension))
  expect_true("e401" %in% names(pension))
  expect_true("age" %in% names(pension))
  expect_true("inc" %in% names(pension))
})

test_that("Penn dataset loads and has expected structure", {
  data("Penn", package = "dml.sensemakr")
  expect_true(is.data.frame(Penn))
  expect_equal(nrow(Penn), 5099)
  expect_true("tg" %in% names(Penn))
  expect_true("inuidur1" %in% names(Penn))
  expect_true("female" %in% names(Penn))
})

test_that("gasdemand dataset loads and has expected structure", {
  data("gasdemand", package = "dml.sensemakr")
  expect_true(is.data.frame(gasdemand))
  expect_equal(nrow(gasdemand), 3640)
  expect_true("log_q" %in% names(gasdemand))
  expect_true("log_p" %in% names(gasdemand))
})
