# Test edge cases and input validation

library(testthat)
library(dml.sensemakr)

# === dml() input validation ===
test_that("dml() errors on invalid model", {
  data("pension", package = "dml.sensemakr")
  y <- pension$net_tfa[1:100]
  d <- pension$e401[1:100]
  x <- model.matrix(~ -1 + age + inc, data = pension[1:100, ])
  expect_error(dml(y, d, x, model = "invalid"))
})

# === caretArgs ===
test_that("caretArgs handles string input", {
  result <- dml.sensemakr:::caretArgs("ranger")
  expect_type(result, "list")
  expect_true(!is.null(result$method))
  expect_true(!is.null(result$trControl))
})

test_that("caretArgs handles list input", {
  result <- dml.sensemakr:::caretArgs(list(method = "ranger"))
  expect_type(result, "list")
  expect_true(!is.null(result$method))
})

# === combine.cross.fits edge cases ===
test_that("combine.cross.fits with single repetition", {
  results <- list(
    list(estimates = list(theta.s = 3.0, se.theta.s = 0.5))
  )
  combined <- dml.sensemakr:::combine.cross.fits(results, param = "theta.s")
  expect_equal(combined["mean", "estimate"], 3.0)
  expect_equal(combined["median", "estimate"], 3.0)
})

# === confidence_bounds with different return options ===
test_that("confidence_bounds.dml.bounds respects return parameter", {
  data("pension", package = "dml.sensemakr")
  set.seed(55)
  idx <- sample(nrow(pension), 300)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ, data = pension[idx, ])
  fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, verbose = FALSE)
  bounds <- dml_bounds(fit, cf.y = 0.04, cf.d = 0.03)

  cb_lwr <- confidence_bounds(bounds, return = "lwr")
  expect_equal(ncol(cb_lwr), 1)
  expect_equal(colnames(cb_lwr), "lwr")

  cb_upr <- confidence_bounds(bounds, return = "upr")
  expect_equal(ncol(cb_upr), 1)
  expect_equal(colnames(cb_upr), "upr")

  cb_both <- confidence_bounds(bounds, return = c("lwr", "upr"))
  expect_equal(ncol(cb_both), 2)
})

# === confidence_bounds with new sensitivity parameters ===
test_that("confidence_bounds.dml.bounds recomputes with new parameters", {
  data("pension", package = "dml.sensemakr")
  set.seed(55)
  idx <- sample(nrow(pension), 300)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ, data = pension[idx, ])
  fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, verbose = FALSE)
  bounds <- dml_bounds(fit, cf.y = 0.04, cf.d = 0.03)

  cb_orig <- confidence_bounds(bounds)
  cb_new <- confidence_bounds(bounds, cf.y = 0.10, cf.d = 0.10)

  # New bounds with stronger confounding should be wider
  width_orig <- cb_orig[1, "upr"] - cb_orig[1, "lwr"]
  width_new <- cb_new[1, "upr"] - cb_new[1, "lwr"]
  expect_true(width_new > width_orig)
})

# === robustness_value edge case: already includes zero ===
test_that("robustness_value returns 0 when CI includes theta", {
  data("pension", package = "dml.sensemakr")
  set.seed(55)
  idx <- sample(nrow(pension), 300)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ, data = pension[idx, ])
  fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, verbose = FALSE)

  # Use a theta value well inside the CI
  ci <- confint(fit)
  theta_inside <- mean(ci[1, ])  # midpoint of CI
  rv <- robustness_value(fit, theta = theta_inside)
  expect_equal(rv[[1]], 0)
})
