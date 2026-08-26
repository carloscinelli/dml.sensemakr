# Test conditional ATT/ATU estimation for NPM.
# The outcome regression is fit on a single treatment arm and used to impute the
# counterfactual mean for the other arm.

library(testthat)
library(dml.sensemakr)

# Fit conditional ATT (uncentered and centered, sharing a seed so the cross-fits
# are identical and only nu2.s differs) and conditional ATU, once, on a small
# subset for speed.
setup_cond <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(505)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  att_unc <- dml(y, d, x, model = "npm", target = "att", cf.folds = 2, cf.reps = 1,
                 cf.seed = 123, centered = FALSE, verbose = FALSE)
  att_cen <- dml(y, d, x, model = "npm", target = "att", cf.folds = 2, cf.reps = 1,
                 cf.seed = 123, centered = TRUE, verbose = FALSE)
  atu     <- dml(y, d, x, model = "npm", target = "atu", cf.folds = 2, cf.reps = 1,
                 cf.seed = 123, verbose = FALSE)
  list(att_unc = att_unc, att_cen = att_cen, atu = atu)
})

# === conditional flag auto-set ===
test_that("dml() auto-sets conditional = TRUE for a single npm att/atu target", {
  expect_true(setup_cond$att_unc$info$conditional)
  expect_true(setup_cond$atu$info$conditional)
})

# === correct result slot populated ===
test_that("conditional ATT populates the 'treat' slot and not 'untr'", {
  main <- setup_cond$att_unc$results$main
  expect_false(is.null(main$treat))
  expect_null(main$untr)
})

test_that("conditional ATU populates the 'untr' slot and not 'treat'", {
  main <- setup_cond$atu$results$main
  expect_false(is.null(main$untr))
  expect_null(main$treat)
})

# === short parameters ===
test_that("conditional ATT produces finite short parameters with positive SEs", {
  est <- setup_cond$att_unc$results$main$treat[[1]]$estimates
  expect_true(is.finite(est$theta.s))
  expect_true(is.finite(est$sigma2.s) && est$sigma2.s > 0)
  expect_true(is.finite(est$nu2.s))
  expect_true(est$se.theta.s > 0)
})

# === centered vs uncentered parameterization ===
test_that("centered leaves theta.s and sigma2.s unchanged and shifts nu2.s by exactly 1", {
  u <- setup_cond$att_unc$results$main$treat[[1]]$estimates
  cc <- setup_cond$att_cen$results$main$treat[[1]]$estimates
  # shared cross-fit: the point estimate and outcome variance are identical
  expect_equal(u$theta.s, cc$theta.s)
  expect_equal(u$sigma2.s, cc$sigma2.s)
  # uncentered nu2 (chi^2 + 1) is exactly one larger than centered (chi^2)
  expect_equal(u$nu2.s - cc$nu2.s, 1)
})

# === downstream sensitivity runs on a conditional fit ===
test_that("dml_bounds, robustness_value and extreme_robustness_value run on conditional ATT", {
  fit <- setup_cond$att_unc
  b <- dml_bounds(fit, cf.y = 0.04, cf.d = 0.03)
  expect_s3_class(b, "dml.bounds")
  rv  <- robustness_value(fit, alpha = 0.05)
  xrv <- extreme_robustness_value(fit, alpha = 0.05)
  expect_true(all(rv >= 0 & rv <= 1))
  expect_true(all(xrv >= 0 & xrv <= 1))
})

# === conditional is disabled (with a warning) outside its supported case ===
test_that("conditional = TRUE is disabled with a warning for plm", {
  data("pension", package = "dml.sensemakr")
  set.seed(11)
  idx <- sample(nrow(pension), 200)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc, data = pension[idx, ])
  expect_warning(
    fit <- dml(y, d, x, model = "plm", conditional = TRUE,
               cf.folds = 2, cf.reps = 1, verbose = FALSE),
    "only supported for model = 'npm'"
  )
  expect_false(fit$info$conditional)
})
