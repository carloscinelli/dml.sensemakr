# Test DML PLM fitting on a small dataset for speed
# Uses a small subset of pension data with minimal folds/reps.

library(testthat)
library(dml.sensemakr)

# Fit once and reuse across tests (small dataset, minimal settings)
setup_plm <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(42)
  idx <- sample(nrow(pension), 500)  # small subset for speed
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 2, verbose = FALSE)
  list(fit = fit, y = y, d = d, x = x)
})

# === dml() object structure ===
test_that("dml() returns correct class and structure for PLM", {
  fit <- setup_plm$fit
  expect_s3_class(fit, "dml")
  expect_true(all(c("data", "call", "info", "fits", "results", "coefs") %in% names(fit)))
  expect_equal(fit$info$model, "plm")
  expect_equal(fit$info$target, "ate")
  expect_equal(fit$info$cf.folds, 2)
  expect_equal(fit$info$cf.reps, 2)
})

test_that("dml() stores data correctly", {
  fit <- setup_plm$fit
  expect_equal(fit$data$y, setup_plm$y)
  expect_equal(fit$data$d, setup_plm$d)
  expect_equal(fit$data$x, setup_plm$x)
})

test_that("dml() PLM fits contain predictions", {
  fit <- setup_plm$fit
  expect_length(fit$fits, 2)  # cf.reps = 2
  for (i in 1:2) {
    preds <- fit$fits[[i]]$preds
    expect_true(all(c("dhat", "yhat") %in% names(preds)))
    expect_length(preds$dhat, length(setup_plm$y))
    expect_length(preds$yhat, length(setup_plm$y))
  }
})

test_that("dml() PLM produces main coefficients", {
  fit <- setup_plm$fit
  expect_true(!is.null(fit$coefs$main))
  # Main coefs are keyed by target (e.g., "all" for ATE)
  expect_true(length(fit$coefs$main) > 0)
  # Each element should be a 2x2 matrix (mean/median x estimate/se)
  first_coef <- fit$coefs$main[[1]]
  expect_true(is.matrix(first_coef))
  expect_equal(nrow(first_coef), 2)
  expect_equal(rownames(first_coef), c("mean", "median"))
  expect_equal(colnames(first_coef), c("estimate", "se"))
})

# === coef, se, confint ===
test_that("coef.dml returns named vector", {
  cf <- coef(setup_plm$fit)
  expect_type(cf, "double")
  expect_true(length(cf) > 0)
  # PLM with target="ate" produces names like "ate.all"
  expect_true(any(grepl("^ate", names(cf))))
})

test_that("coef.dml works with mean and median methods", {
  cf_median <- coef(setup_plm$fit, combine.method = "median")
  cf_mean <- coef(setup_plm$fit, combine.method = "mean")
  expect_type(cf_median, "double")
  expect_type(cf_mean, "double")
  expect_equal(length(cf_median), length(cf_mean))
})

test_that("se.dml returns positive standard errors", {
  s <- se(setup_plm$fit)
  expect_type(s, "double")
  expect_true(all(s > 0))
  expect_equal(names(s), names(coef(setup_plm$fit)))
})

test_that("confint.dml returns matrix with correct dimensions", {
  ci <- confint(setup_plm$fit, level = 0.95)
  expect_true(is.matrix(ci))
  expect_equal(ncol(ci), 2)
  # Lower bound should be less than upper bound
  expect_true(all(ci[, 1] < ci[, 2]))
})

test_that("confint.dml respects level parameter", {
  ci_95 <- confint(setup_plm$fit, level = 0.95)
  ci_99 <- confint(setup_plm$fit, level = 0.99)
  # 99% CI should be wider than 95%
  width_95 <- ci_95[1, 2] - ci_95[1, 1]
  width_99 <- ci_99[1, 2] - ci_99[1, 1]
  expect_true(width_99 > width_95)
})

# === summary ===
test_that("summary.dml returns correct class", {
  s <- summary(setup_plm$fit)
  expect_s3_class(s, "summary_dml")
})

test_that("print.dml does not error", {
  expect_output(print(setup_plm$fit), "Debiased Machine Learning")
})

test_that("print.summary_dml does not error", {
  expect_output(print(summary(setup_plm$fit)), "Debiased Machine Learning")
})

# === dml_bounds ===
test_that("dml_bounds returns correct class and structure", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expect_s3_class(bounds, "dml.bounds")
  expect_equal(bounds$info$cf.y, 0.04)
  expect_equal(bounds$info$cf.d, 0.03)
  expect_equal(bounds$info$rho2, 1)
  expect_true(!is.null(bounds$coefs$main))
  expect_true(!is.null(bounds$dml.fit))
})

test_that("coef.dml.bounds returns matrix", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  cf <- coef(bounds)
  expect_true(is.matrix(cf))
  expect_true(nrow(cf) > 0)
  expect_true("theta.s" %in% rownames(cf))
  expect_true("bias.bound" %in% rownames(cf))
  expect_true("theta.m" %in% rownames(cf))
  expect_true("theta.p" %in% rownames(cf))
})

test_that("se.dml.bounds returns matrix with positive values", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  s <- se(bounds)
  expect_true(is.matrix(s))
  expect_true(all(s > 0))
})

test_that("confint.dml.bounds returns a list", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  ci <- confint(bounds)
  expect_type(ci, "list")
  expect_true(length(ci) > 0)
})

test_that("summary.dml.bounds does not error", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expect_output(print(summary(bounds)), "Debiased Machine Learning")
})

# === confidence_bounds on dml object ===
test_that("confidence_bounds.dml returns correct structure", {
  cb <- confidence_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expect_s3_class(cb, "confidence.bounds")
  expect_true(is.matrix(cb))
  expect_equal(ncol(cb), 2)
  expect_equal(colnames(cb), c("lwr", "upr"))
  expect_true(all(cb[, "lwr"] < cb[, "upr"]))
})

# === confidence_bounds on dml.bounds object ===
test_that("confidence_bounds.dml.bounds returns correct structure", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  cb <- confidence_bounds(bounds)
  expect_s3_class(cb, "confidence.bounds")
  expect_true(is.matrix(cb))
  expect_equal(ncol(cb), 2)
  expect_true(all(cb[, "lwr"] < cb[, "upr"]))
})

# === robustness_value ===
test_that("robustness_value.dml returns named numeric", {
  rv <- robustness_value(setup_plm$fit)
  expect_type(rv, "double")
  expect_true(length(rv) > 0)
  expect_true(all(rv >= 0 & rv <= 1))
})

test_that("robustness_value.dml.bounds returns named numeric", {
  bounds <- dml_bounds(setup_plm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  rv <- robustness_value(bounds)
  expect_type(rv, "double")
  expect_true(length(rv) > 0)
  expect_true(all(rv >= 0 & rv <= 1))
})

# === sensemakr ===
test_that("sensemakr.dml returns correct class", {
  sens <- sensemakr(setup_plm$fit, cf.y = 0.04, cf.d = 0.03)
  expect_s3_class(sens, "dml.sensemakr")
  expect_true(!is.null(sens$info))
  expect_true(!is.null(sens$model))
  expect_true(!is.null(sens$sensitivity_stats))
  expect_true(!is.null(sens$conf.bounds))
})

test_that("print.dml.sensemakr does not error", {
  sens <- sensemakr(setup_plm$fit, cf.y = 0.04, cf.d = 0.03)
  expect_output(print(sens), "Sensitivity Analysis")
})

test_that("summary.dml.sensemakr does not error", {
  sens <- sensemakr(setup_plm$fit, cf.y = 0.04, cf.d = 0.03)
  expect_output(print(summary(sens)), "Sensitivity Analysis")
})
