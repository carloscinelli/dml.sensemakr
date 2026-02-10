# Test DML NPM fitting on a small dataset for speed.

library(testthat)
library(dml.sensemakr)

# Fit once and reuse across tests (small dataset, minimal settings)
setup_npm <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(99)
  idx <- sample(nrow(pension), 500)  # small subset for speed
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  g <- cut(x[, "inc"], quantile(x[, "inc"], c(0, 0.5, 1), na.rm = TRUE),
           labels = c("low", "high"), include.lowest = TRUE)
  fit <- dml(y, d, x, model = "npm", groups = g, cf.folds = 2, cf.reps = 2, verbose = FALSE)
  list(fit = fit, y = y, d = d, x = x, groups = g)
})

# === dml() NPM structure ===
test_that("dml() NPM returns correct structure", {
  fit <- setup_npm$fit
  expect_s3_class(fit, "dml")
  expect_equal(fit$info$model, "npm")
})

test_that("dml() NPM fits have yhat0 and yhat1 predictions", {
  fit <- setup_npm$fit
  for (i in 1:2) {
    preds <- fit$fits[[i]]$preds
    expect_true(all(c("dhat", "yhat0", "yhat1", "phat") %in% names(preds)))
    expect_length(preds$dhat, length(setup_npm$y))
    expect_length(preds$yhat0, length(setup_npm$y))
    expect_length(preds$yhat1, length(setup_npm$y))
  }
})

# === Group results ===
test_that("dml() with groups produces group coefficients", {
  fit <- setup_npm$fit
  expect_true(!is.null(fit$coefs$groups))
  expect_true(length(fit$coefs$groups) > 0)
})

test_that("coef.dml returns both ATE and GATE", {
  cf <- coef(setup_npm$fit)
  expect_true(any(grepl("^ate", names(cf))))
  expect_true(any(grepl("^gate", names(cf))))
})

test_that("se.dml returns SEs for ATE and GATE", {
  s <- se(setup_npm$fit)
  expect_true(any(grepl("^ate", names(s))))
  expect_true(any(grepl("^gate", names(s))))
  expect_true(all(s > 0))
})

test_that("confint.dml with groups has correct number of rows", {
  ci <- confint(setup_npm$fit)
  cf <- coef(setup_npm$fit)
  expect_equal(nrow(ci), length(cf))
})

# === DML bounds with groups ===
test_that("dml_bounds with groups produces group bounds", {
  bounds <- dml_bounds(setup_npm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expect_s3_class(bounds, "dml.bounds")
  expect_true(!is.null(bounds$coefs$groups))
})

test_that("coef.dml.bounds with groups returns matrix with GATE rows", {
  bounds <- dml_bounds(setup_npm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  cf <- coef(bounds)
  expect_true(is.matrix(cf))
  expect_true(any(grepl("^gate", colnames(cf))))
  expect_true(any(grepl("^ate", colnames(cf))))
})

# === confidence_bounds with groups ===
test_that("confidence_bounds with groups returns multi-row matrix", {
  bounds <- dml_bounds(setup_npm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  cb <- confidence_bounds(bounds)
  expect_true(nrow(cb) > 1)
  # Check that bounds exist (may have NaN with small samples, so just check structure)
  expect_equal(ncol(cb), 2)
  expect_equal(colnames(cb), c("lwr", "upr"))
})

# === sensitivity analysis with groups ===
test_that("sensemakr with groups works correctly", {
  sens <- sensemakr(setup_npm$fit, cf.y = 0.04, cf.d = 0.03)
  expect_s3_class(sens, "dml.sensemakr")
  expect_output(print(summary(sens)), "Sensitivity Analysis")
})

# === plotting ===
test_that("plot.dml does not error for NPM", {
  expect_silent(plot(setup_npm$fit))
})

test_that("plot.dml.bounds does not error", {
  bounds <- dml_bounds(setup_npm$fit, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expect_silent(plot(bounds))
})

test_that("coef_plot returns a ggplot object", {
  # coef_plot takes numeric vectors, not a dml object
  cf <- coef(setup_npm$fit)
  ci <- confint(setup_npm$fit)
  p <- coef_plot(estimate = cf, lwr1 = ci[, 1], upr1 = ci[, 2],
                 labels = names(cf))
  expect_true(inherits(p, "gg") || inherits(p, "ggplot"))
})

test_that("ovb_contour_plot does not error", {
  expect_no_error(ovb_contour_plot(setup_npm$fit))
})
