# Test extreme_robustness_value (XRV): the minimum Riesz-representer strength
# (cf.d, with cf.y = 1) needed to bring the confidence bound to the null.

library(testthat)
library(dml.sensemakr)

setup_xrv <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(808)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 2, cf.seed = 123, verbose = FALSE)
  list(fit = fit)
})

# === structure and range ===
test_that("extreme_robustness_value returns a named numeric in [0, 1]", {
  xrv <- extreme_robustness_value(setup_xrv$fit, alpha = 0.05)
  expect_type(xrv, "double")
  expect_true(!is.null(names(xrv)))
  expect_true(all(xrv >= 0 & xrv <= 1))
})

# === alpha = 1 closed form ===
test_that("extreme_robustness_value at alpha = 1 matches the closed form f0^2 / (1 + f0^2)", {
  fit <- setup_xrv$fit
  xrv1 <- extreme_robustness_value(fit, alpha = 1, theta = 0)
  # closed form: f0 = |theta - theta.s| / S,  XRV = f0^2 / (1 + f0^2)
  S2 <- stats::median(sapply(fit$results$main$all, function(z) z$estimates$S2))
  f0 <- abs(0 - coef(fit)[["ate"]]) / sqrt(S2)
  expect_equal(unname(xrv1[["ate"]]), f0^2 / (1 + f0^2), tolerance = 1e-6)
})

# === zero when the confidence bound already includes theta ===
test_that("extreme_robustness_value is ~0 when the confidence bound includes theta", {
  fit <- setup_xrv$fit
  ci <- confint(fit)
  theta_inside <- mean(ci[1, ])          # midpoint of the CI
  xrv <- extreme_robustness_value(fit, theta = theta_inside, alpha = 0.05)
  expect_equal(unname(xrv[[1]]), 0, tolerance = 1e-4)
})
