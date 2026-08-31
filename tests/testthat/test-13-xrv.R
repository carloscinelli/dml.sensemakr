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

# === alpha = 1 endpoint crossing ===
test_that("extreme_robustness_value at alpha = 1 uses the aggregated endpoint", {
  fit <- setup_xrv$fit
  xrv1 <- extreme_robustness_value(fit, alpha = 1, theta = 0)
  results <- fit$results$main$all
  statistic <- function(name) {
    vapply(results, function(result) result$estimates[[name]], numeric(1))
  }
  endpoint <- function(factor) {
    confidence_bounds(
      theta.s = statistic("theta.s"),
      S2 = statistic("S2"),
      se.theta.s = statistic("se.theta.s"),
      se.S2 = statistic("se.S2"),
      cov.theta.S2 = statistic("cov.theta.S2"),
      cf.y = 1,
      cf.d = factor^2 / (1 + factor^2),
      level = 0,
      combine.method = "median",
      max = FALSE
    )[["lwr"]]
  }
  upper <- 1
  while (endpoint(upper) > 0) upper <- 2 * upper
  required_factor <- uniroot(endpoint, c(0, upper), tol = 1e-12)$root
  expected <- required_factor^2 / (1 + required_factor^2)
  expect_equal(unname(xrv1[["ate"]]), expected, tolerance = 1e-9)
})

# === zero when the confidence bound already includes theta ===
test_that("extreme_robustness_value is ~0 when the confidence bound includes theta", {
  fit <- setup_xrv$fit
  ci <- confint(fit)
  theta_inside <- mean(ci[1, ])          # midpoint of the CI
  xrv <- extreme_robustness_value(fit, theta = theta_inside, alpha = 0.05)
  expect_equal(unname(xrv[[1]]), 0, tolerance = 1e-4)
})
