# Test helper/internal functions
# These tests are fast and test individual components in isolation.

library(testthat)
library(dml.sensemakr)

# === psi.sd ===
test_that("psi.sd computes standard error from influence function", {
  psi <- rnorm(100, sd = 2)
  expected <- sqrt(mean(psi^2)) / sqrt(100)
  expect_equal(dml.sensemakr:::psi.sd(psi), expected)
})

test_that("psi.sd returns zero for zero influence function", {
  psi <- rep(0, 50)
  expect_equal(dml.sensemakr:::psi.sd(psi), 0)
})

# === trim.ps ===
test_that("trim.ps clips propensity scores correctly with numeric trim", {
  ps <- c(0.001, 0.01, 0.5, 0.99, 0.999)
  result <- dml.sensemakr:::trim.ps(ps, trim = 0.02)
  expect_equal(result$ps, c(0.02, 0.02, 0.5, 0.98, 0.98))
  expect_equal(result$trim$lower, 0.02)
  expect_equal(result$trim$upper, 0.98)
})

test_that("trim.ps clips propensity scores correctly with list trim", {
  ps <- c(0.01, 0.1, 0.5, 0.9, 0.99)
  result <- dml.sensemakr:::trim.ps(ps, trim = list(lower = 0.05, upper = 0.95))
  expect_equal(result$ps, c(0.05, 0.1, 0.5, 0.9, 0.95))
})

test_that("trim.ps tracks trimmed indices correctly", {
  ps <- c(0.001, 0.5, 0.999)
  result <- dml.sensemakr:::trim.ps(ps, trim = 0.02)
  expect_equal(result$trimmed_indices$low, 1)
  expect_equal(result$trimmed_indices$high, 3)
  expect_equal(result$trimmed_indices$all, c(1, 3))
  expect_equal(result$trimmed_num$all, 2)
  expect_equal(result$trimmed_prop$all, 2/3)
})

test_that("trim.ps returns no trimmed indices when nothing is trimmed", {
  ps <- c(0.3, 0.5, 0.7)
  result <- dml.sensemakr:::trim.ps(ps, trim = 0.02)
  expect_length(result$trimmed_indices$all, 0)
  expect_equal(result$trimmed_num$all, 0)
})

test_that("trim.ps errors with bad trim input", {
  ps <- c(0.5, 0.5)
  expect_error(dml.sensemakr:::trim.ps(ps, trim = c(0.01, 0.99)),
               "trim")
  expect_error(dml.sensemakr:::trim.ps(ps, trim = list(a = 0.01, b = 0.99)),
               "trim")
})

# === bias.factor ===
test_that("bias.factor computes correct value", {
  # bias.factor = sqrt(rho2 * cf.y * cf.d / (1 - cf.d))
  result <- dml.sensemakr:::bias.factor(cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  expected <- sqrt(1 * 0.04 * (0.03 / (1 - 0.03)))
  expect_equal(result, expected)
})

test_that("bias.factor is zero when cf.y is zero", {
  expect_equal(dml.sensemakr:::bias.factor(cf.y = 0, cf.d = 0.03, rho2 = 1), 0)
})

test_that("bias.factor is zero when cf.d is zero", {
  expect_equal(dml.sensemakr:::bias.factor(cf.y = 0.04, cf.d = 0, rho2 = 1), 0)
})

test_that("bias.factor scales with sqrt(rho2)", {
  bf1 <- dml.sensemakr:::bias.factor(cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  bf2 <- dml.sensemakr:::bias.factor(cf.y = 0.04, cf.d = 0.03, rho2 = 0.5)
  expect_equal(bf2, bf1 * sqrt(0.5))
})

# === combine.mean and combine.median ===
test_that("combine.mean computes correct estimate", {
  thetas <- c(1, 2, 3)
  ses <- c(0.1, 0.2, 0.3)
  result <- dml.sensemakr:::combine.mean(thetas, ses)
  expect_equal(result["estimate"], c(estimate = 2))
  expect_true(result["se"] > 0)
  expect_named(result, c("estimate", "se"))
})

test_that("combine.median computes correct estimate", {
  thetas <- c(1, 2, 3)
  ses <- c(0.1, 0.2, 0.3)
  result <- dml.sensemakr:::combine.median(thetas, ses)
  expect_equal(result["estimate"], c(estimate = 2))
  expect_true(result["se"] > 0)
  expect_named(result, c("estimate", "se"))
})

test_that("combine.mean and combine.median agree for single estimate", {
  thetas <- c(5)
  ses <- c(0.5)
  result_mean <- dml.sensemakr:::combine.mean(thetas, ses)
  result_median <- dml.sensemakr:::combine.median(thetas, ses)
  expect_equal(result_mean["estimate"], result_median["estimate"])
  expect_equal(result_mean["se"], result_median["se"])
})

test_that("combine.mean SE incorporates cross-rep variation", {
  # If all estimates agree, SE is just mean of SEs
  thetas <- c(2, 2, 2)
  ses <- c(0.5, 0.5, 0.5)
  result <- dml.sensemakr:::combine.mean(thetas, ses)
  expect_equal(result["se"], c(se = 0.5))

  # If estimates disagree, SE is larger
  thetas2 <- c(1, 2, 3)
  result2 <- dml.sensemakr:::combine.mean(thetas2, ses)
  expect_true(result2["se"] > result["se"])
})

# === r2 ===
test_that("r2 computes correct R-squared", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- obs  # perfect prediction

  expect_equal(dml.sensemakr:::r2(pred, obs), 1)
})

test_that("r2 returns 0 for constant prediction at mean", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- rep(mean(obs), 5)
  expect_equal(dml.sensemakr:::r2(pred, obs), 0)
})

test_that("r2 floors at 0 for terrible predictions", {
  obs <- c(1, 2, 3, 4, 5)
  pred <- c(100, 200, 300, 400, 500)  # terrible
  expect_equal(dml.sensemakr:::r2(pred, obs), 0)
})

# === expand.cmat ===
test_that("expand.cmat adds t-value and p-value columns", {
  # column 1 = estimate, column 2 = se (R fills by column)
  cmat <- cbind(estimate = c(a = 4, b = 6), se = c(a = 2, b = 3))
  result <- dml.sensemakr:::expand.cmat(cmat)
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result), c("estimate", "se", "t.value", "p.value"))
  # t.value = estimate / se
  expect_equal(result["a", "t.value"], 4 / 2)
  expect_equal(result["b", "t.value"], 6 / 3)
  expect_true(inherits(result, "cmat"))
})

# === calc_confint ===
test_that("calc_confint produces correct confidence intervals", {
  cf <- c(ate = 5)
  ses <- c(ate = 1)
  ci <- dml.sensemakr:::calc_confint(cf, ses, level = 0.95)
  z <- qnorm(0.025)
  expect_equal(ci[1, 1], 5 + z)
  expect_equal(ci[1, 2], 5 - z)
})

test_that("calc_confint filters by parm argument", {
  cf <- c(ate = 5, gate.q1 = 3)
  ses <- c(ate = 1, gate.q1 = 2)
  ci <- dml.sensemakr:::calc_confint(cf, ses, parm = "ate", level = 0.95)
  expect_equal(nrow(ci), 1)
  expect_equal(rownames(ci), "ate")
})

# === format_perc ===
test_that("format_perc formats probabilities as percentages", {
  result <- dml.sensemakr:::format_perc(c(0.025, 0.975), 3)
  expect_length(result, 2)
  expect_true(grepl("2.5", result[1]))
  expect_true(grepl("97.5", result[2]))
  expect_true(all(grepl("%", result)))
})

# === check_r2 (local helper in plot.R) ===
test_that("check_r2 validates sensitivity parameters", {
  # Should not error for valid inputs
  expect_silent(dml.sensemakr:::check_r2(0.5, 0.3))
  expect_silent(dml.sensemakr:::check_r2(0, 1))
  expect_silent(dml.sensemakr:::check_r2(NULL, 0.5))
  expect_silent(dml.sensemakr:::check_r2(0.5, NULL))

  # Should error for invalid inputs
  expect_error(dml.sensemakr:::check_r2(-0.1, 0.5))
  expect_error(dml.sensemakr:::check_r2(1.1, 0.5))
  expect_error(dml.sensemakr:::check_r2(0.5, -0.1))
  expect_error(dml.sensemakr:::check_r2(0.5, 1.1))
})

# === num ===
test_that("num converts factor to numeric", {
  f <- factor(c("zero", "one", "zero", "one"))
  result <- dml.sensemakr:::num(f)
  expect_type(result, "double")
})

test_that("num returns numeric unchanged", {
  v <- c(1, 2, 3)
  expect_equal(dml.sensemakr:::num(v), v)
})
