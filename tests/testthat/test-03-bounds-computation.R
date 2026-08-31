# Test bounds computation functions
# These tests use pre-computed short results (no ML fitting) for speed.

library(testthat)
library(dml.sensemakr)

# === bounds() function ===
test_that("bounds() computes correct bias bound and adjusted estimates", {
  # Create synthetic short results
  n <- 100
  set.seed(42)
  short.results <- list(
    estimates = list(
      theta.s = 5.0,
      sigma2.s = 2.0,
      nu2.s = 0.5
    ),
    psis = list(
      psi.theta.s = rnorm(n, sd = 0.1),
      psi.sigma2.s = rnorm(n, sd = 0.1),
      psi.nu2.s = rnorm(n, sd = 0.1)
    )
  )

  result <- dml.sensemakr:::bounds(short.results, cf.y = 0.04, cf.d = 0.03, rho2 = 1)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("psis", "estimates"))

  # Check estimates
  bf <- dml.sensemakr:::bias.factor(cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  S <- sqrt(2.0 * 0.5)
  expected_bias <- S * bf

  expect_equal(result$estimates$theta.s, 5.0)
  expect_equal(result$estimates$bias.bound, expected_bias)
  expect_equal(result$estimates$theta.m, 5.0 - expected_bias)
  expect_equal(result$estimates$theta.p, 5.0 + expected_bias)

  # Lower bound should be less than short estimate

  expect_true(result$estimates$theta.m < result$estimates$theta.s)
  # Upper bound should be greater than short estimate
  expect_true(result$estimates$theta.p > result$estimates$theta.s)

  # Standard errors should be positive
  expect_true(result$estimates$se.theta.s > 0)
  expect_true(result$estimates$se.bias.bound > 0)
  expect_true(result$estimates$se.theta.m > 0)
  expect_true(result$estimates$se.theta.p > 0)
})

test_that("bounds() warns when nu2 is negative", {
  short.results <- list(
    estimates = list(theta.s = 5, sigma2.s = 2, nu2.s = -0.5),
    psis = list(
      psi.theta.s = rnorm(10),
      psi.sigma2.s = rnorm(10),
      psi.nu2.s = rnorm(10)
    )
  )
  expect_warning(
    dml.sensemakr:::bounds(short.results, cf.y = 0.04, cf.d = 0.03),
    "nu\\^2 is negative"
  )
})

test_that("bounds() with zero sensitivity parameters gives zero bias", {
  n <- 50
  set.seed(1)
  short.results <- list(
    estimates = list(theta.s = 3.0, sigma2.s = 1.0, nu2.s = 1.0),
    psis = list(
      psi.theta.s = rnorm(n, sd = 0.1),
      psi.sigma2.s = rnorm(n, sd = 0.1),
      psi.nu2.s = rnorm(n, sd = 0.1)
    )
  )

  result <- dml.sensemakr:::bounds(short.results, cf.y = 0, cf.d = 0.03, rho2 = 1)
  expect_equal(result$estimates$bias.bound, 0)
  expect_equal(result$estimates$theta.m, result$estimates$theta.s)
  expect_equal(result$estimates$theta.p, result$estimates$theta.s)
})


# === ate.plm ===
test_that("ate.plm computes correct ATE in simple case", {
  set.seed(123)
  n <- 1000
  x <- rnorm(n)
  d <- x + rnorm(n)
  y <- 2 * d + x + rnorm(n)  # true ATE = 2

  # Perfect nuisance function knowledge
  yhat <- x  # E[Y|X] = x + 2*E[D|X] = x + 2*x = 3x but in PLM, yhat = E[Y|X]
  dhat <- x  # E[D|X] = x

  result <- dml.sensemakr:::ate.plm(y, d, yhat, dhat)

  # Should recover ATE close to 2
  expect_equal(result$estimates$theta.s, 2, tolerance = 0.15)

  # SE should be positive
  expect_true(result$estimates$se.theta.s > 0)

  # sigma2 and nu2 should be positive
  expect_true(result$estimates$sigma2.s > 0)
  expect_true(result$estimates$nu2.s > 0)

  # Check influence function length
  expect_length(result$psis$psi.theta.s, n)
})

# === ate.npm ===
test_that("ate.npm computes ATE for binary treatment", {
  set.seed(456)
  n <- 500
  x <- rnorm(n)
  dhat <- plogis(x)
  d <- rbinom(n, 1, dhat)
  y1 <- 2 + x + rnorm(n, sd = 0.5)
  y0 <- x + rnorm(n, sd = 0.5)
  y <- ifelse(d == 1, y1, y0)  # true ATE = 2

  # Use oracle nuisance functions
  yhat1 <- 2 + x
  yhat0 <- x
  phat <- mean(d)

  result <- dml.sensemakr:::ate.npm(y, d, parameter = "all",
                                    yhat1 = yhat1, yhat0 = yhat0,
                                    dhat = dhat, phat = phat,
                                    trim = 0.01)

  # Should recover ATE near 2
  expect_equal(result$estimates$theta.s, 2, tolerance = 0.3)

  # SE should be positive
  expect_true(result$estimates$se.theta.s > 0)

  # Check trim summary exists
  expect_type(result$trim.summary, "list")
  expect_true("ps" %in% names(result$trim.summary))
})


# === combine.cross.fits ===
test_that("combine.cross.fits produces correct matrix", {
  # Create mock results similar to what DML produces
  make_result <- function(theta, se) {
    list(estimates = list(theta.s = theta, se.theta.s = se))
  }
  results <- list(make_result(1, 0.1), make_result(2, 0.2), make_result(3, 0.3))

  combined <- dml.sensemakr:::combine.cross.fits(results, param = "theta.s")

  expect_true(is.matrix(combined))
  expect_equal(nrow(combined), 2)
  expect_equal(rownames(combined), c("mean", "median"))
  expect_equal(colnames(combined), c("estimate", "se"))
  expect_equal(combined["mean", "estimate"], 2)
  expect_equal(combined["median", "estimate"], 2)
})

# === confidence_bounds.numeric ===
test_that("confidence_bounds.numeric computes correct bounds", {
  # Simple case with known values
  theta.s <- c(5, 5, 5)
  S2 <- c(1, 1, 1)
  se.theta.s <- c(0.1, 0.1, 0.1)
  se.S2 <- c(0.01, 0.01, 0.01)
  cov.theta.S2 <- c(0, 0, 0)

  result <- confidence_bounds(theta.s, S2,
                              se.theta.s, se.S2,
                              cov.theta.S2,
                              cf.y = 0.04, cf.d = 0.03,
                              rho2 = 1, level = 0.95, max = FALSE)

  expect_named(result, c("lwr", "upr"))
  expect_true(result["lwr"] < result["upr"])
  # Lower bound should be below short estimate
  expect_true(result["lwr"] < 5)
})
