# Sign convention for the benchmark alignment rho.
#
# For the ATE, ATU, and the *unconditional* ATT the leave-one-out
# decomposition is delta = theta.s - theta.s,-j = +rho * M, so the reported
# rho carries the sign of delta (align.sign = -1). Benchmarking the *same*
# confounder must therefore return the same rho sign across these estimands.
#
# The conditional (single-arm / DiD-style) ATT imputes the subtracted
# counterfactual and mirrors the conditional ATU, giving delta = -rho * M, so
# its reported rho keeps the opposite sign (align.sign = +1).
#

# A confounder X2 that raises both the outcome and the propensity, so it has a
# single, well-defined alignment shared by every estimand.
new_sign_data <- memoise::memoise(function() {
  withr::local_seed(10)
  n <- 4000
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  D <- rbinom(n, 1, plogis(0.8 * X1 + 0.9 * X2))
  list(D = D, Y = 5 * D + 2 * X1 + 2.5 * X2 + rnorm(n),
       Xm = cbind(X1 = X1, X2 = X2))
})

rho_of <- function(target, conditional) {
  dat <- new_sign_data()
  D <- dat$D; Y <- dat$Y; Xm <- dat$Xm
  cl  <- bquote(dml(Y, D, Xm, model = "npm", target = .(target),
                    conditional = .(conditional), cf.folds = 5, cf.reps = 1,
                    cf.seed = 1, verbose = FALSE, yreg = "lm",
                    dreg = list(method = "glm", family = binomial())))
  fit <- eval(cl, environment())
  benchmark <- dml_benchmark(fit, "X2")$benchmarks$X2
  # The sign convention concerns positive gains. Correctly specified learners
  # keep this fixture away from the zero-gain case created by binding floors.
  expect_gt(benchmark$gain.Y, 0)
  expect_gt(benchmark$gain.D, 0)
  benchmark$rho
}

test_that("unconditional ATT rho aligns with ATE and ATU", {
  r_ate <- rho_of("ate", FALSE)
  r_att <- rho_of("att", FALSE)
  r_atu <- rho_of("atu", FALSE)
  expect_identical(sign(r_att), sign(r_ate))
  expect_identical(sign(r_atu), sign(r_ate))
})

test_that("conditional ATT keeps the opposite (DiD-mirror) rho sign", {
  r_att_u <- rho_of("att", FALSE)
  r_att_c <- rho_of("att", TRUE)
  expect_false(identical(sign(r_att_c), sign(r_att_u)))
})
