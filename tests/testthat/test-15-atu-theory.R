# Theory tests for the conditional ATU estimator and covariate benchmarking,
# checked against the manuscript's orthogonal scores and gain-metric formulas.
#

# ============================================================================
# (1) ATT <-> ATU symmetry of the internal short-parameter helpers.
#     atu.npm.cond() is the mirror of att.npm.cond() under D -> 1-D, p -> 1-p,
#     pi -> 1-pi, and the fitted arm g0s <-> g1s. So evaluating ATU on flipped
#     inputs must reproduce ATT: theta.s flips sign; sigma2.s and nu2.s are
#     unchanged. This is a fast, exact check (no ML fitting).
# ============================================================================
test_that("atu.npm.cond is the exact mirror of att.npm.cond", {
  withr::local_seed(1)
  n  <- 600
  d  <- rbinom(n, 1, 0.45)
  pi <- runif(n, 0.08, 0.92)       # propensity P(D=1|X)  (inside the trim range)
  p  <- rep(0.45, n)               # marginal P(D=1)
  y  <- rnorm(n, sd = 2)
  g0 <- rnorm(n)                   # an (arbitrary) fitted regression

  att <- dml.sensemakr:::att.npm.cond(y = y, d = d, yhat0 = g0,
                                      dhat = pi, phat = p, trim = 0.01)
  atu <- dml.sensemakr:::atu.npm.cond(y = y, d = 1 - d, yhat1 = g0,
                                      dhat = 1 - pi, phat = 1 - p, trim = 0.01)

  expect_equal(atu$estimates$theta.s, -att$estimates$theta.s)
  expect_equal(atu$estimates$sigma2.s, att$estimates$sigma2.s)
  expect_equal(atu$estimates$nu2.s,    att$estimates$nu2.s)
})

# ---- shared conditional-ATU fits (uncentered and centered, same seed) -------
new_atu_fixture <- memoise::memoise(function() {
  pension <- dml.sensemakr::pension
  withr::local_seed(414)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  unc <- dml(y, d, x, model = "npm", target = "atu", cf.folds = 2, cf.reps = 1,
              cf.seed = 123, centered = FALSE, verbose = FALSE)
  cen <- dml(y, d, x, model = "npm", target = "atu", cf.folds = 2, cf.reps = 1,
              cf.seed = 123, centered = TRUE, verbose = FALSE)
  list(unc = unc, cen = cen, y = y, d = d, x = x)
})

# ============================================================================
# (2) centered vs uncentered for ATU (mirror of the ATT invariant)
# ============================================================================
test_that("centered leaves ATU theta.s/sigma2.s unchanged and shifts nu2.s by 1", {
  atu_unc <- new_atu_fixture()$unc
  atu_cen <- new_atu_fixture()$cen
  u  <- atu_unc$results$main$untr[[1]]$estimates
  cc <- atu_cen$results$main$untr[[1]]$estimates
  expect_equal(u$theta.s,  cc$theta.s)
  expect_equal(u$sigma2.s, cc$sigma2.s)
  expect_equal(u$nu2.s - cc$nu2.s, 1)
})

# ============================================================================
# (3) downstream sensitivity runs on the ATU fit
# ============================================================================
test_that("dml_bounds, robustness_value and extreme_robustness_value run on ATU", {
  atu_unc <- new_atu_fixture()$unc
  b <- dml_bounds(atu_unc, cf.y = 0.04, cf.d = 0.03)
  expect_s3_class(b, "dml.bounds")
  rv  <- robustness_value(atu_unc, alpha = 0.05)
  xrv <- extreme_robustness_value(atu_unc, alpha = 0.05)
  expect_true(all(rv >= 0 & rv <= 1))
  expect_true(all(xrv >= 0 & xrv <= 1))
})

# ============================================================================
# (4) benchmarking on a conditional (ATU) fit, and the gain-metric identities.
#     Reproduce the leave-one-out refit with the same seed so gains can be
#     pinned to G1D,j = (nu2 - nu2_wo)/nu2_wo and G1dY,j = (sig2_wo - sig2)/sig2.
# ============================================================================
test_that("dml_benchmark on a conditional fit matches the gain-metric formulas", {
  atu_unc <- new_atu_fixture()$unc
  y <- new_atu_fixture()$y; d <- new_atu_fixture()$d; x <- new_atu_fixture()$x
  bench <- dml_benchmark(atu_unc, benchmark_covariates = "inc")
  expect_s3_class(bench, "dml_benchmark")

  index.o <- which(colnames(x) == "inc")
  xo <- x[, -index.o, drop = FALSE]
  fit_wo <- dml(y, d, xo, model = "npm", target = "atu", cf.folds = 2, cf.reps = 1,
                cf.seed = 123, centered = FALSE, verbose = FALSE)

  nu    <- dml.sensemakr:::extract_estimate(atu_unc$results$main$untr, "nu2.s")
  nu.wo <- dml.sensemakr:::extract_estimate(fit_wo$results$main$untr,  "nu2.s")
  sg    <- dml.sensemakr:::extract_estimate(atu_unc$results$main$untr, "sigma2.s")
  sg.wo <- dml.sensemakr:::extract_estimate(fit_wo$results$main$untr,  "sigma2.s")

  b <- bench$benchmarks$inc
  expect_equal(b$gain.D, pmax(0, (nu - nu.wo) / nu.wo))
  expect_equal(b$gain.Y, pmax(0, (sg.wo - sg) / sg))
  # delta = theta.s - theta.sj (bias decomposition)
  expect_equal(b$delta, b$theta.s - b$theta.sj)
  # rho is a correlation
  expect_true(all(abs(b$rho) <= 1))
})
