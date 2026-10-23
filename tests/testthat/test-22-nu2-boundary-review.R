# Mathematical boundary identities and regressions from the nu2 floor review.
# These fixtures exercise scores directly, without fitting ML learners.

test_that("binary boundary scores equal the empirical-floor influence function", {
  for (nt in c(3, 5, 7)) {
    d <- c(rep(1, nt), rep(0, 10 - nt))
    p <- mean(d)
    y <- seq_along(d) / 10
    floor <- 1 / (p * (1 - p))
    psi.floor <- (2 * p - 1) * (d - p) / (p^2 * (1 - p)^2)
    for (target in c("all", "treat", "untr")) {
      fit <- dml.sensemakr:::ate.npm(y, d, target, y + 1, y - 1,
                                    dhat = rep(p, length(d)), phat = p)
      expect_equal(fit$estimates$nu2.s, floor)
      expect_equal(fit$psis$psi.nu2.s, psi.floor, tolerance = 1e-12)
    }
    for (centered in c(FALSE, TRUE)) {
      att <- dml.sensemakr:::att.npm.cond(y, d, rep(0, 10),
        dhat = rep(p, 10), phat = p, centered = centered)
      atu <- dml.sensemakr:::atu.npm.cond(y, d, rep(0, 10),
        dhat = rep(p, 10), phat = p, centered = centered)
      for (fit in list(att, atu)) {
        expect_equal(fit$estimates$nu2.s, if (centered) 0 else 1,
                     tolerance = 1e-12)
        expect_equal(fit$psis$psi.nu2.s, rep(0, 10), tolerance = 1e-12)
      }
    }
  }
})

test_that("PLM boundary scores include variation of the estimated variance floor", {
  for (d in list(c(0, 0, 0, 1, 1), c(-3, -1, 0, 2, 4))) {
    v <- mean((d - mean(d))^2)
    fit <- dml.sensemakr:::ate.plm(seq_along(d), d,
                                  yhat = rep(0, 5), dhat = rep(mean(d), 5))
    expect_equal(fit$estimates$nu2.s, 1 / v)
    expect_equal(fit$psis$psi.nu2.s, (v - (d - mean(d))^2) / v^2)
  }
})

test_that("active propensity clipping propagates the empirical-floor uncertainty", {
  # Here the input propensity is correct, but clipping at .02 makes the raw
  # ATE score converge below its population floor. The returned estimator
  # equals the sample-share floor, whose variance must be propagated.
  d <- c(rep(1, 100), rep(0, 9900))
  y <- rep(c(-1, 1), 5000)
  p <- mean(d)
  q <- 0.02
  n <- length(d)
  floor <- 1 / (p * (1 - p))
  fit <- dml.sensemakr:::ate.npm(y, d, "all", rep(0, n), rep(0, n),
                                dhat = rep(p, n), phat = p, trim = q)
  psi.floor <- (2 * p - 1) * (d - p) / (p^2 * (1 - p)^2)
  expect_equal(fit$estimates$nu2.s, floor)
  expect_equal(fit$psis$psi.nu2.s, psi.floor)
  expect_equal(fit$estimates$se.nu2.s, sqrt(mean(psi.floor^2) / n))

  # Unit outcome residual variance and zero covariance give a closed-form
  # endpoint SE. The former retained score understated it by a factor .269.
  b <- dml.sensemakr:::bounds(fit, cf.y = 0.5, cf.d = 2 / 3)$estimates
  var.theta <- p / q^2 + (1 - p) / (1 - q)^2
  var.scale <- (1 - 2 * p)^2 / (4 * p^2 * (1 - p)^2)
  expect_equal(b$se.theta.m, sqrt((var.theta + var.scale) / n))
  expect_equal(b$se.theta.p, sqrt((var.theta + var.scale) / n))
})

test_that("centered conditional scores do not cross the floor through cancellation", {
  # With a constant fitted odds ratio r and phat equal to the sample share,
  # the exact centered score is 2*r - r^2 - 1 = -(r - 1)^2 <= 0.
  # The former expanded polynomial gave +1.879e-12 in this example.
  d <- c(rep(1, 97), rep(0, 3))
  p <- mean(d)
  pi <- rep(p + 1e-8, length(d))
  y <- seq_along(d) / length(d)
  att <- dml.sensemakr:::att.npm.cond(y, d, rep(0, length(d)),
                                    dhat = pi, phat = p, centered = TRUE)
  atu <- dml.sensemakr:::atu.npm.cond(y, 1 - d, rep(0, length(d)),
                                    dhat = 1 - pi, phat = 1 - p, centered = TRUE)
  for (fit in list(att, atu)) {
    expect_identical(fit$estimates$nu2.s, 0)
    expect_identical(fit$estimates$S2, 0)
    expect_equal(fit$psis$psi.nu2.s, numeric(length(d)))
    expect_identical(dml.sensemakr:::bounds(fit)$estimates$se.bias.bound, NA_real_)
  }
})

test_that("stable centered scores preserve positive values and the uncentered identity", {
  d <- rep(c(0, 1), each = 10)
  y <- seq_along(d)
  # A positive score far below a conventional numerical tolerance must remain
  # positive; the numerical fix is an identity, not a thresholding rule.
  pi <- 0.5 + 1e-12 * (2 * d - 1)
  centered <- dml.sensemakr:::att.npm.cond(y, d, rep(0, 20),
    dhat = pi, phat = 0.5, centered = TRUE)
  expect_gt(centered$estimates$nu2.s, 0)
  expect_lt(centered$estimates$nu2.s, 1e-9)

  # Nonconstant, fold-specific shares also obey the same score identity.
  pi <- rep(c(0.2, 0.8), each = 10)
  d <- c(rep(1, 2), rep(0, 8), rep(1, 8), rep(0, 2))
  p <- rep(c(0.45, 0.55), each = 10)
  for (flip in c(FALSE, TRUE)) {
    dd <- if (flip) 1 - d else d
    pp <- if (flip) 1 - pi else pi
    phat <- if (flip) 1 - p else p
    unc <- dml.sensemakr:::att.npm.cond(y, dd, rep(0, 20),
      dhat = pp, phat = phat, centered = FALSE)
    cen <- dml.sensemakr:::att.npm.cond(y, dd, rep(0, 20),
      dhat = pp, phat = phat, centered = TRUE)
    expect_equal(unc$estimates$nu2.s - cen$estimates$nu2.s, 1)
    expect_equal(unc$psis$psi.nu2.s, cen$psis$psi.nu2.s)
  }
})

test_that("conditional parameterizations select the same floor before adding one", {
  d <- rep(c(0, 1), each = 10)
  y <- seq_along(d)
  # The exact raw centered value is negative but small enough that adding one
  # can round to one. It must still select the same zero IF in both versions.
  pi <- rep(0.5 + 1e-9, 20)
  for (centered in c(FALSE, TRUE)) {
    att <- dml.sensemakr:::att.npm.cond(y, d, rep(0, 20),
      dhat = pi, phat = 0.5, centered = centered)
    atu <- dml.sensemakr:::atu.npm.cond(y, 1 - d, rep(0, 20),
      dhat = 1 - pi, phat = 0.5, centered = centered)
    for (fit in list(att, atu)) {
      expect_identical(fit$estimates$nu2.s, if (centered) 0 else 1)
      expect_identical(fit$psis$psi.nu2.s, numeric(20))
    }
  }
})

test_that("numeric confidence bounds retain effect uncertainty at zero confounding", {
  for (method in c("mean", "median")) {
    theta <- c(1.8, 2.0, 2.3)
    se <- c(0.2, 0.25, 0.3)
    combine <- if (method == "mean") dml.sensemakr:::combine.mean else
                                      dml.sensemakr:::combine.median
    combined <- combine(theta, se)
    expected <- unname(combined["estimate"] + c(-1, 1) *
                         qnorm(0.95) * combined["se"])
    for (args in list(list(cf.y = 0, cf.d = 0.03),
                     list(cf.y = 0.04, cf.d = 0),
                     list(cf.y = 0.04, cf.d = 0.03, rho2 = 0))) {
      actual <- do.call(confidence_bounds, c(list(theta.s = theta,
        S2 = c(0, 2, 0), se.theta.s = se,
        se.S2 = c(0, 0.1, NA_real_), cov.theta.S2 = c(0, 0, NA_real_),
        combine.method = method), args))
      expect_equal(unname(actual), expected)
    }
  }
})

test_that("numeric and score confidence bounds agree at positive and zero scales", {
  d <- rep(c(0, 1), each = 5)
  y <- c(-2, -1, 0, 1, 2, 0, 1, 2, 3, 4)
  for (centered in c(FALSE, TRUE)) {
    fit <- dml.sensemakr:::att.npm.cond(y, d, rep(0, 10),
      dhat = rep(0.5, 10), phat = 0.5, centered = centered)
    e <- fit$estimates
    for (cf.y in c(0, 0.04)) {
      b <- dml.sensemakr:::bounds(fit, cf.y = cf.y, cf.d = 0.03)$estimates
      actual <- confidence_bounds(e$theta.s, S2 = e$S2,
        se.theta.s = e$se.theta.s, se.S2 = e$se.S2,
        cov.theta.S2 = e$cov.theta.S2, cf.y = cf.y, cf.d = 0.03)
      expected <- c(b$theta.m - qnorm(0.95) * b$se.theta.m,
                    b$theta.p + qnorm(0.95) * b$se.theta.p)
      expect_equal(unname(actual), expected)
    }
  }
})

test_that("explicit zero benchmarking strength bypasses undefined relative gains", {
  d <- rep(c(0, 1), each = 5)
  y <- seq_along(d)
  fit <- dml.sensemakr:::att.npm.cond(y, d, rep(0, 10),
    dhat = rep(0.5, 10), phat = 0.5, centered = TRUE)
  model <- structure(list(info = list(target = "att"),
    results = list(main = list(treat = list(fit)))), class = "dml")
  # A full and reduced centered imbalance both equal to zero give GD = 0/0.
  bench <- structure(list(benchmarks = list(x = list(
    gain.Y = 0.2, gain.D = NaN, rho = 0)), benchmarks_psis = list(x = list(
    psi.theta.s = list(fit$psis$psi.theta.s),
    psi.nu2.s = list(fit$psis$psi.nu2.s),
    psi.sigma2.s = list(fit$psis$psi.sigma2.s),
    psi.GY = list(rep(NA_real_, 10)), psi.GD = list(rep(NA_real_, 10)),
    psi.rho = list(rep(NA_real_, 10))))), class = "dml_benchmark")
  theta <- fit$estimates$theta.s
  se <- fit$estimates$se.theta.s
  for (args in list(list(kY = 0), list(kD = 0), list(rho2 = 0))) {
    expect_silent(b <- do.call(benchmark_bounds,
                              c(list(model = model, benchmark = bench), args)))
    expect_equal(b$BF, 0)
    expect_equal(b$theta.minus, theta)
    expect_equal(b$theta.plus, theta)
    expect_equal(b$se.minus, se)
    expect_equal(b$se.plus, se)
    expect_equal(b$lwr, theta - qnorm(0.95) * se)
    expect_equal(b$upr, theta + qnorm(0.95) * se)
  }
  # An undefined gain is still unavailable for a positive requested strength.
  expect_warning(missing <- benchmark_bounds(model, bench),
                 "not the same as a measured gain")
  expect_true(is.na(missing$BF))

  # Even finite gains can carry unavailable derivative estimates. A fixed
  # zero alignment makes those derivatives irrelevant, without 0 * NA.
  bench$benchmarks$x$gain.D <- 0.3
  expect_silent(zero <- benchmark_bounds(model, bench, rho2 = 0))
  expect_equal(zero$se.minus, se)
  expect_equal(zero$se.plus, se)
})
