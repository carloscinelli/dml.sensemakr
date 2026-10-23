# Check the scale and its uncertainty against the selected score or floor.
expect_nu2_scale <- function(result, nu2, psi.nu2) {
  e <- result$estimates
  psi <- result$psis
  psi.S2 <- e$sigma2.s * psi.nu2 + nu2 * psi$psi.sigma2.s
  expect_equal(e$nu2.s, nu2)
  expect_equal(psi$psi.nu2.s, psi.nu2)
  expect_equal(e$se.nu2.s, sqrt(mean(psi.nu2^2) / length(psi.nu2)))
  expect_equal(e$S2, e$sigma2.s * nu2)
  expect_equal(psi$psi.S2, psi.S2)
  expect_equal(e$se.S2, sqrt(mean(psi.S2^2) / length(psi.S2)))
  expect_equal(e$cov.theta.S2,
               mean(psi$psi.theta.s * psi.S2) / length(psi.S2))
}

test_that("unconditional binary floors propagate their sample share", {
  d <- c(rep(0, 9), rep(1, 3))
  y <- seq_along(d) / 3
  pi <- ifelse(d == 0, 0.85, 0.15)
  # Cross-fitted shares deliberately differ from the full-sample share.
  p <- rep(c(0.2, 0.3), each = 6)
  floor <- 1 / (mean(d) * (1 - mean(d)))
  for (target in c("all", "treat", "untr")) {
    l <- switch(target, all = rep(1, length(d)), treat = d / p,
                untr = (1 - d) / (1 - p))
    r <- switch(target, all = d / pi - (1 - d) / (1 - pi),
                treat = d / p - (1 - d) * pi / ((1 - pi) * p),
                untr = d * (1 - pi) / (pi * (1 - p)) - (1 - d) / (1 - p))
    m <- switch(target, all = 1 / (pi * (1 - pi)),
                treat = d / (p^2 * (1 - pi)),
                untr = (1 - d) / ((1 - p)^2 * pi))
    raw <- mean(2 * m - r^2) / mean(2 * l - 1)
    psi <- (2 * m - r^2 - (2 * l - 1) * raw) / mean(2 * l - 1)
    expect_lt(raw, floor)
    fit <- dml.sensemakr:::ate.npm(y, d, target, y + 1, y - 1, pi, p)
    p.sample <- mean(d)
    psi.floor <- (2 * p.sample - 1) * (d - p.sample) /
      (p.sample^2 * (1 - p.sample)^2)
    expect_nu2_scale(fit, floor, psi.floor)
    b <- dml.sensemakr:::bounds(fit)
    expect_true(all(is.finite(unlist(b$estimates))))
    expect_equal(b$estimates$bias.bound,
                 sqrt(fit$estimates$S2) * dml.sensemakr:::bias.factor(0.04, 0.03))
  }
})

test_that("conditional constant floors have zero scores and preserve symmetry", {
  d <- c(rep(0, 9), rep(1, 3))
  y <- seq_along(d) / 3
  pi <- ifelse(d == 0, 0.85, 0.15)
  for (flip in c(FALSE, TRUE)) {
    dd <- if (flip) 1 - d else d
    pp <- if (flip) 1 - pi else pi
    p <- mean(dd)
    l <- dd / p
    lc <- (1 - dd) / (1 - p)
    ratio <- (pp / (1 - pp)) / (p / (1 - p))
    raw <- mean(2 * l * ratio - lc * ratio^2)
    psi <- 2 * l * ratio - lc * ratio^2 - (2 * l - lc) * raw
    expect_lt(raw, 1)
    for (centered in c(FALSE, TRUE)) {
      att <- dml.sensemakr:::att.npm.cond(y, dd, yhat0 = y / 2,
                                         dhat = pp, phat = p, centered = centered)
      atu <- dml.sensemakr:::atu.npm.cond(y, 1 - dd, yhat1 = y / 2,
                                         dhat = 1 - pp, phat = 1 - p,
                                         centered = centered)
      expect_nu2_scale(att, if (centered) 0 else 1, numeric(length(d)))
      expect_nu2_scale(atu, if (centered) 0 else 1, numeric(length(d)))
      expect_equal(atu$estimates$theta.s, -att$estimates$theta.s)
    }
  }
})

test_that("PLM floors use empirical treatment variance with divisor n", {
  for (d in list(c(0, 0, 0, 1, 1, 1), c(-3, -1, 0, 1, 2, 4))) {
    y <- c(1, -1, 3, 2, -2, 1)
    dhat <- d + seq(5, 10)
    raw <- 1 / mean((d - dhat)^2)
    psi <- raw - (d - dhat)^2 * raw^2
    floor <- 1 / mean((d - mean(d))^2)
    fit <- dml.sensemakr:::ate.plm(y, d, rep(0, length(d)), dhat)
    expect_lt(raw, floor)
    psi.floor <- floor - (d - mean(d))^2 * floor^2
    expect_nu2_scale(fit, floor, psi.floor)
    # Only the sensitivity scale changes, not the treatment effect.
    expect_equal(fit$estimates$theta.s, mean(y * (d - dhat)) / mean((d - dhat)^2))
  }
})

test_that("estimates above their floors are unchanged", {
  # Two strata with exact treatment proportions and p = 1/2.
  pi <- rep(c(0.2, 0.8), each = 10)
  d <- c(rep(1, 2), rep(0, 8), rep(1, 8), rep(0, 2))
  y <- seq_along(d) / 3
  for (target in c("all", "treat", "untr")) {
    fit <- dml.sensemakr:::ate.npm(y, d, target, y + 1, y - 1, pi, 0.5)
    expect_equal(fit$estimates$nu2.s, if (target == "all") 6.25 else 8.5)
    expect_equal(mean(fit$psis$psi.nu2.s), 0, tolerance = 1e-12)
  }
  for (centered in c(FALSE, TRUE)) {
    att <- dml.sensemakr:::att.npm.cond(y, d, y / 2, dhat = pi, phat = 0.5,
                                       centered = centered)
    atu <- dml.sensemakr:::atu.npm.cond(y, d, y / 2, dhat = pi, phat = 0.5,
                                       centered = centered)
    expect_equal(att$estimates$nu2.s, 3.25 - as.numeric(centered))
    expect_equal(atu$estimates$nu2.s, 3.25 - as.numeric(centered))
  }
  fit <- dml.sensemakr:::ate.plm(y, d, y / 2, pi)
  expect_equal(fit$estimates$nu2.s, 6.25)
})

test_that("group estimates use their own floors in every parameterization", {
  groups <- factor(rep(c("a", "b"), each = 12))
  d <- c(rep(0, 9), rep(1, 3), rep(0, 6), rep(1, 6))
  n <- length(d)
  y <- seq_len(n) / 3
  pi <- ifelse(d == 0, 0.85, 0.15)
  mock <- list(data = list(y = y, d = d, x = cbind(group = as.integer(groups))),
               info = list(cf.reps = 2, target = c("ate", "att", "atu")),
               fits = rep(list(list(preds = list(dhat = pi, phat = rep(mean(d), n),
                                                 yhat0 = y / 2, yhat1 = y / 2))), 2))
  for (conditional in c(FALSE, TRUE)) {
    for (centered in c(FALSE, TRUE)) {
      mock$info$conditional <- conditional
      mock$info$centered <- centered
      fits <- dml.sensemakr:::group.ate.npm(mock, groups)
      for (target in mock$info$target) for (group in levels(groups)) {
        p <- mean(d[groups == group])
        floor <- if (conditional && target != "ate") {
          if (centered) 0 else 1
        } else 1 / (p * (1 - p))
        for (fit in fits[[paste(target, group, sep = ".")]]) {
          expect_equal(fit$estimates$nu2.s, floor)
        }
      }
    }
  }
  for (dd in list(d, seq_len(n) + rep(c(0, 10), each = 12))) {
    mock$data$d <- dd
    mock$fits <- rep(list(list(preds = list(dhat = dd + seq_len(n), yhat = y / 2))), 2)
    fits <- dml.sensemakr:::group.ate.plm(mock, groups)
    for (group in levels(groups)) {
      dg <- dd[groups == group]
      for (fit in fits[[paste("ate", group, sep = ".")]]) {
        expect_equal(fit$estimates$nu2.s, 1 / mean((dg - mean(dg))^2))
      }
    }
  }
})

test_that("zero centered scale gives explicit unavailable SEs except at zero confounding", {
  d <- c(0, 0, 0, 1, 1, 1)
  fit <- dml.sensemakr:::att.npm.cond(seq_along(d), d, rep(0, 6),
                                     dhat = 1 - d, phat = 0.5, centered = TRUE)
  expect_equal(fit$estimates$S2, 0)
  positive <- dml.sensemakr:::bounds(fit)
  expect_equal(positive$estimates$bias.bound, 0)
  expect_identical(positive$estimates$se.bias.bound, NA_real_)
  expect_identical(positive$estimates$se.theta.m, NA_real_)
  expect_identical(positive$estimates$se.theta.p, NA_real_)
  for (args in list(list(cf.y = 0), list(cf.d = 0), list(rho2 = 0))) {
    zero <- do.call(dml.sensemakr:::bounds, c(list(short.results = fit), args))
    expect_equal(zero$estimates$bias.bound, 0)
    expect_equal(zero$estimates$se.bias.bound, 0)
    expect_equal(zero$estimates$se.theta.m, fit$estimates$se.theta.s)
    expect_equal(zero$estimates$se.theta.p, fit$estimates$se.theta.s)
  }

  model <- structure(list(info = list(target = "att"),
                          results = list(main = list(treat = list(fit)))), class = "dml")
  bench <- structure(list(
    benchmarks = list(x = list(gain.Y = 0.2, gain.D = 0.3, rho = 0.5)),
    benchmarks_psis = list(x = list(psi.theta.s = list(fit$psis$psi.theta.s),
      psi.nu2.s = list(fit$psis$psi.nu2.s),
      psi.sigma2.s = list(fit$psis$psi.sigma2.s),
      psi.GY = list(rep(0, 6)), psi.GD = list(rep(0, 6)),
      psi.rho = list(rep(0, 6))))), class = "dml_benchmark")
  positive <- benchmark_bounds(model, bench)
  expect_equal(positive$theta.minus, fit$estimates$theta.s)
  expect_true(is.na(positive$se.minus))
  zero <- benchmark_bounds(model, bench, kY = 0)
  expect_equal(zero$se.minus, fit$estimates$se.theta.s)
  expect_equal(zero$se.plus, fit$estimates$se.theta.s)
})

test_that("undefined conditional parameters are not converted to finite estimates", {
  fit <- dml.sensemakr:::att.npm.cond(1:6, rep(0, 6), rep(0, 6),
                                     dhat = rep(0.5, 6), phat = 0)
  expect_true(is.na(fit$estimates$nu2.s))
})
