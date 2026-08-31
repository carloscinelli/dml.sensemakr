library(testthat)
library(dml.sensemakr)

.test_rv_result <- function(theta, S2 = 1, se.theta = 0.2,
                            se.S2 = 0.1, covariance = 0) {
  list(estimates = list(
    theta.s = theta,
    S2 = S2,
    se.theta.s = se.theta,
    se.S2 = se.S2,
    cov.theta.S2 = covariance
  ))
}

.test_rv_coefs <- function(results) {
  theta <- vapply(results, function(x) x$estimates$theta.s, numeric(1))
  se <- vapply(results, function(x) x$estimates$se.theta.s, numeric(1))
  rbind(
    mean = dml.sensemakr:::combine.mean(theta, se),
    median = dml.sensemakr:::combine.median(theta, se)
  )
}

.test_rv_fit <- function(main, target, groups = NULL) {
  results <- list(main = main)
  coefs <- list(main = lapply(main, .test_rv_coefs))
  if (!is.null(groups)) {
    results$groups <- groups
    coefs$groups <- lapply(groups, .test_rv_coefs)
  }
  structure(
    list(info = list(target = target), results = results, coefs = coefs),
    class = "dml"
  )
}

.test_rv_from_factor <- function(factor, rho2 = 1) {
  scaled <- factor / sqrt(rho2)
  2 * scaled / (sqrt(scaled^2 + 4) + scaled)
}

.test_xrv_from_factor <- function(factor, rho2 = 1) {
  factor^2 / (rho2 + factor^2)
}

test_that("RV and XRV S3 method formals preserve the public API", {
  ns <- asNamespace("dml.sensemakr")
  rv_dml <- getS3method("robustness_value", "dml", envir = ns)
  rv_bounds <- getS3method("robustness_value", "dml.bounds", envir = ns)
  xrv_dml <- getS3method("extreme_robustness_value", "dml", envir = ns)
  xrv_bounds <- getS3method(
    "extreme_robustness_value", "dml.bounds", envir = ns
  )

  expect_identical(names(formals(rv_dml)),
                   c("model", "theta", "alpha", "..."))
  expect_identical(names(formals(rv_bounds)),
                   c("model", "theta", "alpha", "..."))
  expect_identical(names(formals(xrv_dml)),
                   c("model", "theta", "alpha", "rho2", "..."))
  expect_identical(names(formals(xrv_bounds)),
                   c("model", "theta", "alpha", "rho2", "..."))
  for (method in list(rv_dml, rv_bounds, xrv_dml, xrv_bounds)) {
    expect_identical(formals(method)$theta, 0)
    expect_identical(formals(method)$alpha, 0.05)
  }
  expect_identical(formals(xrv_dml)$rho2, 1)
  expect_identical(formals(xrv_bounds)$rho2, 1)
})

test_that("the factor-direct evaluator reproduces confidence_bounds.numeric", {
  raw <- list(
    theta = c(0.7, 0.55, 0.9, -0.1, 0.4),
    S2 = c(1.2, 0.8, 2, 1.5, 0.6),
    se.theta = c(0.11, 0.08, 0.15, 0.09, 0.12),
    se.S2 = c(0.07, 0.05, 0.12, 0.08, 0.06),
    correlation = c(-0.6, 0.4, 0, 0.7, -0.3)
  )
  raw$covariance <- raw$correlation * raw$se.theta * raw$se.S2

  for (repetitions in c(1L, 4L, 5L)) {
    take <- seq_len(repetitions)
    statistics <- list(
      theta = raw$theta[take],
      S = sqrt(raw$S2[take]),
      variance_constant = raw$se.theta[take]^2,
      variance_linear_magnitude =
        raw$covariance[take] / sqrt(raw$S2[take]),
      variance_quadratic =
        raw$se.S2[take]^2 / (4 * raw$S2[take])
    )
    for (method in c("mean", "median")) {
      for (factor in c(0, 0.01, 0.2, 2)) {
        for (alpha in c(0.05, 1)) {
          observed <- dml.sensemakr:::.rv_factor_bounds(
            factor, statistics, qnorm(max(1 - alpha, 0.5)), method
          )
          expected <- confidence_bounds(
            theta.s = raw$theta[take],
            S2 = raw$S2[take],
            se.theta.s = raw$se.theta[take],
            se.S2 = raw$se.S2[take],
            cov.theta.S2 = raw$covariance[take],
            cf.y = 1,
            cf.d = factor^2 / (1 + factor^2),
            rho2 = 1,
            combine.method = method,
            level = 1 - alpha
          )
          expect_equal(unname(observed), unname(expected), tolerance = 2e-12)
        }
      }
    }
  }
})

test_that("the ordinary two-sided CI gate returns exact zero first", {
  main <- list(
    all = list(.test_rv_result(
      1.8, S2 = 1, se.theta = 1, se.S2 = sqrt(0.4)
    )),
    treat = list(.test_rv_result(
      4, S2 = 1, se.theta = 1, se.S2 = sqrt(0.4)
    ))
  )
  fit <- .test_rv_fit(main, target = c("ate", "att"))
  ci <- confint(fit, level = 0.95)

  expect_lte(ci["ate", 1], 0)
  expect_gte(ci["ate", 2], 0)
  # The one-sided lower endpoint at factor zero is nevertheless positive.
  expect_gt(1.8 - qnorm(0.95), 0)

  for (method in c("mean", "median")) {
    rv <- robustness_value(fit, theta = 0, alpha = 0.05,
                           combine.method = method)
    xrv <- extreme_robustness_value(
      fit, theta = 0, alpha = 0.05, combine.method = method
    )
    expect_identical(unname(rv[["ate"]]), 0)
    expect_identical(unname(xrv[["ate"]]), 0)
    expect_gt(rv[["att"]], 0)
    expect_gt(xrv[["att"]], 0)
  }

  one_row <- .test_rv_fit(main["all"], target = "ate")
  boundary <- confint(one_row, level = 0.95)["ate", 1]
  expect_identical(
    unname(robustness_value(one_row, theta = boundary)[["ate"]]), 0
  )
  expect_identical(
    unname(extreme_robustness_value(one_row, theta = boundary)[["ate"]]), 0
  )
})

test_that("the zero gate precedes positive-S2 validation", {
  fit <- .test_rv_fit(
    list(all = list(.test_rv_result(0, S2 = -1, se.theta = 0.1))),
    target = "ate"
  )
  expect_warning(rv <- robustness_value(fit), NA)
  expect_warning(xrv <- extreme_robustness_value(fit), NA)
  expect_identical(unname(rv[["ate"]]), 0)
  expect_identical(unname(xrv[["ate"]]), 0)
})

test_that("invalid positive-sensitivity repetitions return NA explicitly", {
  fit <- .test_rv_fit(
    list(all = list(.test_rv_result(4, S2 = -1, se.theta = 0.1))),
    target = "ate"
  )
  expect_warning(rv <- robustness_value(fit), "invalid.*non-positive S2")
  expect_warning(
    xrv <- extreme_robustness_value(fit), "invalid.*non-positive S2"
  )
  expect_true(is.na(rv[["ate"]]))
  expect_true(is.na(xrv[["ate"]]))
})

test_that("alpha = 1 honors the current mean and median aggregation rules", {
  repetitions <- lapply(
    c(2, 2.1, 8), .test_rv_result,
    S2 = 1, se.theta = 0.1, se.S2 = 0.1
  )
  fit <- .test_rv_fit(list(all = repetitions), target = "ate")
  factors <- c(mean = mean(c(2, 2.1, 8)), median = median(c(2, 2.1, 8)))

  for (method in names(factors)) {
    rv <- robustness_value(fit, theta = 0, alpha = 1,
                           combine.method = method)
    xrv <- extreme_robustness_value(
      fit, theta = 0, alpha = 1, combine.method = method
    )
    expect_equal(unname(rv[["ate"]]),
                 .test_rv_from_factor(factors[[method]]), tolerance = 2e-9)
    expect_equal(unname(xrv[["ate"]]),
                 .test_xrv_from_factor(factors[[method]]), tolerance = 2e-9)
  }
  expect_equal(
    robustness_value(fit, alpha = 1),
    robustness_value(fit, alpha = 1, combine.method = "median")
  )

  # The selected aggregation rule must also govern the ordinary-CI zero gate.
  # Dispersion makes the mean CI include zero, whereas the median CI excludes it.
  expect_identical(
    unname(robustness_value(fit, combine.method = "mean")[["ate"]]), 0
  )
  expect_gt(
    robustness_value(fit, combine.method = "median")[["ate"]], 0
  )
  expect_identical(
    unname(extreme_robustness_value(
      fit, combine.method = "mean"
    )[["ate"]]),
    0
  )
  expect_gt(
    extreme_robustness_value(fit, combine.method = "median")[["ate"]], 0
  )
})

test_that("mean aggregation's analytical and envelope solvers agree", {
  statistics <- list(
    theta = c(1.1, 1.4, 0.9, 1.25, 1.05),
    S = c(0.8, 1.2, 0.9, 1.1, 1),
    variance_constant = c(0.04, 0.09, 0.0625, 0.049, 0.081),
    variance_linear_magnitude = c(0.01, -0.02, 0.015, 0, 0.025),
    variance_quadratic = c(0.005, 0.007, 0.004, 0.006, 0.008)
  )
  critical <- qnorm(0.95)
  for (case in list(c(theta = -0.5, side = "lwr"),
                    c(theta = 2.5, side = "upr"))) {
    theta <- as.numeric(case[["theta"]])
    side <- case[["side"]]
    analytical <- dml.sensemakr:::.rv_required_factor_mean(
      statistics, theta, critical, side, tolerance = 1e-12
    )
    general <- dml.sensemakr:::.rv_required_factor_general(
      statistics, theta, critical, "mean",
      tolerance = 1e-12, maximum_iterations = 200L
    )
    expect_equal(analytical, general, tolerance = 1e-9)
    endpoint <- dml.sensemakr:::.rv_factor_bounds(
      analytical, statistics, critical, "mean"
    )[[side]]
    expect_equal(endpoint, theta, tolerance = 1e-10)
  }
})

test_that("median envelope finds an interior global extremum", {
  statistics <- list(
    theta = c(-0.884351392620848, 1.08017007065805,
              -0.7420206032882, 0.973184260986041,
              0.911899151950328, -0.614060618132291,
              0.326733785498567),
    S = c(1.60212154053152, 1.78219421692193, 1.86681311763823,
          1.21517276652157, 1.13039120808244, 0.936282948404551,
          0.544964247196913),
    variance_constant = c(
      0.438078652303624, 0.0464904612186741, 0.498780968599552,
      0.173372033434326, 0.364786796129857, 0.0119108305694334,
      0.410802288538976
    ),
    variance_linear_magnitude = c(
      -0.0431722465171573, 0.0374486430481096, 0.0858426709511676,
      -0.111369341263059, -0.015699336930572, -0.0453763196156501,
      -0.0580365516928586
    ),
    variance_quadratic = c(
      0.0512110723777797, 0.0234897641903963, 0.0241661500073986,
      0.0433666126269147, 0.0333993762943074, 0.0867133044061185,
      0.0207242585299824
    )
  )
  maximum <- 0.7630566223524511
  critical <- qnorm(0.95)
  solution <- dml.sensemakr:::.rv_endpoint_extreme(
    statistics, "upr", maximum, critical, "median"
  )
  endpoint_values <- c(
    dml.sensemakr:::.rv_factor_bounds(0, statistics, critical, "median")[["upr"]],
    dml.sensemakr:::.rv_factor_bounds(
      maximum, statistics, critical, "median"
    )[["upr"]]
  )

  expect_equal(unname(solution[["factor"]]),
               0.4279247173498355, tolerance = 2e-10)
  expect_equal(unname(solution[["value"]]),
               2.208577954847576, tolerance = 2e-10)
  expect_gt(solution[["factor"]], 0)
  expect_lt(solution[["factor"]], maximum)
  expect_gt(solution[["value"]], max(endpoint_values) + 0.1)

  outcome_scale <- 1e-12
  scaled <- statistics
  scaled$theta <- outcome_scale * scaled$theta
  scaled$S <- outcome_scale * scaled$S
  scaled$variance_constant <-
    outcome_scale^2 * scaled$variance_constant
  scaled$variance_linear_magnitude <-
    outcome_scale^2 * scaled$variance_linear_magnitude
  scaled$variance_quadratic <-
    outcome_scale^2 * scaled$variance_quadratic
  scaled_solution <- dml.sensemakr:::.rv_endpoint_extreme(
    scaled, "upr", maximum, critical, "median"
  )
  expect_equal(unname(scaled_solution[["factor"]]),
               unname(solution[["factor"]]), tolerance = 2e-10)
  expect_equal(unname(scaled_solution[["value"]]) / outcome_scale,
               unname(solution[["value"]]), tolerance = 2e-10)
})

test_that("factorized endpoint variances remain stable at a PSD boundary", {
  theta_se <- 0.19063870385878209
  S2_se <- 46.203122593166192
  S <- 405.72464285830637
  factor <- 3.3481208930028035
  statistics <- list(
    theta = 1,
    S = S,
    variance_constant = theta_se^2,
    variance_linear_magnitude = theta_se * S2_se / S,
    variance_quadratic = S2_se^2 / (4 * S^2)
  )
  expanded <- statistics$variance_constant -
    factor * statistics$variance_linear_magnitude +
    factor^2 * statistics$variance_quadratic
  expect_lt(expanded, 0)

  bounds <- dml.sensemakr:::.rv_factor_bounds(
    factor, statistics, qnorm(0.95), "mean"
  )
  expect_true(all(is.finite(bounds)))
  expect_equal(unname(bounds[["lwr"]]), 1 - factor * S,
               tolerance = 2e-10)
})

test_that("ATE/ATT/ATU and group rows map to their stored results", {
  main <- list(
    all = list(.test_rv_result(1, S2 = 1)),
    treat = list(.test_rv_result(2, S2 = 4)),
    untr = list(.test_rv_result(3, S2 = 1))
  )
  groups <- list(
    "att.low" = list(.test_rv_result(0.5, S2 = 4)),
    "atu.high" = list(.test_rv_result(-1, S2 = 1))
  )
  fit <- .test_rv_fit(
    main, target = c("ate", "att", "atu"), groups = groups
  )
  factors <- c(ate = 1, att = 1, atu = 3,
               "g.att.low" = 0.25, "g.atu.high" = 1)

  rv <- robustness_value(fit, theta = 0, alpha = 1)
  rv_quarter <- robustness_value(
    fit, theta = 0, alpha = 1, rho2 = 0.25
  )
  xrv <- extreme_robustness_value(fit, theta = 0, alpha = 1)
  xrv_quarter <- extreme_robustness_value(
    fit, theta = 0, alpha = 1, rho2 = 0.25
  )
  expect_type(rv, "double")
  expect_type(xrv, "double")
  expect_identical(names(rv), names(factors))
  expect_identical(names(xrv), names(factors))
  expect_equal(unname(rv), unname(.test_rv_from_factor(factors)),
               tolerance = 1e-10)
  expect_equal(unname(rv_quarter),
               unname(.test_rv_from_factor(factors, rho2 = 0.25)),
               tolerance = 1e-10)
  expect_equal(unname(xrv), unname(.test_xrv_from_factor(factors)),
               tolerance = 1e-10)
  expect_equal(unname(xrv_quarter),
               unname(.test_xrv_from_factor(factors, rho2 = 0.25)),
               tolerance = 1e-10)

  selected <- robustness_value(
    fit, theta = 0, alpha = 1, parm = c("att", "g.att.low")
  )
  expect_identical(names(selected), c("att", "g.att.low"))
})

test_that("dml.bounds methods delegate without changing results", {
  repetitions <- lapply(
    c(2, 2.1, 8), .test_rv_result,
    S2 = 1, se.theta = 0.1, se.S2 = 0.1
  )
  fit <- .test_rv_fit(list(all = repetitions), target = "ate")
  bounded <- structure(list(dml.fit = fit), class = "dml.bounds")

  for (method in c("mean", "median")) {
    expect_equal(
      robustness_value(bounded, alpha = 1, combine.method = method),
      robustness_value(fit, alpha = 1, combine.method = method),
      tolerance = 0
    )
    expect_equal(
      extreme_robustness_value(
        bounded, alpha = 1, rho2 = 0.4, combine.method = method
      ),
      extreme_robustness_value(
        fit, alpha = 1, rho2 = 0.4, combine.method = method
      ),
      tolerance = 0
    )
  }
})

test_that("the Long Story Short empirical sufficient statistics are pinned", {
  empirical <- structure(c(
    9038.19687775086, 9018.85434365544, 9103.23354726871,
    8869.73664900216, 9276.01286314713,
    14212211471.7079, 14132123067.9104, 14226324302.4578,
    14386333748.8283, 14356318571.6163,
    1326.26463125728, 1304.32778931761, 1340.77477606325,
    1313.04572941434, 1325.65787468218,
    2108823089.75727, 2099364052.4773, 2124066948.29578,
    2155062749.31819, 2106084351.62118,
    238973527480.196, 147308691425.416, 323824368153.084,
    152163620631.631, 255606106343.244
  ), dim = c(5L, 5L), dimnames = list(NULL, c(
    "theta.s", "S2", "se.theta.s", "se.S2", "cov.theta.S2"
  )))
  repetitions <- lapply(seq_len(nrow(empirical)), function(index) {
    .test_rv_result(
      theta = empirical[index, "theta.s"],
      S2 = empirical[index, "S2"],
      se.theta = empirical[index, "se.theta.s"],
      se.S2 = empirical[index, "se.S2"],
      covariance = empirical[index, "cov.theta.S2"]
    )
  })
  fit <- .test_rv_fit(list(all = repetitions), target = "ate")
  result <- dml.sensemakr:::.rv_sensitivity_statistics(
    fit, theta = 0, alpha = 0.05, rho2 = 1,
    combine.method = "median", confint.arguments = list()
  )

  expect_equal(unname(result$factor[["ate"]]),
               0.0567811900982633, tolerance = 1e-9)
  expect_equal(unname(result$RV[["ate"]]),
               0.0551920172690127, tolerance = 1e-9)
  expect_equal(unname(result$XRV[["ate"]]),
               0.00321374211162753, tolerance = 1e-9)
  expect_equal(
    unname(robustness_value(fit)[["ate"]]),
    0.0551920172690127,
    tolerance = 1e-9
  )
  expect_equal(
    unname(extreme_robustness_value(fit)[["ate"]]),
    0.00321374211162753,
    tolerance = 1e-9
  )
  expect_equal(
    unname(result$XRV[["ate"]]),
    unname(result$RV[["ate"]]^2 /
      (1 - result$RV[["ate"]] + result$RV[["ate"]]^2)),
    tolerance = 2e-12
  )
})

test_that("the published DiD minimum-wage factor maps to its reported RVs", {
  factor <- 0.0448029688077114
  rv <- .test_rv_from_factor(factor)
  xrv <- .test_xrv_from_factor(factor)

  expect_equal(round(100 * rv, 4), 4.3811)
  expect_equal(round(100 * xrv, 4), 0.2003)
  expect_equal(xrv, rv^2 / (1 - rv + rv^2), tolerance = 2e-15)
})
