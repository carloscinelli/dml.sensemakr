library(testthat)
library(dml.sensemakr)

# These references deliberately do not call confidence_bounds(), bias.factor(),
# combine.mean(), combine.median(), or any of the RV envelope helpers.
.test_cb_factor <- function(cf.y, cf.d, rho2) {
  sqrt(rho2 * cf.y * cf.d / (1 - cf.d))
}

.test_cb_at_factor <- function(factor, raw, level, combine.method) {
  S <- sqrt(raw$S2)
  lower_point <- raw$theta - factor * S
  upper_point <- raw$theta + factor * S
  lower_variance <- raw$se.theta^2 +
    factor^2 * raw$se.S2^2 / (4 * raw$S2) -
    factor * raw$covariance / S
  upper_variance <- raw$se.theta^2 +
    factor^2 * raw$se.S2^2 / (4 * raw$S2) +
    factor * raw$covariance / S
  if (min(c(lower_variance, upper_variance)) < -1e-12) {
    stop("The reference fixture has an invalid endpoint variance.")
  }
  lower_variance <- pmax(lower_variance, 0)
  upper_variance <- pmax(upper_variance, 0)

  aggregate <- function(point, variance) {
    estimate <- if (combine.method == "mean") mean(point) else median(point)
    combined_variance <- if (combine.method == "mean") {
      mean(variance + (point - estimate)^2)
    } else {
      median(variance + (point - estimate)^2)
    }
    c(estimate = estimate, se = sqrt(combined_variance))
  }
  lower <- aggregate(lower_point, lower_variance)
  upper <- aggregate(upper_point, upper_variance)
  critical <- qnorm(max(level, 0.5))
  c(
    lwr = unname(lower[["estimate"]] - critical * lower[["se"]]),
    upr = unname(upper[["estimate"]] + critical * upper[["se"]])
  )
}

.test_cb_grid_extreme <- function(raw, maximum.factor, level,
                                  combine.method, side,
                                  grid.size = 20001L) {
  grid <- seq(0, maximum.factor, length.out = grid.size)
  values <- vapply(
    grid,
    function(factor) .test_cb_at_factor(
      factor, raw, level, combine.method
    )[[side]],
    numeric(1)
  )
  selected <- if (side == "lwr") which.min(values) else which.max(values)
  candidates <- c(0, maximum.factor, grid[[selected]])

  # The dense grid identifies the global piece. Refine only inside that small
  # neighborhood; retain both endpoints explicitly because optimize() does not.
  if (selected > 1L && selected < length(grid)) {
    bracket <- grid[c(selected - 1L, selected + 1L)]
    objective <- function(factor) {
      value <- .test_cb_at_factor(
        factor, raw, level, combine.method
      )[[side]]
      if (side == "lwr") value else -value
    }
    refined <- optimize(objective, bracket, tol = 1e-12)$minimum
    candidates <- c(candidates, refined)
  }
  candidate.values <- vapply(
    candidates,
    function(factor) .test_cb_at_factor(
      factor, raw, level, combine.method
    )[[side]],
    numeric(1)
  )
  selected <- if (side == "lwr") {
    which.min(candidate.values)
  } else {
    which.max(candidate.values)
  }
  c(value = candidate.values[[selected]], factor = candidates[[selected]])
}

.test_cb_grid_envelope <- function(raw, maximum.factor, level,
                                   combine.method, grid.size = 20001L) {
  lower <- .test_cb_grid_extreme(
    raw, maximum.factor, level, combine.method, "lwr",
    grid.size = grid.size
  )
  upper <- .test_cb_grid_extreme(
    raw, maximum.factor, level, combine.method, "upr",
    grid.size = grid.size
  )
  c(
    lwr = unname(lower[["value"]]),
    upr = unname(upper[["value"]]),
    lwr.at = unname(lower[["factor"]]),
    upr.at = unname(upper[["factor"]])
  )
}

.test_cb_interior_raw <- function() {
  S <- c(
    1.60212154053152, 1.78219421692193, 1.86681311763823,
    1.21517276652157, 1.13039120808244, 0.936282948404551,
    0.544964247196913
  )
  variance_constant <- c(
    0.438078652303624, 0.0464904612186741, 0.498780968599552,
    0.173372033434326, 0.364786796129857, 0.0119108305694334,
    0.410802288538976
  )
  variance_linear <- c(
    -0.0431722465171573, 0.0374486430481096, 0.0858426709511676,
    -0.111369341263059, -0.015699336930572, -0.0453763196156501,
    -0.0580365516928586
  )
  variance_quadratic <- c(
    0.0512110723777797, 0.0234897641903963, 0.0241661500073986,
    0.0433666126269147, 0.0333993762943074, 0.0867133044061185,
    0.0207242585299824
  )
  list(
    theta = c(
      -0.884351392620848, 1.08017007065805, -0.7420206032882,
      0.973184260986041, 0.911899151950328, -0.614060618132291,
      0.326733785498567
    ),
    S2 = S^2,
    se.theta = sqrt(variance_constant),
    se.S2 = 2 * S * sqrt(variance_quadratic),
    covariance = variance_linear * S
  )
}

.test_cb_call_numeric <- function(raw, cf.y, cf.d, rho2, level,
                                  combine.method, max) {
  confidence_bounds(
    theta.s = raw$theta,
    S2 = raw$S2,
    se.theta.s = raw$se.theta,
    se.S2 = raw$se.S2,
    cov.theta.S2 = raw$covariance,
    cf.y = cf.y,
    cf.d = cf.d,
    rho2 = rho2,
    level = level,
    combine.method = combine.method,
    max = max
  )
}

test_that("confidence_bounds methods add max without shifting old arguments", {
  namespace <- asNamespace("dml.sensemakr")
  numeric_method <- getS3method("confidence_bounds", "numeric", envir = namespace)
  dml_method <- getS3method("confidence_bounds", "dml", envir = namespace)
  bounds_method <- getS3method(
    "confidence_bounds", "dml.bounds", envir = namespace
  )

  expect_identical(names(formals(numeric_method)), c(
    "theta.s", "S2", "se.theta.s", "se.S2", "cov.theta.S2",
    "cf.y", "cf.d", "rho2", "combine.method", "level", "max", "..."
  ))
  expect_identical(names(formals(dml_method)), c(
    "model", "cf.y", "cf.d", "rho2", "level", "combine.method",
    "max", "..."
  ))
  expect_identical(names(formals(bounds_method)), c(
    "model", "cf.y", "cf.d", "rho2", "level", "combine.method",
    "return", "max", "..."
  ))
  expect_identical(formals(numeric_method)$max, TRUE)
  expect_identical(formals(dml_method)$max, TRUE)
  expect_identical(formals(bounds_method)$max, TRUE)
})

test_that("numeric max=FALSE is the fixed-corner calculation", {
  raw <- .test_cb_interior_raw()
  maximum.factor <- 0.7630566223524511
  cf.y <- 1
  cf.d <- maximum.factor^2 / (1 + maximum.factor^2)
  level <- 0.95

  for (method in c("mean", "median")) {
    observed <- .test_cb_call_numeric(
      raw, cf.y, cf.d, rho2 = 1, level, method, max = FALSE
    )
    expected <- .test_cb_at_factor(
      maximum.factor, raw, level, method
    )
    expect_equal(unname(observed), unname(expected), tolerance = 2e-12)
  }

  median_fixed <- .test_cb_call_numeric(
    raw, cf.y, cf.d, 1, level, "median", max = FALSE
  )
  expect_equal(unname(median_fixed),
               c(-1.4480712364319865, 2.0150818146147835),
               tolerance = 2e-12)
})

test_that("numeric max=TRUE is the independently gridded envelope", {
  raw <- .test_cb_interior_raw()
  maximum.factor <- 0.7630566223524511
  cf.y <- 1
  cf.d <- maximum.factor^2 / (1 + maximum.factor^2)
  level <- 0.95

  for (method in c("mean", "median")) {
    observed <- .test_cb_call_numeric(
      raw, cf.y, cf.d, rho2 = 1, level, method, max = TRUE
    )
    default <- confidence_bounds(
      raw$theta, raw$S2, raw$se.theta, raw$se.S2, raw$covariance,
      cf.y, cf.d, 1, method, level
    )
    fixed <- .test_cb_call_numeric(
      raw, cf.y, cf.d, rho2 = 1, level, method, max = FALSE
    )
    reference <- .test_cb_grid_envelope(
      raw, maximum.factor, level, method
    )

    expect_equal(default, observed, tolerance = 0)
    expect_equal(unname(observed), unname(reference[c("lwr", "upr")]),
                 tolerance = 2e-8)
    expect_lte(observed[["lwr"]], fixed[["lwr"]] + 1e-12)
    expect_gte(observed[["upr"]], fixed[["upr"]] - 1e-12)
  }

  median_envelope <- .test_cb_call_numeric(
    raw, cf.y, cf.d, 1, level, "median", max = TRUE
  )
  expect_equal(unname(median_envelope),
               c(-1.4480712364319865, 2.208577954847576),
               tolerance = 2e-9)
  expect_gt(median_envelope[["upr"]] -
              .test_cb_call_numeric(
                raw, cf.y, cf.d, 1, level, "median", max = FALSE
              )[["upr"]],
            0.19)
})

test_that("numeric CI-max validates inputs and handles invalid repetitions", {
  raw <- list(
    theta = c(1, 2),
    S2 = c(1, 4),
    se.theta = c(0.2, 0.3),
    se.S2 = c(0.1, 0.2),
    covariance = c(0.01, -0.02)
  )
  arguments <- list(
    theta.s = raw$theta,
    S2 = raw$S2,
    se.theta.s = raw$se.theta,
    se.S2 = raw$se.S2,
    cov.theta.S2 = raw$covariance,
    cf.y = 0.2,
    cf.d = 0.3,
    max = TRUE
  )

  expect_equal(
    do.call(confidence_bounds, c(arguments, list(level = 0.2))),
    do.call(confidence_bounds, c(arguments, list(level = 0.5))),
    tolerance = 0
  )
  invalid_max <- arguments
  invalid_max$max <- 1
  expect_error(
    do.call(confidence_bounds, invalid_max),
    "single non-missing logical"
  )
  invalid_cf_d <- arguments
  invalid_cf_d$cf.d <- 1
  expect_error(
    do.call(confidence_bounds, invalid_cf_d),
    "0 <= cf.d < 1"
  )

  partially_invalid <- arguments
  partially_invalid$S2[2] <- NA_real_
  expect_warning(
    dropped <- do.call(confidence_bounds, partially_invalid),
    "dropping those repetitions"
  )
  one_valid <- lapply(arguments, function(value) {
    if (length(value) == 2L) value[1L] else value
  })
  expect_equal(
    dropped,
    do.call(confidence_bounds, one_valid),
    tolerance = 0
  )

  all_invalid <- arguments
  all_invalid$S2[] <- NA_real_
  expect_warning(
    missing <- do.call(confidence_bounds, all_invalid),
    "returning NA confidence bounds"
  )
  expect_true(all(is.na(missing)))
})

.test_cb_result <- function(theta, S2, se.theta, se.S2, covariance) {
  count <- 4L
  u <- c(-1, -1, 1, 1)
  v <- c(-1, 1, -1, 1)
  correlation <- covariance / (se.theta * se.S2)
  stopifnot(abs(correlation) <= 1)
  psi.theta <- sqrt(count) * se.theta * u
  psi.S2 <- sqrt(count) * se.S2 * (
    correlation * u + sqrt(1 - correlation^2) * v
  )
  list(
    psis = list(
      psi.theta.s = psi.theta,
      psi.sigma2.s = numeric(count),
      psi.nu2.s = psi.S2,
      psi.S2 = psi.S2
    ),
    estimates = list(
      theta.s = theta,
      se.theta.s = se.theta,
      sigma2.s = 1,
      se.sigma2.s = 0,
      nu2.s = S2,
      se.nu2.s = se.S2,
      S2 = S2,
      se.S2 = se.S2,
      cov.theta.S2 = covariance
    )
  )
}

.test_cb_coefs <- function(results) {
  theta <- vapply(results, function(x) x$estimates$theta.s, numeric(1))
  se <- vapply(results, function(x) x$estimates$se.theta.s, numeric(1))
  make <- function(method) {
    estimate <- if (method == "mean") mean(theta) else median(theta)
    variance <- if (method == "mean") {
      mean(se^2 + (theta - estimate)^2)
    } else {
      median(se^2 + (theta - estimate)^2)
    }
    c(estimate = estimate, se = sqrt(variance))
  }
  rbind(mean = make("mean"), median = make("median"))
}

.test_cb_fit <- function() {
  main <- list(
    all = list(.test_cb_result(1, 1, 0.2, 0.1, 0.01)),
    treat = list(.test_cb_result(2, 4, 0.3, 0.2, -0.02)),
    untr = list(.test_cb_result(-1, 2.25, 0.15, 0.12, 0.005))
  )
  groups <- list(
    "att.low" = list(.test_cb_result(0.5, 0.64, 0.18, 0.09, -0.004)),
    "atu.high" = list(.test_cb_result(-0.7, 1.44, 0.22, 0.11, 0.006))
  )
  structure(
    list(
      info = list(target = c("ate", "att", "atu")),
      results = list(main = main, groups = groups),
      coefs = list(
        main = lapply(main, .test_cb_coefs),
        groups = lapply(groups, .test_cb_coefs)
      )
    ),
    class = "dml"
  )
}

.test_cb_parameter_raw <- function(fit) {
  results <- c(fit$results$main, fit$results$groups)
  names(results) <- c("ate", "att", "atu", "g.att.low", "g.atu.high")
  lapply(results, function(repetitions) {
    get <- function(name) vapply(
      repetitions, function(x) x$estimates[[name]], numeric(1)
    )
    list(
      theta = get("theta.s"),
      S2 = get("S2"),
      se.theta = get("se.theta.s"),
      se.S2 = get("se.S2"),
      covariance = get("cov.theta.S2")
    )
  })
}

.test_cb_model_reference <- function(fit, cf.y, cf.d, rho2, level,
                                     combine.method, max) {
  raw <- .test_cb_parameter_raw(fit)
  maximum.factor <- .test_cb_factor(cf.y, cf.d, rho2)
  rows <- lapply(raw, function(parameter) {
    if (max) {
      .test_cb_grid_envelope(
        parameter, maximum.factor, level, combine.method,
        grid.size = 2001L
      )[c("lwr", "upr")]
    } else {
      .test_cb_at_factor(
        maximum.factor, parameter, level, combine.method
      )
    }
  })
  output <- do.call(rbind, rows)
  colnames(output) <- c("lwr", "upr")
  output
}

test_that("dml and dml.bounds preserve targets, groups, return, and class", {
  fit <- .test_cb_fit()
  cf.y <- 0.4
  cf.d <- 0.3
  rho2 <- 0.7
  level <- 0.9
  parameter.names <- c("ate", "att", "atu", "g.att.low", "g.atu.high")
  bounded <- dml_bounds(fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)

  for (method in c("mean", "median")) {
    for (maximize in c(FALSE, TRUE)) {
      from_fit <- confidence_bounds(
        fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2,
        level = level, combine.method = method, max = maximize
      )
      from_bounds <- confidence_bounds(
        bounded, level = level, combine.method = method, max = maximize
      )
      expected <- .test_cb_model_reference(
        fit, cf.y, cf.d, rho2, level, method, maximize
      )

      expect_s3_class(from_fit, "confidence.bounds")
      expect_identical(rownames(from_fit), parameter.names)
      expect_identical(colnames(from_fit), c("lwr", "upr"))
      expect_equal(as.numeric(from_fit), as.numeric(expected),
                   tolerance = 2e-8)
      expect_equal(from_bounds, from_fit, tolerance = 2e-12)
      expect_equal(attr(from_fit, "conf.levels"),
                   c(point = level, region = 2 * level - 1))
      expected.info <- list(cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
      if (maximize) expected.info$max <- TRUE
      expect_equal(attr(from_fit, "sens.param"), expected.info)
      if (maximize) {
        extrema <- attr(from_fit, "extrema.at")
        expect_identical(dim(extrema), c(5L, 2L))
        expect_identical(colnames(extrema), c("lwr.at", "upr.at"))
        expect_true(all(extrema >= 0))
        expect_true(all(extrema <= .test_cb_factor(cf.y, cf.d, rho2)))
      } else {
        expect_null(attr(from_fit, "extrema.at"))
      }

      lower <- confidence_bounds(
        fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2,
        level = level, combine.method = method, max = maximize,
        return = "lwr"
      )
      upper <- confidence_bounds(
        bounded, level = level, combine.method = method,
        max = maximize, return = "upr"
      )
      reversed <- confidence_bounds(
        bounded, level = level, combine.method = method,
        max = maximize, return = c("upr", "lwr")
      )
      expect_equal(as.numeric(lower), as.numeric(from_fit[, "lwr"]),
                   tolerance = 0)
      expect_equal(as.numeric(upper), as.numeric(from_fit[, "upr"]),
                   tolerance = 0)
      expect_identical(rownames(lower), parameter.names)
      expect_identical(rownames(upper), parameter.names)
      expect_identical(colnames(lower), "lwr")
      expect_identical(colnames(upper), "upr")
      expect_identical(colnames(reversed), c("upr", "lwr"))
      expect_equal(
        as.numeric(reversed),
        as.numeric(from_fit[, c("upr", "lwr")]),
        tolerance = 0
      )
    }
  }

  expect_equal(
    confidence_bounds(
      fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2, level = level
    ),
    confidence_bounds(
      fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2,
      level = level, max = TRUE
    ),
    tolerance = 0
  )
})

test_that("dml.bounds overrides fill stored parameters and forward all options", {
  fit <- .test_cb_fit()
  bounded <- dml_bounds(fit, cf.y = 0.2, cf.d = 0.3, rho2 = 0.6)

  for (maximize in c(FALSE, TRUE)) {
    observed <- confidence_bounds(
      bounded, cf.y = 0.4, level = 0.8,
      combine.method = "mean", max = maximize, return = "upr"
    )
    expected <- confidence_bounds(
      fit, cf.y = 0.4, cf.d = 0.3, rho2 = 0.6, level = 0.8,
      combine.method = "mean", max = maximize, return = "upr"
    )
    expect_equal(observed, expected, tolerance = 2e-12)
    expect_equal(attr(observed, "conf.levels"),
                 c(point = 0.8, region = 0.6))
    expected.info <- list(cf.y = 0.4, cf.d = 0.3, rho2 = 0.6)
    if (maximize) expected.info$max <- TRUE
    expect_equal(attr(observed, "sens.param"), expected.info)
  }
})

test_that("printed bounds distinguish maxima from fixed sensitivities", {
  fit <- .test_cb_fit()
  bounded <- dml_bounds(fit, cf.y = 0.2, cf.d = 0.3, rho2 = 0.6)

  expect_output(
    print(confidence_bounds(bounded, max = TRUE)),
    "Maximum sensitivity parameters"
  )
  expect_output(
    print(confidence_bounds(bounded, max = FALSE)),
    "Sensitivity parameters"
  )
})
