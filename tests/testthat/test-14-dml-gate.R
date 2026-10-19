# Test dml_gate(): add group ATEs to an already-fitted dml object.

# An inline groups= fit and a groups-free fit share a seed, so the groups added
# post hoc by dml_gate() should reproduce the inline group estimates exactly.
new_gate_fixture <- memoise::memoise(function() {
  data("pension", package = "dml.sensemakr")
  withr::local_seed(606)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  g <- cut(x[, "inc"], quantile(x[, "inc"], c(0, 0.5, 1), na.rm = TRUE),
           labels = c("low", "high"), include.lowest = TRUE)
  fit_inline <- dml(y, d, x, model = "npm", groups = g, cf.folds = 2, cf.reps = 1,
                    cf.seed = 123, verbose = FALSE)
  fit_nogrp  <- dml(y, d, x, model = "npm", cf.folds = 2, cf.reps = 1,
                    cf.seed = 123, verbose = FALSE)
  list(fit_inline = fit_inline, fit_nogrp = fit_nogrp, g = g)
})

# === dml_gate attaches group results to a fitted object ===
test_that("dml_gate() attaches group ATEs to a fitted dml object", {
  setup_gate <- new_gate_fixture()
  gated <- dml_gate(setup_gate$fit_nogrp, groups = setup_gate$g)
  expect_s3_class(gated, "dml")
  expect_false(is.null(gated$coefs$groups))
  expect_equal(names(gated$coefs$groups), paste0("ate.", levels(setup_gate$g)))
})

# === dml_gate matches the inline dml(groups=) path ===
test_that("dml_gate() reproduces the inline groups= estimates", {
  setup_gate <- new_gate_fixture()
  gated <- dml_gate(setup_gate$fit_nogrp, groups = setup_gate$g)
  inline_coefs <- setup_gate$fit_inline$coefs$groups
  gated_coefs  <- gated$coefs$groups
  expect_equal(names(gated_coefs), names(inline_coefs))
  for (lev in names(inline_coefs)) {
    expect_equal(gated_coefs[[lev]], inline_coefs[[lev]], tolerance = 1e-6)
  }
})

new_trim_fixture <- function(ps.trim = 0.01, target = "ate", conditional = NULL) {
  withr::local_seed(420)
  n <- 120
  x <- cbind(x1 = seq(-3, 3, length.out = n), x2 = rnorm(n))
  d <- rbinom(n, 1, plogis(3 * x[, "x1"]))
  y <- 1 + 0.5 * d + x[, "x1"] + sin(x[, "x2"]) + rnorm(n, sd = 0.2)
  dml(y, d, x, model = "npm", target = target, conditional = conditional,
      groups = rep("all", n), ps.trim = ps.trim, reg = "lm",
      dirty.tuning = FALSE, cf.folds = 2, cf.reps = 2, cf.seed = 1,
      verbose = FALSE)
}

test_that("a group containing everyone reproduces the ATE with the same trimming", {
  for (trim in list(0.01, 0.1, list(lower = 0.1, upper = 0.85))) {
    fit <- new_trim_fixture(ps.trim = trim)
    expect_equal(fit$coefs$groups$ate.all, fit$coefs$main$all)

    without_groups <- fit
    without_groups$results$groups <- without_groups$coefs$groups <- NULL
    gated <- dml_gate(without_groups, rep("all", nrow(fit$data$x)))
    expect_equal(gated$results$groups, fit$results$groups)
    expect_equal(gated$coefs$groups$ate.all, fit$coefs$main$all)
  }
})

test_that("all npm group targets inherit the fitted propensity-score bounds", {
  specifications <- list(
    list(target = c("ate", "att", "atu"), conditional = FALSE),
    list(target = "att", conditional = TRUE),
    list(target = "atu", conditional = TRUE)
  )
  trim <- list(lower = 0.1, upper = 0.85)
  for (specification in specifications) {
    fit <- do.call(new_trim_fixture, c(list(ps.trim = trim), specification))
    for (group in fit$results$groups) {
      for (repetition in group) {
        expect_equal(repetition$trim.summary$trim, trim)
      }
    }
  }
})

test_that("dml_gate() recovers trimming from older fits and permits an override", {
  fit <- new_trim_fixture(ps.trim = 0.1)
  fit$info$ps.trim <- NULL
  groups <- rep("all", nrow(fit$data$x))
  recovered <- dml_gate(fit, groups)
  expect_equal(recovered$coefs$groups$ate.all, fit$coefs$main$all)

  overridden <- dml_gate(fit, groups, trim = list(lower = 0.15, upper = 0.8))
  for (repetition in overridden$results$groups$ate.all) {
    expect_equal(repetition$trim.summary$trim, list(lower = 0.15, upper = 0.8))
  }
})
