# Regression tests for the PR #9 review findings.
#
# Each test pins a specific bug found in the review of the conditional ATT/ATU
# changes, so it cannot be silently reintroduced. Fixtures are shared across
# tests to keep the file fast.

library(testthat)
library(dml.sensemakr)

setup_rf <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(909)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  g <- factor(pension$marr[idx], labels = c("single", "married"))

  # NOTE: the learner spec deliberately lives in a variable named `yreg`.
  # dml_benchmark() re-evaluates this call, and bench_fun() once had a formal of
  # the same name that shadowed it (review finding 2); building the benchmark
  # fixture below fails outright if that regression comes back.
  yreg <- "ranger"
  fit.plm <- dml(y, d, x, model = "plm", yreg = yreg,
                 cf.folds = 2, cf.reps = 2, cf.seed = 123, verbose = FALSE)

  # conditional ATT on the non-tuned path (also exercises finding 12)
  fit.att <- dml(y, d, x, model = "npm", target = "att",
                 dirty.tuning = FALSE, save.models = TRUE,
                 cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE)

  # npm ATE with groups (for the alpha = 1 closed form on group rows)
  fit.grp <- dml(y, d, x, model = "npm", groups = g,
                 cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE)

  # npm ATT with groups: stays conditional, but trains both arms for the groups
  fit.attg <- dml(y, d, x, model = "npm", target = "att", groups = g,
                  cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE)

  # benchmark on two covariates; y/d/x resolve through the stored call
  # environment (this local() frame), with no .GlobalEnv workaround
  bench2 <- dml_benchmark(fit.plm, c("age", "inc"))

  list(y = y, d = d, x = x, g = g,
       fit.plm = fit.plm, fit.att = fit.att, fit.grp = fit.grp,
       fit.attg = fit.attg, bench2 = bench2)
})

# a copy of the benchmark with known positive gains, so the k-multiplier tests
# are deterministic (real gains on a small subsample can be <= 0)
bench_pos <- local({
  b <- setup_rf$bench2
  for (v in names(b$benchmarks)) {
    b$benchmarks[[v]]$gain.Y[] <- 0.2
    b$benchmarks[[v]]$gain.D[] <- 0.3
    b$benchmarks[[v]]$rho[]    <- 0.5
  }
  b
})

# === finding 1: info$yreg keeps its yreg0/yreg1 structure on the plm path ===
test_that("plm fit with dirty tuning keeps the yreg0/yreg1 record and summary reports the learner", {
  fit <- setup_rf$fit.plm
  expect_named(fit$info$yreg, c("yreg0", "yreg1"))
  out <- paste(capture.output(print(summary(fit))), collapse = "\n")
  expect_match(out, "yreg0:ranger")
  expect_no_match(out, "(not used)", fixed = TRUE)
})

# === finding 2: dml_benchmark works when the user's variable is named yreg ===
test_that("dml_benchmark resolves the stored call in the user's environment", {
  # the fixture itself was built with a local variable named `yreg`; this
  # asserts the result is sound, not just that construction did not error
  expect_s3_class(setup_rf$bench2, "dml_benchmark")
  s <- summary(setup_rf$bench2)
  expect_true(all(is.finite(s$benchmarks[, "gain.Y"])))
})

# === finding 3: plm + target 'att' warns, falls back to ATE, and prints ===
test_that("plm with target 'att' warns and reports the ATE instead of crashing", {
  expect_warning(
    fit <- dml(setup_rf$y, setup_rf$d, setup_rf$x, model = "plm", target = "att",
               cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE),
    "cannot target 'att'"
  )
  expect_identical(fit$info$target, "ate")
  expect_true("ate" %in% names(coef(fit)))
  expect_output(print(summary(fit)), "ate")
})

# === finding 4: na.rm in summary.dml_benchmark is honored again ===
test_that("summary.dml_benchmark drops NA repetitions when na.rm = TRUE", {
  b <- setup_rf$bench2
  b$benchmarks[["inc"]]$gain.Y[2] <- NA   # simulate one failed repetition
  s.rm   <- summary(b, na.rm = TRUE)
  s.keep <- summary(b, na.rm = FALSE)
  expect_true(is.finite(s.rm$benchmarks["inc", "gain.Y"]))
  expect_true(is.na(s.keep$benchmarks["inc", "gain.Y"]))
})

# === finding 5: conditional fits carry only the requested estimand ===
test_that("conditional ATT fit has no stale unconditional 'all' slot", {
  fit <- setup_rf$fit.att
  expect_named(fit$results$main, "treat")
  expect_named(fit$coefs$main, "treat")
  cb <- confidence_bounds(fit, cf.y = 0.03, cf.d = 0.03)
  expect_identical(rownames(cb), "att")
  expect_true(all(is.finite(cb)))
  # asking for the never-estimated ATE now refuses instead of half-plotting
  expect_error(ovb_contour_plot(fit, parameter = "ate"), "ate")
})

# === finding 6: groups never silently downgrade the main parameterization ===
test_that("requesting groups keeps the conditional parameterization for the main target", {
  fit <- setup_rf$fit.attg
  # the recommended (conditional) parameterization survives a groups request ...
  expect_true(isTRUE(fit$info$conditional))
  # ... and the group rows report the ATT within each group, named g.att.<level>
  expect_named(fit$coefs$groups, c("att.single", "att.married"))
  gates <- sapply(fit$coefs$groups, function(z) z["median", "estimate"])
  expect_true(all(is.finite(gates)))
  expect_identical(names(coef(fit)), c("att", "g.att.single", "g.att.married"))
  expect_false(all(is.na(fit$fits[[1]]$preds$yhat1)))   # second arm trained

  # the main slot must match a conditional fit, NOT the unconditional one: nu2.s
  # differs by roughly a factor of five between the two parameterizations
  nu2 <- function(z) z$results$main$treat[[1]]$estimates$nu2.s
  uncond <- dml(setup_rf$y, setup_rf$d, setup_rf$x, model = "npm", target = "att",
                conditional = FALSE, groups = setup_rf$g, cf.folds = 2, cf.reps = 1,
                cf.seed = 123, verbose = FALSE)
  expect_equal(nu2(fit), nu2(setup_rf$fit.att), tolerance = 0.1)
  expect_gt(abs(nu2(uncond) - nu2(fit)), 1)
})

test_that("dml_gate still refuses a conditional fit with only one arm predicted", {
  # fit.att has no groups, so the treated arm was never trained: gates are
  # impossible and must error rather than come back NA
  expect_error(dml_gate(setup_rf$fit.att, groups = setup_rf$g), "both outcome arms")
})

# === both parameterizations stay available for reproduction ===
test_that("unconditional and centered parameterizations remain reachable", {
  # unconditional: Chernozhukov et al. (2026) parameterization
  uncond <- dml(setup_rf$y, setup_rf$d, setup_rf$x, model = "npm", target = "att",
                conditional = FALSE, cf.folds = 2, cf.reps = 1, cf.seed = 123,
                verbose = FALSE)
  expect_false(isTRUE(uncond$info$conditional))
  expect_true(is.finite(uncond$results$main$treat[[1]]$estimates$nu2.s))

  # centered: Huang and Pimentel (2025) parameterization, nu2 shifted by exactly 1
  cent <- dml(setup_rf$y, setup_rf$d, setup_rf$x, model = "npm", target = "att",
              centered = TRUE, cf.folds = 2, cf.reps = 1, cf.seed = 123,
              verbose = FALSE)
  expect_true(isTRUE(cent$info$centered))
  expect_equal(cent$results$main$treat[[1]]$estimates$nu2.s + 1,
               setup_rf$fit.att$results$main$treat[[1]]$estimates$nu2.s,
               tolerance = 1e-8)
})

# === finding 7: the alpha = 1 closed form honors rho2 ===
test_that("extreme_robustness_value at alpha = 1 scales with rho2", {
  fit  <- setup_rf$fit.plm
  xrv1 <- extreme_robustness_value(fit, alpha = 1)              # rho2 = 1
  xrv5 <- extreme_robustness_value(fit, alpha = 1, rho2 = 0.5)
  f02  <- xrv1[["ate"]] / (1 - xrv1[["ate"]])
  expect_gt(xrv5[["ate"]], xrv1[["ate"]])
  expect_equal(unname(xrv5[["ate"]]),
               (f02 / 0.5) / (1 + f02 / 0.5), tolerance = 1e-8)
})

# === finding 8: the alpha = 1 closed form works on fits with groups ===
test_that("extreme_robustness_value at alpha = 1 handles gate rows", {
  # must not error on gate rows (it used to crash); a gate whose estimated S2
  # comes out non-positive on this small subsample is NA with a warning
  xrv <- suppressWarnings(extreme_robustness_value(setup_rf$fit.grp, alpha = 1))
  expect_true(all(c("ate", "g.ate.single", "g.ate.married") %in% names(xrv)))
  expect_true(is.finite(xrv[["ate"]]))
  ok <- is.na(xrv) | (xrv >= 0 & xrv <= 1)
  expect_true(all(ok))
})

# === finding 9: XRV is exactly 0 when theta is inside the interval ===
test_that("extreme_robustness_value short-circuits to an exact 0", {
  fit <- setup_rf$fit.plm
  theta.inside <- mean(confint(fit)["ate", ])
  xrv <- extreme_robustness_value(fit, theta = theta.inside, alpha = 0.05)
  # exact zero, not the ~1e-8 boundary value a fall-through optimization leaves
  expect_identical(unname(xrv[["ate"]]), 0)
})

# === finding 10: kY = 0 / kD = 0 collapse to the plain CI, not NaN ===
test_that("benchmark_bounds with a zero multiplier returns the plain CI", {
  for (arg in list(list(kY = 0), list(kD = 0))) {
    bb <- do.call(benchmark_bounds,
                  c(list(setup_rf$fit.plm, bench_pos), arg))
    expect_true(all(is.finite(bb$lwr)) && all(is.finite(bb$upr)))
    expect_equal(bb$lwr, bb$lwr.fixed)
    expect_equal(bb$upr, bb$upr.fixed)
    expect_true(all(bb$BF == 0))
  }
  # sanity: with k = 1 the mutated benchmark gives a strictly positive BF
  bb1 <- benchmark_bounds(setup_rf$fit.plm, bench_pos)
  expect_true(all(bb1$BF > 0))
})

# === finding 11: per-covariate kY/kD vectors are applied per covariate ===
test_that("benchmark_bounds accepts one k per benchmark covariate", {
  bb <- benchmark_bounds(setup_rf$fit.plm, bench_pos, kY = c(0, 1))
  expect_true(bb["age", "BF"] == 0)   # kY = 0 applies to the first covariate
  expect_true(bb["inc", "BF"] > 0)    # kY = 1 to the second
  bb2 <- benchmark_bounds(setup_rf$fit.plm, bench_pos, kD = c(1, 0.5))
  expect_true(bb2["age", "BF"] > bb2["inc", "BF"])
  expect_output(print(bb2), "kD = 1, 0.5", fixed = TRUE)
  # a wrong-length vector is a clear error, not recycling
  expect_error(benchmark_bounds(setup_rf$fit.plm, bench_pos, kY = c(1, 2, 3)),
               "one per benchmark covariate")
})

# === finding 12: dirty.tuning = FALSE also skips the unused outcome arm ===
test_that("conditional ATT never trains the treated-arm outcome model", {
  # no groups requested, so the second arm is genuinely unused
  fit <- setup_rf$fit.att   # dirty.tuning = FALSE, save.models = TRUE
  expect_true(all(is.na(fit$fits[[1]]$preds$yhat1)))
  expect_null(fit$fits[[1]]$model.y1[[1]])
  expect_null(fit$info$yreg$yreg1)
})

# === review round 2: plots follow the fit's own target ===
test_that("contour and sensemakr plots work on conditional and grouped fits", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  # default parameter resolves to the fit's own target (no "ate" slot needed)
  expect_no_error(ovb_contour_plot(setup_rf$fit.att, cf.y = 0.03, cf.d = 0.03))
  expect_no_error(plot(sensemakr(setup_rf$fit.att, cf.y = 0.03, cf.d = 0.03)))
  # explicitly requesting a never-estimated target still refuses clearly
  expect_error(ovb_contour_plot(setup_rf$fit.att, parameter = "ate"), "ate")
  # group plots do not require the (absent) main "ate" slot
  expect_no_error(ovb_contour_plot(setup_rf$fit.attg, group = TRUE, group.number = 1))
})

# === review round 2: dml() keeps the 0.2.0 positional argument order ===
test_that("conditional/centered come after all 0.2.0 parameters", {
  f <- names(formals(dml))
  expect_equal(f[1:16],
               c("y", "d", "x", "model", "target", "groups", "cf.folds",
                 "cf.reps", "cf.seed", "ps.trim", "reg", "yreg", "dreg",
                 "dirty.tuning", "save.models", "y.class"))
  expect_gt(match("conditional", f), match("d.class", f))
  expect_gt(match("centered", f), match("d.class", f))
})

# === finding 15: trimmed-observation bookkeeping keeps x a matrix ===
test_that("collect.trimmed.obs keeps a single trimmed row as a matrix", {
  tr <- dml.sensemakr:::collect.trimmed.obs(
    y = 1:5, d = c(0, 1, 0, 1, 0), x = matrix(1:10, 5, 2),
    trimmed_indices = list(all = 2L, low = 2L, high = integer(0))
  )
  expect_true(is.matrix(tr$all$x.trimmed))
  expect_identical(nrow(tr$all$x.trimmed), 1L)
  expect_identical(nrow(tr$high$x.trimmed), 0L)
})

# === review round 3: group rows follow every requested target ===
test_that("each requested target gets its own group block", {
  fit <- dml(setup_rf$y, setup_rf$d, setup_rf$x, model = "npm",
             target = c("att", "atu"), groups = setup_rf$g,
             cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE)
  expect_identical(names(coef(fit)),
                   c("att", "atu",
                     "g.att.single", "g.att.married",
                     "g.atu.single", "g.atu.married"))
  # no group ATE appears, because no ATE was requested
  expect_false(any(grepl("^g\\.ate\\.", names(coef(fit)))))
  # the "g." marker selects every group row
  expect_length(coef(fit)[grepl("^g\\.", names(coef(fit)))], 4L)
})

test_that("group rows carry the estimand and the level in their names", {
  fit <- setup_rf$fit.grp   # target = "ate"
  expect_identical(names(coef(fit)), c("ate", "g.ate.single", "g.ate.married"))
  out <- paste(capture.output(print(summary(fit))), collapse = "\n")
  expect_match(out, "Group Average Treatment Effect")
})
