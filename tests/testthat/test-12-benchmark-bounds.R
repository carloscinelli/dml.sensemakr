# Test benchmark_bounds(): covariate-benchmark bounds that propagate the
# estimation uncertainty of the benchmarked gains (Appendix E).
#
# Note: like dml_benchmark(), benchmark_bounds() re-evaluates the dml() call via
# eval(model.call), so y/d/x must live in the global environment.

library(testthat)
library(dml.sensemakr)

data("pension", package = "dml.sensemakr")
set.seed(303)
idx <- sample(nrow(pension), 400)
# Assign to global environment so eval(model.call) inside dml_benchmark can find them
assign("y", pension$net_tfa[idx], envir = .GlobalEnv)
assign("d", pension$e401[idx], envir = .GlobalEnv)
assign("x", model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                         data = pension[idx, ]), envir = .GlobalEnv)
bb_fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, cf.seed = 123, verbose = FALSE)

# === structure ===
test_that("benchmark_bounds returns a dml_benchmark_bounds data frame", {
  bb <- benchmark_bounds(bb_fit, benchmark = "inc")
  expect_s3_class(bb, "dml_benchmark_bounds")
  expect_true(is.data.frame(bb))
  expect_equal(colnames(bb),
               c("BF", "theta.minus", "theta.plus", "lwr.fixed", "upr.fixed",
                 "lwr", "upr", "se.minus", "se.plus"))
})

# === valid, ordered bounds ===
test_that("benchmark_bounds point bounds and confidence bounds are ordered", {
  bb <- benchmark_bounds(bb_fit, benchmark = "inc")
  expect_true(bb["inc", "BF"] >= 0)
  expect_true(bb["inc", "theta.minus"] <= bb["inc", "theta.plus"])
  # the confidence bounds bracket the point bounds on each side
  expect_true(bb["inc", "lwr"] <= bb["inc", "theta.minus"])
  expect_true(bb["inc", "upr"] >= bb["inc", "theta.plus"])
  expect_true(bb["inc", "lwr.fixed"] <= bb["inc", "theta.minus"])
  expect_true(bb["inc", "upr.fixed"] >= bb["inc", "theta.plus"])
  expect_true(bb["inc", "se.minus"] > 0 && bb["inc", "se.plus"] > 0)
})

# === reuse a precomputed dml_benchmark object ===
test_that("benchmark_bounds accepts a dml_benchmark object", {
  bench <- dml_benchmark(bb_fit, benchmark_covariates = "inc")
  bb <- benchmark_bounds(bb_fit, benchmark = bench)
  expect_s3_class(bb, "dml_benchmark_bounds")
})

# === divergence when kD * gain.D >= 1 ===
test_that("benchmark_bounds returns NA with a warning when kD * gain.D >= 1", {
  expect_warning(bb <- benchmark_bounds(bb_fit, benchmark = "inc", kD = 100),
                 "diverges")
  expect_true(is.na(bb["inc", "lwr"]))
  expect_true(is.na(bb["inc", "upr"]))
})

# === input validation ===
test_that("benchmark_bounds rejects rho2 outside [0, 1]", {
  expect_error(benchmark_bounds(bb_fit, benchmark = "inc", rho2 = 2))
})

# === print method ===
test_that("print.dml_benchmark_bounds does not error", {
  bb <- benchmark_bounds(bb_fit, benchmark = "inc")
  expect_output(print(bb), "benchmark bounds")
})

# Clean up global environment
rm(y, d, x, envir = .GlobalEnv)
