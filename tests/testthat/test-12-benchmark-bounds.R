# Test benchmark_bounds(): covariate-benchmark bounds that propagate the
# estimation uncertainty of the benchmarked gains (Appendix E).
#
# Fixtures retain their own call environment for benchmark refits.

new_bb_fit <- memoise::memoise(function() {
  pension <- dml.sensemakr::pension
  withr::local_seed(303)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1,
      cf.seed = 123, verbose = FALSE)
})

# === structure ===
test_that("benchmark_bounds returns a dml_benchmark_bounds data frame", {
  bb_fit <- new_bb_fit()
  bb <- benchmark_bounds(bb_fit, benchmark = "inc")
  expect_s3_class(bb, "dml_benchmark_bounds")
  expect_true(is.data.frame(bb))
  expect_equal(colnames(bb),
               c("BF", "theta.minus", "theta.plus", "lwr.fixed", "upr.fixed",
                 "lwr", "upr", "se.minus", "se.plus"))
})

# === valid, ordered bounds ===
test_that("benchmark_bounds point bounds and confidence bounds are ordered", {
  bb_fit <- new_bb_fit()
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
  bb_fit <- new_bb_fit()
  bench <- dml_benchmark(bb_fit, benchmark_covariates = "inc")
  bb <- benchmark_bounds(bb_fit, benchmark = bench)
  expect_s3_class(bb, "dml_benchmark_bounds")
})

# === divergence when kD * gain.D >= 1 ===
test_that("benchmark_bounds returns -Inf/Inf with a warning when kD * gain.D >= 1", {
  bb_fit <- new_bb_fit()
  expect_warning(bb <- benchmark_bounds(bb_fit, benchmark = "inc", kD = 100),
                 "diverges")
  expect_equal(bb["inc", "lwr"], -Inf)
  expect_equal(bb["inc", "upr"], Inf)
  expect_equal(bb["inc", "theta.minus"], -Inf)
  expect_equal(bb["inc", "theta.plus"], Inf)
  expect_equal(bb["inc", "BF"], Inf)
  expect_true(is.na(bb["inc", "se.minus"]))
})

# === input validation ===
test_that("benchmark_bounds rejects rho2 outside [0, 1]", {
  bb_fit <- new_bb_fit()
  expect_error(benchmark_bounds(bb_fit, benchmark = "inc", rho2 = 2))
})

# === print method ===
test_that("print.dml_benchmark_bounds does not error", {
  bb_fit <- new_bb_fit()
  bb <- benchmark_bounds(bb_fit, benchmark = "inc")
  expect_output(print(bb), "benchmark bounds")
})
