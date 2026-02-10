# Test benchmark functions
#
# Note: dml_benchmark() re-evaluates the dml() call via eval(model.call).
# The eval happens in the package namespace, so variables from the stored call
# must be in the global environment.

library(testthat)
library(dml.sensemakr)

data("pension", package = "dml.sensemakr")
set.seed(77)
idx <- sample(nrow(pension), 500)
# Assign to global environment so eval(model.call) inside dml_benchmark can find them
assign("y", pension$net_tfa[idx], envir = .GlobalEnv)
assign("d", pension$e401[idx], envir = .GlobalEnv)
assign("x", model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                           data = pension[idx, ]), envir = .GlobalEnv)
bench_fit <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, verbose = FALSE)

test_that("dml_benchmark returns correct class", {
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc"))
  expect_s3_class(bench, "dml_benchmark")
})

test_that("dml_benchmark with multiple covariates", {
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc", "pira"))
  expect_s3_class(bench, "dml_benchmark")
})

test_that("summary.dml_benchmark does not error", {
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc"))
  expect_output(print(summary(bench)), regexp = NULL)
})

# Clean up global environment
rm(y, d, x, envir = .GlobalEnv)
