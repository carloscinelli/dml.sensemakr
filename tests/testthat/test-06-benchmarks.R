# Test benchmark functions
#
# Fixtures retain their own call environment for benchmark refits.

new_bench_fit <- memoise::memoise(function() {
  pension <- dml.sensemakr::pension
  withr::local_seed(77)
  idx <- sample(nrow(pension), 500)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1,
      cf.seed = 123, verbose = FALSE)
})

test_that("dml_benchmark returns correct class", {
  bench_fit <- new_bench_fit()
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc"))
  expect_s3_class(bench, "dml_benchmark")
})

test_that("dml_benchmark with multiple covariates", {
  bench_fit <- new_bench_fit()
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc", "pira"))
  expect_s3_class(bench, "dml_benchmark")
})

test_that("summary.dml_benchmark does not error", {
  bench_fit <- new_bench_fit()
  bench <- dml_benchmark(bench_fit, benchmark_covariates = c("inc"))
  expect_output(print(summary(bench)), regexp = NULL)
})
