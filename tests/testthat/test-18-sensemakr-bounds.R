# sensemakr() bounds-on-OVB table (issue #11): kY/kD multipliers, one table
# holding the manual scenario and one row per benchmark multiplier, printed
# under a single heading. Multi-target fits warn instead of silently dropping
# the benchmark.

library(testthat)
library(dml.sensemakr)

data("pension", package = "dml.sensemakr")
set.seed(1); i <- sample(nrow(pension), 400)
y <- pension$net_tfa[i]; d <- pension$e401[i]
x <- model.matrix(~ -1 + age + inc + educ + fsize, data = pension[i, ])

fit1 <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, cf.seed = 7,
            verbose = FALSE)

test_that("sensemakr builds one bounds table with manual and benchmark rows", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = 1:2)))

  b <- s$bounds
  expect_s3_class(b, "dml_ovb_bounds")
  expect_equal(nrow(b), 3L)                        # manual + 1x + 2x
  expect_equal(b$bound.label, c("Confounding Scenario", "1x inc", "2x inc"))

  # manual row carries the user's parameters and matches confidence_bounds()
  expect_equal(b$cf.y[1], 0.03)
  expect_equal(b$rho[1], 1)
  cb <- confidence_bounds(fit1, cf.y = 0.03, cf.d = 0.03, rho2 = 1)
  expect_equal(b$lwr[1], unname(cb["ate", "lwr"]))
  expect_equal(b$upr[1], unname(cb["ate", "upr"]))

  # benchmark rows scale the gains by the multiplier
  expect_equal(b$cf.y[3], 2 * b$cf.y[2])
  expect_equal(b$cf.d[3], 2 * b$cf.d[2])
  # and agree with benchmark_bounds() at the same k
  bb1 <- as.data.frame(benchmark_bounds(fit1, s$bench.bounds, kY = 1, kD = 1))
  expect_equal(b$lwr[2], bb1$lwr.fixed)
  expect_equal(b$upr[2], bb1$upr.fixed)

  out <- capture.output(print(s))
  expect_true(any(grepl("Bounds on omitted variable bias", out)))
  expect_false(any(grepl("Confidence Bounds for Sensitivity Scenario", out)))
})

test_that("asymmetric multipliers get an unambiguous label", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = c(1, 1), kY = c(1, 3))))
  expect_equal(s$bounds$bound.label[-1], c("1x inc", "3xY/1xD inc"))
  expect_equal(s$bounds$cf.y[3], 3 * s$bounds$cf.y[2])
  expect_equal(s$bounds$cf.d[3], s$bounds$cf.d[2])
})

test_that("a diverging multiplier shows infinite bounds in the table", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = 100)))
  b <- s$bounds
  expect_equal(b$lwr[b$bound.label == "100x inc"], -Inf)
  expect_equal(b$upr[b$bound.label == "100x inc"], Inf)
})

test_that("multi-target fits warn and keep the manual rows for every target", {
  fit2 <- dml(y, d, x, model = "npm", target = c("ate", "att"),
              cf.folds = 2, cf.reps = 1, cf.seed = 7, verbose = FALSE)
  expect_warning(
    s2 <- suppressMessages(sensemakr(fit2, benchmark_covariates = "inc",
                                     cf.y = 0.03, cf.d = 0.03)),
    "one target at a time")
  expect_equal(sort(s2$bounds$target), c("ate", "att"))
  expect_true(all(s2$bounds$bound.label == "Confounding Scenario"))
  expect_null(s2$bench.bounds)
})

test_that("no cf.y and no benchmarks means no bounds table", {
  s <- sensemakr(fit1)
  expect_null(s$bounds)
  expect_false(any(grepl("Bounds on omitted variable bias",
                         capture.output(print(s)))))
})
