# sensemakr() bounds on omitted variable bias, split into two tables:
# the postulated scenario (cf.y/cf.d/rho2, one row per target, groups
# included) and the benchmark scenarios (one row per multiplier per
# covariate, both the fixed-benchmark and benchmark-uncertainty CIs).

library(testthat)
library(dml.sensemakr)

data("pension", package = "dml.sensemakr")
set.seed(1); i <- sample(nrow(pension), 400)
y <- pension$net_tfa[i]; d <- pension$e401[i]
x <- model.matrix(~ -1 + age + inc + educ + fsize, data = pension[i, ])

fit1 <- dml(y, d, x, model = "plm", cf.folds = 2, cf.reps = 1, cf.seed = 7,
            verbose = FALSE)

test_that("sensemakr builds the postulated and benchmark tables", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = 1:2)))

  # postulated table: user's parameters, matches confidence_bounds()
  b <- s$bounds
  expect_s3_class(b, "dml_ovb_bounds")
  expect_equal(nrow(b), 1L)
  expect_equal(b$rho2, 1)
  expect_equal(b$bound.label, "Confounding Scenario")
  expect_equal(b$cf.y, 0.03)
  cb <- confidence_bounds(fit1, cf.y = 0.03, cf.d = 0.03, rho2 = 1)
  expect_equal(b$lwr, unname(cb["ate", "lwr"]))
  expect_equal(b$upr, unname(cb["ate", "upr"]))

  # benchmark table: one row per multiplier, both CI pairs, matches
  # benchmark_bounds() at the same k
  bt <- s$bench.table
  expect_s3_class(bt, "dml_bench_bounds")
  expect_equal(bt$target, c("ate", "ate"))
  expect_equal(bt$bound.label, c("1x inc", "2x inc"))
  expect_equal(bt$cf.y[2], 2 * bt$cf.y[1])
  expect_equal(bt$cf.d[2], 2 * bt$cf.d[1])
  bb1 <- as.data.frame(benchmark_bounds(fit1, s$bench.bounds, kY = 1, kD = 1))
  expect_equal(bt$lwr.fixed[1], bb1$lwr.fixed)
  expect_equal(bt$upr.fixed[1], bb1$upr.fixed)
  expect_equal(bt$lwr[1], bb1$lwr)          # propagated pair now shown too
  expect_equal(bt$upr[1], bb1$upr)
  expect_true(bt$lwr[1] <= bt$lwr.fixed[1]) # propagated is wider

  out <- capture.output(print(s))
  expect_true(any(grepl("postulated scenario", out)))
  expect_true(any(grepl("Benchmark bounds", out)))
})

test_that("asymmetric multipliers get an unambiguous label", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = c(1, 1), kY = c(1, 3))))
  expect_equal(s$bench.table$bound.label, c("1x inc", "3xY/1xD inc"))
  expect_equal(s$bench.table$cf.y[2], 3 * s$bench.table$cf.y[1])
  expect_equal(s$bench.table$cf.d[2], s$bench.table$cf.d[1])
})

test_that("a diverging multiplier shows infinite bounds", {
  s <- suppressWarnings(suppressMessages(
    sensemakr(fit1, benchmark_covariates = "inc",
              cf.y = 0.03, cf.d = 0.03, kD = 100)))
  row <- s$bench.table[s$bench.table$bound.label == "100x inc", ]
  expect_equal(row$lwr.fixed, -Inf)
  expect_equal(row$upr.fixed, Inf)
})

test_that("multi-target fits warn and keep the postulated rows per target", {
  fit2 <- dml(y, d, x, model = "npm", target = c("ate", "att"),
              cf.folds = 2, cf.reps = 1, cf.seed = 7, verbose = FALSE)
  expect_warning(
    s2 <- suppressMessages(sensemakr(fit2, benchmark_covariates = "inc",
                                     cf.y = 0.03, cf.d = 0.03)),
    "one target at a time")
  expect_equal(sort(s2$bounds$target), c("ate", "att"))
  expect_null(s2$bench.table)
  expect_null(s2$bench.bounds)
})

test_that("no cf.y and no benchmarks means no bounds tables", {
  s <- sensemakr(fit1)
  expect_null(s$bounds)
  expect_null(s$bench.table)
  expect_false(any(grepl("Bounds on omitted variable bias",
                         capture.output(print(s)))))
})
