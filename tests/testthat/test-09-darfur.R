test_that("Darfur NPM supports factor covariates and benchmarking", {
  withr::local_seed(12)

  # loads dataset
  darfur <- droplevels(sensemakr::darfur)

  # runs regression model
  model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
                pastvoted + hhsize_darfur + female + village, data = darfur)
  # y <- as.numeric(darfur$peacefactor)
  # d <- darfur$directlyharmed
  x <- model.matrix(~ age + farmer_dar + herder_dar +
                      pastvoted + hhsize_darfur + female + village, data = darfur)

  dml.darfur <- dml(as.numeric(darfur$peacefactor),
                    darfur$directlyharmed,
                    x, model = "npm", cf.folds = 2, cf.reps = 5,
                    cf.seed = 12, verbose = FALSE)
  expect_s3_class(dml.darfur, "dml")
  expect_true(all(is.finite(coef(dml.darfur))))
  summary(dml.darfur)
  bench <- dml_benchmark(dml.darfur, benchmark_covariates = c("female"))
  expect_s3_class(bench, "dml_benchmark")

  expect_true(all(is.finite(robustness_value(dml.darfur))))
})
