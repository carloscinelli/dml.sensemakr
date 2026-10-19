test_that("rigorous lasso fits a partially linear effect", {
  withr::local_seed(10)
  x <- matrix(rnorm(1000), ncol = 4)
  d <- x[, 1] + rnorm(nrow(x))
  y <- 2 * d + x[, 2] + rnorm(nrow(x))
  expect_no_warning(
    fit <- dml(y, d, x, model = "plm", reg = "rlasso",
               cf.folds = 2, cf.seed = 10, verbose = FALSE, warnings = TRUE)
  )
  expect_s3_class(fit, "dml")
  expect_equal(unname(coef(fit)), 2, tolerance = 0.3)
  expect_true(all(is.finite(se(fit))))
})
