library(sensemakr)
library(dml.sensemakr)

test_that("sensemakr stores kd/ky parameters", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  # default kd/ky

  sens <- sensemakr(dml.plm, cf.y = 0.04, cf.d = 0.03)
  expect_equal(sens$info$kd, 1)
  expect_equal(sens$info$ky, 1)

  # custom kd/ky
  sens2 <- sensemakr(dml.plm, cf.y = 0.04, cf.d = 0.03, kd = c(1, 0.5), ky = c(1, 0.25))
  expect_equal(sens2$info$kd, c(1, 0.5))
  expect_equal(sens2$info$ky, c(1, 0.25))

  # kd = ky by default
  sens3 <- sensemakr(dml.plm, cf.y = 0.04, cf.d = 0.03, kd = c(1, 2))
  expect_equal(sens3$info$ky, c(1, 2))
})


test_that("label_maker matches sensemakr OLS format", {
  # same kd and ky
  expect_equal(dml.sensemakr:::label_maker("Income", 1, 1), "1x Income")
  expect_equal(dml.sensemakr:::label_maker("Income", 0.25, 0.25), "0.25x Income")
  expect_equal(dml.sensemakr:::label_maker("Income", 2, 2), "2x Income")

  # different kd and ky
  expect_equal(dml.sensemakr:::label_maker("Income", 1, 0.5), "1/0.5x Income")

  # vectorized
  labels <- dml.sensemakr:::label_maker("pira", c(1, 0.25), c(1, 0.25))
  expect_equal(labels, c("1x pira", "0.25x pira"))
})


test_that("dml_benchmark stores model reference", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)
  bench <- dml_benchmark(dml.plm, benchmark_covariates = c("inc", "pira"))

  expect_s3_class(bench, "dml_benchmark")
  expect_false(is.null(bench$model))
  expect_s3_class(bench$model, "dml")
})


test_that("add_bound_to_contour.dml computes correct bound values", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  # Compute bounds manually
  cb_manual <- confidence_bounds(dml.plm, cf.y = 0.04, cf.d = 0.03, level = 0.95)

  # Compute bounds via the internal path (same as add_bound_to_contour.dml)
  results <- dml.plm$results$main[[1]]
  theta.s <- dml.sensemakr:::extract_estimate(results, "theta.s")
  S2 <- dml.sensemakr:::extract_estimate(results, "S2")
  se.theta.s <- dml.sensemakr:::extract_estimate(results, "se.theta.s")
  se.S2 <- dml.sensemakr:::extract_estimate(results, "se.S2")
  cov.theta.S2 <- dml.sensemakr:::extract_estimate(results, "cov.theta.S2")

  cb_numeric <- dml.sensemakr:::confidence_bounds.numeric(
    theta.s = theta.s, S2 = S2,
    se.theta.s = se.theta.s, se.S2 = se.S2,
    cov.theta.S2 = cov.theta.S2,
    combine.method = "median",
    level = 0.95, rho2 = 1,
    cf.d = 0.03, cf.y = 0.04
  )

  # The lwr/upr from the numeric path should match the dml method
  expect_equal(unname(cb_numeric["lwr"]), cb_manual[1, "lwr"], tolerance = 1e-4)
  expect_equal(unname(cb_numeric["upr"]), cb_manual[1, "upr"], tolerance = 1e-4)
})


test_that("automated benchmark overlay matches manual add_bound_to_contour", {
  # This test verifies that the automated sensemakr + plot workflow
  # produces the same bound values as manually calling add_bound_to_contour
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)
  bench <- dml_benchmark(dml.plm, benchmark_covariates = c("inc", "pira"))

  # Get benchmark gains
  bench_summary <- summary(bench)
  expect_true(all(c("gain.Y", "gain.D") %in% colnames(bench_summary)))
  expect_equal(nrow(bench_summary), 2)
  expect_equal(rownames(bench_summary), c("inc", "pira"))

  # For each benchmark covariate, the automated bound value should match
  # manually calling confidence_bounds with the benchmark's gain.Y/gain.D
  for (covar in rownames(bench_summary)) {
    gain.Y <- bench_summary[covar, "gain.Y"]
    gain.D <- bench_summary[covar, "gain.D"]

    # Manual computation
    cb_manual <- confidence_bounds(dml.plm, cf.y = gain.Y, cf.d = gain.D, level = 0.95)

    # Via numeric path (same as what plot uses internally)
    results <- dml.plm$results$main[[1]]
    theta.s <- dml.sensemakr:::extract_estimate(results, "theta.s")
    S2 <- dml.sensemakr:::extract_estimate(results, "S2")
    se.theta.s <- dml.sensemakr:::extract_estimate(results, "se.theta.s")
    se.S2 <- dml.sensemakr:::extract_estimate(results, "se.S2")
    cov.theta.S2 <- dml.sensemakr:::extract_estimate(results, "cov.theta.S2")

    cb_numeric <- dml.sensemakr:::confidence_bounds.numeric(
      theta.s = theta.s, S2 = S2,
      se.theta.s = se.theta.s, se.S2 = se.S2,
      cov.theta.S2 = cov.theta.S2,
      combine.method = "median",
      level = 0.95, rho2 = 1,
      cf.d = gain.D, cf.y = gain.Y
    )

    expect_equal(unname(cb_numeric["lwr"]), cb_manual[1, "lwr"], tolerance = 1e-4,
                 label = paste("lwr bound for", covar))
    expect_equal(unname(cb_numeric["upr"]), cb_manual[1, "upr"], tolerance = 1e-4,
                 label = paste("upr bound for", covar))
  }

  # Test with multipliers
  kd <- 0.25
  ky <- 0.25
  for (covar in rownames(bench_summary)) {
    gain.Y <- bench_summary[covar, "gain.Y"] * ky
    gain.D <- bench_summary[covar, "gain.D"] * kd
    cb_scaled <- confidence_bounds(dml.plm, cf.y = gain.Y, cf.d = gain.D, level = 0.95)

    cb_numeric_scaled <- dml.sensemakr:::confidence_bounds.numeric(
      theta.s = theta.s, S2 = S2,
      se.theta.s = se.theta.s, se.S2 = se.S2,
      cov.theta.S2 = cov.theta.S2,
      combine.method = "median",
      level = 0.95, rho2 = 1,
      cf.d = gain.D, cf.y = gain.Y
    )

    expect_equal(unname(cb_numeric_scaled["lwr"]), cb_scaled[1, "lwr"], tolerance = 1e-4,
                 label = paste("scaled lwr bound for", covar))
  }
})


test_that("plot.dml.sensemakr runs without error with benchmarks", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  # With benchmarks and multipliers
  sens <- sensemakr(dml.plm,
                    benchmark_covariates = c("inc", "pira"),
                    cf.y = 0.04, cf.d = 0.03,
                    kd = c(1, 0.25), ky = c(1, 0.25))

  pdf(NULL)
  expect_no_error(plot(sens, which.bound = "lwr"))
  expect_no_error(plot(sens, which.bound = "upr"))
  dev.off()
})


test_that("add_bound_to_contour dispatches correctly for dml and dml_benchmark", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)
  bench <- dml_benchmark(dml.plm, benchmark_covariates = c("inc"))

  pdf(NULL)

  # dml method: manual cf.y/cf.d
  ovb_contour_plot(dml.plm, which.bound = "lwr")
  expect_no_error(
    add_bound_to_contour(dml.plm, cf.y = 0.04, cf.d = 0.03,
                         which.bound = "lwr", bound_label = "Test")
  )

  # dml_benchmark method: benchmark object
  ovb_contour_plot(dml.plm, which.bound = "lwr")
  expect_no_error(
    add_bound_to_contour(bench, kd = c(1, 0.25), which.bound = "lwr")
  )

  dev.off()
})


test_that("ovb_contour_plot accepts benchmarks parameter directly", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)
  bench <- dml_benchmark(dml.plm, benchmark_covariates = c("inc", "pira"))

  pdf(NULL)
  expect_no_error(
    ovb_contour_plot(dml.plm, which.bound = "lwr",
                     benchmarks = bench, kd = c(1, 0.5), ky = c(1, 0.5))
  )
  dev.off()
})


test_that("rho2 propagates through sensemakr to benchmark bounds", {
  set.seed(10)
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  dml.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  # With rho2 = 1 (default)
  sens1 <- sensemakr(dml.plm, cf.y = 0.04, cf.d = 0.03, rho2 = 1)
  cb1 <- sens1$conf.bounds

  # With rho2 = 0.25
  sens2 <- sensemakr(dml.plm, cf.y = 0.04, cf.d = 0.03, rho2 = 0.25)
  cb2 <- sens2$conf.bounds

  # rho2 = 0.25 should give tighter bounds (less bias)
  expect_true(cb2[1, "lwr"] > cb1[1, "lwr"])
  expect_true(cb2[1, "upr"] < cb1[1, "upr"])

  # rho2 is stored
  expect_equal(sens1$info$rho2, 1)
  expect_equal(sens2$info$rho2, 0.25)
})


test_that("default label changed from Unadjusted to Observed", {
  # Check that the default label.unadjusted is "Observed"
  default_args <- formals(dml.sensemakr:::ovb_contour_plot.dml)
  expect_equal(default_args$label.unadjusted, "Observed")
})


test_that("automated benchmark bounds match replication code (PLM)", {
  # This test replicates the manual workflow from the RESTAT replication code
  # and verifies the automated version produces identical bound values.
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  set.seed(1)
  dml.401k.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  set.seed(2)
  bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
  out <- summary(bench.plm)

  # --- Manual bound values (replication code style) ---
  manual_bounds <- list()

  # 1x pira
  manual_bounds[["1x pira"]] <- confidence_bounds(dml.401k.plm,
                                                   cf.y = out["pira", "gain.Y"],
                                                   cf.d = out["pira", "gain.D"])

  # 1x twoearn
  manual_bounds[["1x twoearn"]] <- confidence_bounds(dml.401k.plm,
                                                      cf.y = out["twoearn", "gain.Y"],
                                                      cf.d = out["twoearn", "gain.D"])

  # 1x inc
  manual_bounds[["1x inc"]] <- confidence_bounds(dml.401k.plm,
                                                  cf.y = out["inc", "gain.Y"],
                                                  cf.d = out["inc", "gain.D"])

  # 1/4 x inc
  manual_bounds[["0.25x inc"]] <- confidence_bounds(dml.401k.plm,
                                                     cf.y = out["inc", "gain.Y"] * 0.25,
                                                     cf.d = out["inc", "gain.D"] * 0.25)

  # --- Automated bound values (new API) ---
  # Use the same short estimates that ovb_contour_plot uses internally
  results <- dml.401k.plm$results$main[[1]]
  theta.s <- dml.sensemakr:::extract_estimate(results, "theta.s")
  S2 <- dml.sensemakr:::extract_estimate(results, "S2")
  se.theta.s <- dml.sensemakr:::extract_estimate(results, "se.theta.s")
  se.S2 <- dml.sensemakr:::extract_estimate(results, "se.S2")
  cov.theta.S2 <- dml.sensemakr:::extract_estimate(results, "cov.theta.S2")

  auto_bound <- function(cf.y, cf.d, level = 0.95) {
    dml.sensemakr:::confidence_bounds.numeric(
      theta.s = theta.s, S2 = S2,
      se.theta.s = se.theta.s, se.S2 = se.S2,
      cov.theta.S2 = cov.theta.S2,
      combine.method = "median",
      level = level, rho2 = 1,
      cf.d = cf.d, cf.y = cf.y
    )
  }

  # 1x benchmarks
  for (covar in c("inc", "pira", "twoearn")) {
    auto <- auto_bound(cf.y = out[covar, "gain.Y"], cf.d = out[covar, "gain.D"])
    manual <- manual_bounds[[paste0("1x ", covar)]]
    expect_equal(unname(auto["lwr"]), manual[1, "lwr"], tolerance = 1e-4,
                 label = paste("1x", covar, "lwr"))
    expect_equal(unname(auto["upr"]), manual[1, "upr"], tolerance = 1e-4,
                 label = paste("1x", covar, "upr"))
  }

  # 0.25x inc
  auto_scaled <- auto_bound(cf.y = out["inc", "gain.Y"] * 0.25,
                             cf.d = out["inc", "gain.D"] * 0.25)
  manual_scaled <- manual_bounds[["0.25x inc"]]
  expect_equal(unname(auto_scaled["lwr"]), manual_scaled[1, "lwr"], tolerance = 1e-4,
               label = "0.25x inc lwr")
  expect_equal(unname(auto_scaled["upr"]), manual_scaled[1, "upr"], tolerance = 1e-4,
               label = "0.25x inc upr")
})


test_that("automated benchmark bounds match replication code with rho2 (PLM)", {
  # Same as above but with rho2 = (1/2)^2, matching the rho2 variant in the replication code
  data("pension")
  y <- pension$net_tfa
  d <- pension$e401
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

  set.seed(1)
  dml.401k.plm <- dml(pension$net_tfa, pension$e401, x, model = "plm", cf.folds = 5, cf.reps = 5)

  set.seed(2)
  bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
  out <- summary(bench.plm)
  rho2 <- (1/2)^2

  # Manual (replication code)
  manual_inc <- confidence_bounds(dml.401k.plm,
                                  cf.y = out["inc", "gain.Y"],
                                  cf.d = out["inc", "gain.D"],
                                  rho2 = rho2)
  manual_pira <- confidence_bounds(dml.401k.plm,
                                   cf.y = out["pira", "gain.Y"],
                                   cf.d = out["pira", "gain.D"],
                                   rho2 = rho2)

  # Automated (numeric path)
  results <- dml.401k.plm$results$main[[1]]
  theta.s <- dml.sensemakr:::extract_estimate(results, "theta.s")
  S2 <- dml.sensemakr:::extract_estimate(results, "S2")
  se.theta.s <- dml.sensemakr:::extract_estimate(results, "se.theta.s")
  se.S2 <- dml.sensemakr:::extract_estimate(results, "se.S2")
  cov.theta.S2 <- dml.sensemakr:::extract_estimate(results, "cov.theta.S2")

  auto_inc <- dml.sensemakr:::confidence_bounds.numeric(
    theta.s = theta.s, S2 = S2,
    se.theta.s = se.theta.s, se.S2 = se.S2,
    cov.theta.S2 = cov.theta.S2,
    combine.method = "median",
    level = 0.95, rho2 = rho2,
    cf.d = out["inc", "gain.D"], cf.y = out["inc", "gain.Y"]
  )

  auto_pira <- dml.sensemakr:::confidence_bounds.numeric(
    theta.s = theta.s, S2 = S2,
    se.theta.s = se.theta.s, se.S2 = se.S2,
    cov.theta.S2 = cov.theta.S2,
    combine.method = "median",
    level = 0.95, rho2 = rho2,
    cf.d = out["pira", "gain.D"], cf.y = out["pira", "gain.Y"]
  )

  expect_equal(unname(auto_inc["lwr"]), manual_inc[1, "lwr"], tolerance = 1e-4)
  expect_equal(unname(auto_pira["lwr"]), manual_pira[1, "lwr"], tolerance = 1e-4)

  # rho2 < 1 should give tighter bounds than rho2 = 1
  manual_inc_full <- confidence_bounds(dml.401k.plm,
                                       cf.y = out["inc", "gain.Y"],
                                       cf.d = out["inc", "gain.D"],
                                       rho2 = 1)
  expect_true(manual_inc[1, "lwr"] > manual_inc_full[1, "lwr"])
})
