# Test dml_gate(): add group ATEs to an already-fitted dml object.

library(testthat)
library(dml.sensemakr)

# An inline groups= fit and a groups-free fit share a seed, so the groups added
# post hoc by dml_gate() should reproduce the inline group estimates exactly.
setup_gate <- local({
  data("pension", package = "dml.sensemakr")
  set.seed(606)
  idx <- sample(nrow(pension), 400)
  y <- pension$net_tfa[idx]
  d <- pension$e401[idx]
  x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown,
                     data = pension[idx, ])
  g <- cut(x[, "inc"], quantile(x[, "inc"], c(0, 0.5, 1), na.rm = TRUE),
           labels = c("low", "high"), include.lowest = TRUE)
  fit_inline <- dml(y, d, x, model = "npm", groups = g, cf.folds = 2, cf.reps = 1,
                    cf.seed = 123, verbose = FALSE)
  fit_nogrp  <- dml(y, d, x, model = "npm", cf.folds = 2, cf.reps = 1,
                    cf.seed = 123, verbose = FALSE)
  list(fit_inline = fit_inline, fit_nogrp = fit_nogrp, g = g)
})

# === dml_gate attaches group results to a fitted object ===
test_that("dml_gate() attaches group ATEs to a fitted dml object", {
  gated <- dml_gate(setup_gate$fit_nogrp, groups = setup_gate$g)
  expect_s3_class(gated, "dml")
  expect_false(is.null(gated$coefs$groups))
  expect_equal(names(gated$coefs$groups), levels(setup_gate$g))
})

# === dml_gate matches the inline dml(groups=) path ===
test_that("dml_gate() reproduces the inline groups= estimates", {
  gated <- dml_gate(setup_gate$fit_nogrp, groups = setup_gate$g)
  inline_coefs <- setup_gate$fit_inline$coefs$groups
  gated_coefs  <- gated$coefs$groups
  expect_equal(names(gated_coefs), names(inline_coefs))
  for (lev in names(inline_coefs)) {
    expect_equal(gated_coefs[[lev]], inline_coefs[[lev]], tolerance = 1e-6)
  }
})
