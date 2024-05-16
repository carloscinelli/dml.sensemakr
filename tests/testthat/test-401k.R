# loads package
library(dml.sensemakr)

## loads data
data("pension")
y <- pension$net_tfa
d <- pension$e401
x <- model.matrix(~ -1 + age + inc + educ+ fsize + marr + twoearn + pira + hown, data = pension)

## computes income quartiles
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)

test_that("Testing 401k PLM", {
  set.seed(10)
  dml.401k.plm <- dml(y, d, x, groups = g1, model = "plm", target = "ate")
  plot(dml.401k.plm)
  dml.401k.plm
  summary(dml.401k.plm)
  summary(dml.401k.plm, combine.method = "mean")
  coef(dml.401k.plm)
  coef(dml.401k.plm, combine.method = "mean")
  se(dml.401k.plm)
  se(dml.401k.plm, combine.method = "mean")
  confint(dml.401k.plm)
  confint(dml.401k.plm, combine.method = "mean")
  confidence_bounds(model = dml.401k.plm, cf.y = 0.03, cf.d = 0.04)
  confidence_bounds(model = dml.401k.plm, cf.y = 0.03, cf.d = 0.04, rho = 0,
                    combine.method = "mean",
                    level = 0.975)
  confint(dml.401k.plm, combine.method = "mean")
  ovb_contour_plot(dml.401k.plm)
  ovb_contour_plot(dml.401k.plm, cf.y = 0.03, cf.d = 0.04, rho2 = 1,
                   which.bound = "lwr",
                   bound.label = "Max Match (3x years)",
                   col.contour = "blue")
  dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc"))
  bounds <- dml_bounds(model = dml.401k.plm,  cf.y = 0.03, cf.d = 0.04, rho2 = 1)
  plot(bounds, type = "all")
})

