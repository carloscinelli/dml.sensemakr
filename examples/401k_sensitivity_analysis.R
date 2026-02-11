rm(list = ls())

# loads package
library(dml.sensemakr)
library(sensemakr)

## loads data
data("pension")
y <- pension$net_tfa # net total financial assets
d <- pension$e401 # 401K eligibility
x <- model.matrix(~ -1 + age + inc + educ+ fsize + marr + twoearn + pira + hown, data = pension)


# Naive estimate
mean(y[d==1]) - mean(y[d==0])


# Partially Linear Model
# run DML
set.seed(1)
dml.401k.plm <- dml(y, d, x, model = "plm", cf.folds = 5, cf.reps = 5)
# results under conditional ignorability
summary(dml.401k.plm)
confint(dml.401k.plm)

robustness_value(dml.401k.plm)


confidence_bounds(dml.401k.plm, cf.y=0.04, cf.d=0.03, level = 0)
confidence_bounds(dml.401k.plm, cf.y=0.04, cf.d=0.03)

(dml_bounds(dml.401k.plm, cf.y=0.04, cf.d=0.03))

summary(dml.sensemakr::sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03))

## compute income quartiles
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)

## Nonparametric model
set.seed(1)
dml.401k.npm <- dml(y, d, x, groups = g1, model = "npm", cf.folds = 5, cf.reps = 5)
summary(dml.401k.npm)
confint(dml.401k.npm)

options(scipen = 99)
robustness_value(dml.401k.npm)
summary(dml.sensemakr::sensemakr(dml.401k.npm, cf.y = 0.04, cf.d = 0.03))

(dml_bounds(dml.401k.npm, cf.y=0.04, cf.d=0.03))
confidence_bounds(dml.401k.npm, cf.y=0.04, cf.d=0.03, level = 0)
confidence_bounds(dml.401k.npm, cf.y=0.04, cf.d=0.03)


# load ggplot2
library(ggplot2)

# coefficient plot under conditional ignorability
group.names <- paste0("gate.q",1:4)
df   <- data.frame(groups = 1:4, estimate = coef(dml.401k.npm)[group.names])
cis  <- confint(dml.401k.npm)[group.names, ]
cis  <- setNames(as.data.frame(cis), c("lwr.ci", "upr.ci"))
df   <- cbind(df, cis)
ggplot(df, aes(x = groups, y = estimate)) + geom_line() +
  geom_ribbon(aes(ymin = lwr.ci, ymax = upr.ci), alpha = 0.1, col = "blue", fill = "blue") +
  theme_bw() +
  xlab("Income Groups by Quartiles") +
  ylab("ATE")


# confidence bounds plot
bds   <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = 0)
bds   <- setNames(as.data.frame(bds), c("lwr.bound", "upr.bound"))
cbds  <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = .95)
cbds  <- setNames(as.data.frame(cbds), c("lwr.cbound", "upr.cbound"))
df2   <- cbind(df, bds[-1,], cbds[-1, ])
ggplot(df2, aes(x = groups, y = estimate)) + geom_line() +
  geom_ribbon(aes(ymin = lwr.bound, ymax = upr.bound),   alpha = 0.1, col = "red", fill = "red") +
  geom_ribbon(aes(ymin = lwr.cbound, ymax = upr.cbound), alpha = 0.1, col = "blue", fill = "blue") +
  theme_bw() +
  xlab("Income Groups by Quartiles") +
  ylab("ATE")



# benchmarks PLM
set.seed(2)
bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
bench.plm


# contour plots
library(sensemakr)
out <- summary(bench.plm)
# sensitivity analysis
sens.401k <- dml.sensemakr::sensemakr(dml.401k.plm, cf.y = NULL)
plot(sens.401k,
     which.bound = "lwr",
     col.contour = "blue",
     label.bump.x = 0.015,
     label.bump.y = 0.003,
     lim.x = .16)
add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm,
                                                     cf.y = .04,
                                                     cf.d = .03),
                     point.pch = 25,
                     point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d =out["pira","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d =out["twoearn","gain.D"]),
                     label.bump.x = 0.02, label.bump.y = -0.002)
add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d =out["inc","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["inc","gain.D"]*.25,
                     r2yz.dx = out["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value =
                       confidence_bounds(dml.401k.plm,
                                         cf.y = out["inc","gain.Y"]*.25,
                                         cf.d =out["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = -0.005)

# sensitivity analysis with rho2
out <- summary(bench.plm)
rho2 <- (1/2)^2
sens.401k <- dml.sensemakr::sensemakr(dml.401k.plm, cf.y = NULL, rho2 = rho2)
plot(sens.401k,
     which.bound = "lwr",
     col.contour = "blue",
     label.bump.x = 0.015,
     label.bump.y = 0.003,
     lim.x = .16)
add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm,
                                                     cf.y = .04,
                                                     cf.d = .03, rho2= rho2),
                     point.pch = 25,
                     point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm,
                                                     cf.y = out["pira","gain.Y"],
                                                     cf.d =out["pira","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm,
                                                     cf.y = out["twoearn","gain.Y"],
                                                     cf.d =out["twoearn","gain.D"],
                                                     rho2 = rho2),
                     label.bump.x = 0.02, label.bump.y = -0.002)
add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm,
                                                     cf.y = out["inc","gain.Y"],
                                                     cf.d =out["inc","gain.D"],
                                                     rho2= rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["inc","gain.D"]*.25,
                     r2yz.dx = out["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value =
                       confidence_bounds(dml.401k.plm,
                                         cf.y = out["inc","gain.Y"]*.25,
                                         cf.d =out["inc","gain.D"]*.25,
                                         rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = -0.005)




# benchmarks NPM
set.seed(2)
bench.npm <- dml_benchmark(dml.401k.npm,
                           benchmark_covariates = c("inc", "pira", "twoearn"))
bench.npm


# contour plots NPM
library(sensemakr)
out <- summary(bench.npm)
sens.401k <- dml.sensemakr::sensemakr(dml.401k.npm, cf.y = NULL)
plot(sens.401k,
     which.bound = "lwr",
     col.contour = "blue",
     label.bump.x = 0.015,
     label.bump.y = 0.003,
     lim.x = .16)
add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.npm,
                                                     cf.y = .04,
                                                     cf.d = .03),
                     point.pch = 25,
                     point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out["pira","gain.Y"], cf.d =out["pira","gain.D"]),
                     label.bump.x = 0.005, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out["twoearn","gain.Y"], cf.d =out["twoearn","gain.D"]),
                     label.bump.x = 0.02, label.bump.y = 0.005)
add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out["inc","gain.Y"], cf.d =out["inc","gain.D"]),
                     label.bump.x = -0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["inc","gain.D"]*(.25),
                     r2yz.dx = out["inc","gain.Y"]*(.25),
                     bound_label = "1/4 x Income",
                     bound_value =
                       confidence_bounds(dml.401k.npm,
                                         cf.y = out["inc","gain.Y"]*.25,
                                         cf.d =out["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = 0.00)

# sensitivity analysis NPM with rho2
out <- summary(bench.npm)
rho2 <- (1/2)^2
sens.401k <- dml.sensemakr::sensemakr(dml.401k.npm, cf.y = NULL, rho2 = rho2)
plot(sens.401k,
     which.bound = "lwr",
     col.contour = "blue",
     label.bump.x = 0.015,
     label.bump.y = 0.003,
     lim.x = .16)
add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.npm,
                                                     cf.y = .04,
                                                     cf.d = .03, rho2= rho2),
                     point.pch = 25,
                     point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.npm,
                                                     cf.y = out["pira","gain.Y"],
                                                     cf.d =out["pira","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.npm,
                                                     cf.y = out["twoearn","gain.Y"],
                                                     cf.d =out["twoearn","gain.D"],
                                                     rho2 = rho2),
                     label.bump.x = 0.02, label.bump.y = 0.005)
add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.npm,
                                                     cf.y = out["inc","gain.Y"],
                                                     cf.d =out["inc","gain.D"],
                                                     rho2= rho2),
                     label.bump.x = -0.01, label.bump.y = 0.009)
add_bound_to_contour(r2dz.x = out["inc","gain.D"]*.25,
                     r2yz.dx = out["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value =
                       confidence_bounds(dml.401k.npm,
                                         cf.y = out["inc","gain.Y"]*(.25),
                                         cf.d =out["inc","gain.D"]*(.25),
                                         rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = 0.00)
