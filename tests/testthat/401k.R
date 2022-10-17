# loads package
rm(list = ls())
library(dml.sensemakr)

## loads data
data("pension")
y <- pension$net_tfa
d <- pension$e401
x <- model.matrix(~ -1 + age + inc + educ+ fsize + marr + twoearn + pira + hown, data = pension)

## computes income quartiles
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)


# Partially Linear Model --------------------------------------------------

set.seed(10)
dml.401k.plm <- dml(y, d, x,
                    y.reg = "nnet",
                    y.trControl = trainControl(method = "cv", number = 5),
                    y.tuneLength = 10,
                    dirty.tuning = T,
                    model = "npm",
                    groups = g1,
                    cf.folds = 2)

## Results under unconfoundedness
summary(dml.401k.plm)
coef(dml.401k.plm)
se(dml.401k.plm)
confint(dml.401k.plm)
plot(dml.401k.plm, text = F)


## senstivity results

### RV
robustness_value(dml.401k.plm)

### confidence bounds
confidence_bounds(dml.401k.plm, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1)

### contours with bound
ovb_contour_plot(dml.401k.plm, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1,
                 bound.label = "Max Match (3x years)",
                 col.contour = "blue")

### compute estimate, SEs for all bounds, bias bound etc
bounds.plm <- dml_bounds(dml.401k.plm,  r2ya.dx = 0.03, r2.rr = 0.04, rho2 =1)
summary(bounds.plm)
plot(bounds.plm, text = F)

# Nonparametric Model -----------------------------------------------------

dml.401k.npm <- dml(y, d, x, groups = g1, model = "npm")

## Results under unconfoundedness
summary(dml.401k.npm)
coef(dml.401k.npm)
se(dml.401k.npm)
confint(dml.401k.npm)
plot(dml.401k.npm, text = F)


## senstivity results

### RVs
robustness_value(dml.401k.npm)

### confidence bounds
confidence_bounds(dml.401k.npm, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1)

### contours of lower limit with bound
ovb_contour_plot(dml.401k.npm, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1,
                 which.bound = "lwr",
                 bound.label = "Max Match (3x years)",
                 col.contour = "blue")

### you can make contours for all groups. Here's contours for group 4
ovb_contour_plot(dml.401k.npm, group = T, group.number = 4,
                 r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1,
                 which.bound = "lwr",
                 bound.label = "Max Match (3x years)",
                 col.contour = "blue")

### compute estimate, SEs for all bounds, bias bound etc
bounds.npm <- dml_bounds(dml.401k.npm,  r2ya.dx = 0.03, r2.rr = 0.04, rho2 =1)
summary(bounds.npm)
plot(bounds.npm, text = F)
