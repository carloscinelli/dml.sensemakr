rm(list =ls())
library(dml.sensemakr)
library(sensemakr)

# loads dataset
data("darfur")

# runs regression model
model <- lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)

sensemakr(model, treatment = "directlyharmed")
robustness_value(model)

y <- as.numeric(darfur$peacefactor)
d <- darfur$directlyharmed
x <- model.matrix(~ age + farmer_dar + herder_dar +
                    pastvoted + hhsize_darfur + female + village, data = darfur)


dml.darfur <- dml(y,d,x, model = "npm",
                  yreg= list(method = "glmnet",
                             trControl = list(method = "cv", number = 5),
                             tuneLength = 10),
                  dreg= list(method = "glmnet",
                             trControl = list(method = "cv", number = 5),
                             tuneLength = 10))
summary(dml.darfur)

summary(dml.darf2 <- dml_gate(dml.darfur, groups = darfur$female))
robustness_value(dml.darfur, alpha = 0.5)
robustness_value(dml.darf2, alpha = 0.05)

confidence_bounds(dml.darfur, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1, level = 0.95)

bounds

bounds <- dml_bounds(dml.darfur,yreg= list(method = "rlasso"),
                     r2ya.dx = 0.03, r2.rr = 0.03, rho = 1)
summary(bounds)
confidence_bounds(bounds)
