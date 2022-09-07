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


dml.darfur <- dml(y,d,x, model = "npm", cf.folds = 5, cf.reps = 5)
summary(dml.darfur)

cats <-paste0(ifelse(darfur$herder_dar, "herder", "not_herder"),
       ifelse(darfur$female, "_female", "_male"))
summary(dml.darf2 <- dml_gate(dml.darfur, groups = darfur$female))
summary(dml.darf2 <- dml_gate(dml.darfur, groups = darfur$herder_dar))
summary(dml.darf2 <- dml_gate(dml.darfur, groups = cats))
confint(dml.darf2)
diff1 <- coef(dml.darf2)["gate.herder_male"]-coef(dml.darf2)["gate.not_herder_male"]
se1 <- sqrt(se(dml.darf2)["gate.herder_male"]^2+se(dml.darf2)["gate.not_herder_male"]^2)
diff1/se1
plot(dml.darf2)

diff1 <- coef(dml.darf2)["gate.herder_male"]-coef(dml.darf2)["gate.not_herder_male"]
se1 <- sqrt(se(dml.darf2)["gate.herder_male"]^2+se(dml.darf2)["gate.not_herder_male"]^2)
diff1/se1

robustness_value(dml.darfur, alpha = 0.5)
robustness_value(dml.darf2, alpha = 0.05)

confidence_bounds(dml.darfur, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1, level = 0.95)

bounds

bounds <- dml_bounds(dml.darfur,yreg= list(method = "rlasso"),
                     r2ya.dx = 0.03, r2.rr = 0.03, rho = 1)
summary(bounds)
confidence_bounds(bounds)
