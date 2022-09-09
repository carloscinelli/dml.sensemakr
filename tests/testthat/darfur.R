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

cats <-paste0(ifelse(darfur$herder_dar, "herder", "not_herder"),
              ifelse(darfur$female, "_female", "_male"))
dml.darfur <- dml(y,d,x, model = "npm", cf.folds = 2, cf.reps = 1, groups = cats)
plot(dml.darfur)
plot(dml.darfur,  colors = c("black", "blue", "red"), lwd = c(.8, 1.25), h0.color = "red")
summary(dml.darfur)

robustness_value(dml.darfur)

dreg = list(method    = "ranger",
            trControl = list(method = "none"),
            tuneGrid  = data.frame(mtry = sqrt(ncol(x)),
                                   splitrule = "extratrees", min.node.size = 5))

cats <-paste0(ifelse(darfur$herder_dar, "herder", "not_herder"),
              ifelse(darfur$female, "_female", "_male"))
dml.darfur2 <- dml(y,d,x, model = "npm",
                   dreg = dreg,groups = cats,
                   binary.d = T, cf.folds = 5, cf.reps = 1)
summary(dml.darfur2)
plot(dml.darfur2)


dreg = list(method    = "glm",
            trControl = list(method = "none"),
            # tuneGrid  = data.frame(mtry = sqrt(ncol(x)),
            #                        splitrule = "extratrees", min.node.size = 5)
            tuneLength = 1
)

dreg = list(method    = "ranger",
            trControl = list(method = "none"))

dml.darfur3 <- dml(y,d,x, model = "npm",
                   dreg = dreg,yreg = dreg,
                   groups = cats,
                    cf.folds = 5, cf.reps = 1)
summary(dml.darfur3)



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

summary(dml.darfur <- dml_gate(dml.darfur, groups = darfur$female))
conf.bounds <- confidence_bounds(dml.darfur, r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1, level = 0.95)

coef_plot2(estimate = coef(bounds)["theta.s", ],
           labels =  names(coef(bounds)["theta.s", ]),
           lwr1 = coef(bounds)["theta.m", ],
           upr1 = coef(bounds)["theta.p", ],
           lwr2 = confidence_bounds(bounds)[,"lwr"],
           upr2 = confidence_bounds(bounds)[,"upr"])

coef_plot(estimate = coef(dml.darfur), lwr = conf.bounds[,1], upr = conf.bounds[,2]) +
  geom_errorbar(aes(ymin = conf.bounds[,1]/2, ymax =  conf.bounds[,2], color = "Bounds"))  +
  scale_color_manual(name = "Legend", values = c(colors, "red")) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "blank", "blank"),
                                                   shape = c(NA, NA, 15))))+
  theme_bw()


bounds <- dml_bounds(dml.darfur, r2ya.dx = 0.03, r2.rr = 0.03, rho = 1)
coef(bounds)
plot(bounds)

summary(bounds)
confidence_bounds(bounds)
