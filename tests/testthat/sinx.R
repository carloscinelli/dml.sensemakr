rm(list = ls())

n <- 1e3
x <- rnorm(n)
d <- rbinom(n, 1, prob = plogis(sin(x)))
e <- rnorm(n)
y0 <- sin(x) + e
y1 <- 1 + 5*sin(x) + e
y <- y1*d+(1-d)*y0
ate <- mean(y1-y0)
ate
att <- mean((y1-y0)[d==1])
att


atu <- mean((y1-y0)[d==0])
atu

# # library(SuperLearner)
# # library(tmle)
# # tmle.fit <- tmle(y,d, cbind(1,x))
# # tmle.fit
#
# m <- lm(y ~ d*sin(x))
# m
# mean(predict(m, data.frame(d=1, x))-predict(m, data.frame(d=0, x)))
#
# m <- lm(y ~ d*poly(x, 2, raw = T))
# mean(predict(m, data.frame(d=1, x))-predict(m, data.frame(d=0, x)))

library(dml.sensemakr)
glm.args <- list(method = "glm", family = "binomial")
x2 <- model.matrix(~poly(x, degree = 7, raw = T)-1)
dml.ranger <- dml(y, d, x2, model = "npm", reg = "lm", dreg = glm.args, cf.reps = 2)
summary(dml.ranger)

dml.ranger <- dml(y, d, x, model = "npm", reg = "lm", dreg = "gam", cf.reps = 2)
summary(dml.ranger)

dml.ranger <- dml(y, d, x,  model = "npm", reg = "ranger", cf.reps = 2)
summary(dml.ranger)


dml.ranger <- dml(y, d, x, target = "atu", model = "npm", reg = "lm", dreg = "gam", cf.reps = 2)
summary(dml.ranger)


ranger.args <- list(method = "ranger",
                    trControl = list(method = "cv", number = 5),
                    tuneGrid = expand.grid(mtry = 1, splitrule = "variance",
                                           min.node.size=seq(1, n/2, length=20)))
dml.ranger <- dml(y, d, x, model = "npm", reg=  ranger.args, cf.folds = 5)
summary(dml.ranger)

library(ranger)
out <- ranger(y=(d), x= cbind(x), num.trees = 500, splitrule = "beta")
out
preds <- predict(out, data = data.frame(x = x), type = "response")
preds$predictions

dml.ranger <- dml(y, d, x2, model = "npm", reg = "lm")
summary(dml.ranger)

confint(dml.ranger)

library(DoubleML)
library(mlr3)
dt <- data.frame(y, d, x)
obj_dml_data_bonus <- DoubleMLData$new(dt,
                                       y_col = "y",
                                       d_cols = "d",
                                       x_cols = "x")
learner_g <- lrn("regr.ranger")
learner_m <- lrn("classif.ranger")

doubleml <- DoubleMLIRM$new(obj_dml_data_bonus,
                                ml_m= learner_m, ml_g=learner_g, score = "ATE")
doubleml$fit()
doubleml$summary()
options(scipen=99)

library(grf)
a <- causal_forest(cbind(x),cbind(y),cbind(d))

summary(a)
average_treatment_effect(a, target.sample = "all")
average_treatment_effect(a, target.sample = "treated")

