rm(list = ls())

# Comparison with DoubleML ------------------------------------------------

library(DoubleML)
library(mlr3)

dt_bonus = fetch_bonus(return_type = "data.table")

obj_dml_data_bonus <- DoubleMLData$new(dt_bonus,
                                       y_col = "inuidur1",
                                       d_cols = "tg",
                                       x_cols = c("female",
                                                  "black",
                                                  "othrace",
                                                  "dep1","dep2","q2", "q3","q4", "q5", "q6",
                                                  "agelt35", "agegt54", "durable","lusd", "husd"))
learner_g <- lrn("regr.ranger")
learner_m <- lrn("regr.ranger")

doubleml_bonus <- DoubleMLPLR$new(obj_dml_data_bonus,
                                  ml_m= learner_m, ml_g=learner_g, score = "partialling out")
doubleml_bonus$fit()
doubleml_bonus$summary()
doubleml_bonus$confint()

learner_g <- lrn("regr.ranger")
learner_m <- lrn("classif.ranger")


doubleml_bonus_2 <- DoubleMLIRM$new(obj_dml_data_bonus,
                                  ml_m= learner_m, ml_g=learner_g, score = "ATE")
doubleml_bonus_2$fit()
doubleml_bonus_2$summary()
doubleml_bonus_2$confint()


doubleml_bonus_2 <- DoubleMLIRM$new(obj_dml_data_bonus,
                                    ml_m= learner_m, ml_g=learner_g, score = "ATTE")
doubleml_bonus_2$fit()
doubleml_bonus_2$summary()
doubleml_bonus_2$confint()

# dml.sensemakr
y <- Penn[, "inuidur1"]
d <- Penn[, "tg"]
x <- model.matrix(~ -1 + female + black + othrace + dep + q2 + q3 + q4 + q5 + q6 + agelt35 +
                    agegt54 + durable + lusd + husd, data = Penn)

dml.ranger <- dml(y, d, x, model = "plm")
dml.ranger <- dml(y, d, x, model = "npm")
summary(dml.ranger)
confint(dml.ranger)



