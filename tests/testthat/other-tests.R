## loads data
rm(list = ls())
library(dml.sensemakr)
data("pension")
y <- pension$net_tfa # net total financial assets
d <- pension$e401 # 401K eligibility
x <- model.matrix(~ -1 + age + inc + educ+ fsize + marr + twoearn + pira + hown, data = pension)

nnet <- list(method = "nnet", tuneGrid = expand.grid(size = 8, decay = 0.01))
dml.401k.plm <- dml(y, d, x, d.class =T,
                    dirty.tuning = F, model = "npm", yreg = nnet, cf.folds = 5, cf.reps = 1)
summary(dml.401k.plm)

dml.401k.plm <- dml(y, d, x, d.class = T, model = "plm", cf.folds = 5, cf.reps = 1)
summary(dml.401k.plm)


xl  <- model.matrix(~ -1 +
                      (poly(age, 6, raw=TRUE) +
                         poly(inc, 8, raw=TRUE) +
                         poly(educ, 4, raw=TRUE) +
                         poly(fsize, 2, raw=TRUE) +
                         marr + twoearn + pira + hown)^2,
                    data = pension)
dml.401k.plm <- dml(y, d, xl,
                    model = "npm",
                    yreg = list(method = "glmnet",
                                tuneGrid = data.frame(alpha = 1,
                                                      lambda = seq(0.001, .005, length=5)),
                                trControl= list(method = "cv", number = 5)), dirty.tuning = T,
                    cf.folds = 5, cf.reps = 1)
summary(dml.401k.plm)

# dml.401k.plm <- dml(y, d, xl,
#                     model = "npm",
#                     yreg = list(method = "rlasso",
#                                 penalty = list(homoscedastic = FALSE,
#                                                X.dependent.lambda = FALSE, lambda.start = NULL, c = 1.1, gamma = 0.1/log(nrow(pension)))),
#                     cf.folds = 5, cf.reps = 1)
# summary(dml.401k.plm)

benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))

model <- dml.401k.plm
resY <- sapply(model$fits, function(x) x$preds$yhat)
cbind(resY, model$data$y)
R2s <- apply(resY, 2, function(x)max(1-var(x)/var(model$data$y),0))
<- combine.mean(R2s, R2s)["estimate"]
length(resY)
length(y)
str(model$fits)



model <- dml.401k.plm
model$fits
model$fits[[1]]
?extract_coefs(model)
model$results$main[[1]]$estimates$nu2.s
debug(bench_plm)
bench_plm(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))

bench_npm(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
