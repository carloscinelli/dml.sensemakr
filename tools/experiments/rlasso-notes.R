# rm(list = ls())
# n=100
# p= 100
# beta = 1/(1:p)^2
# gamma =1/(1:p)^2
#
# X=matrix(rnorm(n*p), n, p)
#
#
# D=   X%*%gamma + rnorm(n)/4
#
# Y =  D+ X%*%beta + rnorm(n)
#
# dml.rlasso <- dml(as.vector(Y), as.vector(D), X, yreg = "rlasso",
#                   dreg = "ranger", cf.folds = 5)
# summary(dml.rlasso)
#
#
# resY = rlasso(Y~ X, Post=T)$res
# resD = rlasso(D~ X, Post=T)$res
# lm(resY ~ resD)$coef[2]
#
# # According to the documentation: https://cran.r-project.org/web/packages/hdm/hdm.pdf
# # For rlassoATE, d, as the treatment variable, must be binary
# rlassoATE(x = as.data.frame(X),d = c(rep(1,50), rep(0,50)), y = Y)
# # rlassoATE(x = as.data.frame(X),d = D, y = Y) # This gives error
