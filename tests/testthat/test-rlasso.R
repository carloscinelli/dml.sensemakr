rm(list = ls())
n=100
p= 100
beta = 1/(1:p)^2
gamma =1/(1:p)^2

X=matrix(rnorm(n*p), n, p)


D=   X%*%gamma + rnorm(n)/4

Y =  D+ X%*%beta + rnorm(n)

dml.rlasso <- dml(y, d, x, reg = "rlasso", cf.folds = 5)
summary(dml.rlasso)

resY = rlasso(Y~ X, Post=T)$res
resD = rlasso(D~ X, Post=T)$res
lm(resY ~ resD)$coef[2]
rlassoATE(X,D, Y)
