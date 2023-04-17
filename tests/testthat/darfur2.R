rm(list = ls())
set.seed(1)
n = 100 #sample size
p = 100 # number of variables
s = 3 # nubmer of variables with non-zero coefficients
X = Xnames = matrix(rnorm(n*p), ncol=p)
colnames(Xnames) <- paste("V", 1:p, sep="")
beta = c(rep(5,s), rep(0,p-s))
Y = X%*%beta + rnorm(n)

d <- X[,1]
X <- X[,-1]
d
X
Y
debug(dml)
dml.hdm <- dml(c(Y),d,X)
summary(dml.hdm)

library(sensemakr)
data("darfur")
x <- model.matrix( ~  age + farmer_dar + herder_dar +
  pastvoted + hhsize_darfur + female + village, data = darfur)
y <- darfur$peacefactor
y2 <- darfur$peace_formerenemies
d <- darfur$directlyharmed
library(dml.sensemakr)
dml.darfur <- dml(y,d,x, groups = darfur$female,d.class = T, model = "npm", yreg = "ranger", dreg = "ranger")
summary(dml.darfur)
with(darfur, mean(d[darfur$female==0]))
with(darfur, mean(d[darfur$female==1]))

dml.darfur <- dml(y,d,x, groups = darfur$female,model = "npm", reg="rlasso")
summary(dml.darfur)


dml.darfur2 <- dml(y2,d,x, groups = darfur$female,y.class = T, d.class = T, model = "npm")
summary(dml.darfur2)



x2 <- model.matrix( ~  age + farmer_dar + herder_dar +
                     pastvoted + hhsize_darfur  + village, data = darfur)
coef(summary(m1<-lm(y ~d*g +x)))[c("d","g", "d:g"),]
b <- coef(summary(m1<-lm(y ~d*g +x)))[c("d","g", "d:g"),"Estimate"]
b
# using partialling out versus just regression
# partialling out and groups recovers the effect heterogeneity? No.
y.res <- resid(lm(y ~ x))
d.res <- resid(lm(d ~ x))
g <- as.factor(darfur$female)
m<- lm(y.res ~  d.res:g)
summary(m)
predict(m, newdata = data.frame(d.res=1,g=1))-predict(m, newdata = data.frame(d.res=0,g=1))
predict(m, newdata = data.frame(d.res=1,g=0))-predict(m, newdata = data.frame(d.res=0,g=0))
mean(predict(m, newdata = data.frame(d.res=1,g=g))-predict(m, newdata = data.frame(d.res=0, g=g)))

