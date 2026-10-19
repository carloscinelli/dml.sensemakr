rm(list = ls())
n <- 1e4
x <- rnorm(n)
a <- rnorm(n)
pxa.f <- function(x, a) plogis(sin(x)+ sin(a))
px.f <- function(x) integrate(f = function(a) pxa.f(x, a)*dnorm(a),
                              lower = -3, upper = 3, rel.tol = 1e-6)$value
p.f <- integrate(f = function(x)px.f(x)*dnorm(x), -3, 3, rel.tol = 1e-10)$value
oddsxa.f <- function(x,a) {
  p <- pxa.f(x, a)
  p/(1-p)
}
oddsx.f <- function(x){
  p <- px.f(x)
  p/(1-p)
}

Eoxa <- integrate(function(y) {
  sapply(y, function(y) {
    integrate(function(x) oddsxa.f(x,y)*dnorm(x), -5, 5, rel.tol = 1e-10)$value*dnorm(y)
  })
}, -5, 5, rel.tol = 1e-10)$value
Eoxa
mean(oddsxa.f(x,a))
Eox <- integrate(function(x) oddsx.f(x)*dnorm(x),
                 lower=-4, upper =4, rel.tol = 1e-12)$value
Eox
mean(sapply(x, oddsx.f))

Ba2 <- (1/p.f^2)*(Eoxa-Eox)

alphas.f <- function(d,x) {
  p <- sapply(x, px.f)
  (d/p - (1-d)/(1-p))*(p/p.f)
    }
alpha.f  <- function(d,x,a) {
  (d/pxa.f(x,a) - (1-d)/(1-pxa.f(x,a)))*(pxa.f(x,a)/p.f)
}

gs.f <- function(d,x) d + d*sin(x)
Bg2 <- 2
g.f <- function(d, x, a) gs.f(d,x) + sqrt(Bg2/Ba2)*(alpha.f(d,x,a)-alphas.f(d,x))

d <- rbinom(n, 1, pxa.f(x, a))
g <- g.f(d,x,a)
y <- g + rnorm(n)
gs <- gs.f(d,x)

alpha <- alpha.f(d, x,a)
alphas <- alphas.f(d, x)
var(sqrt(Bg2/Ba2)*(alpha-alphas))
var(g-gs)
var(g-gs);Bg2
var(alpha-alphas);Ba2


cor(g-gs, alpha-alphas)

(mean(g*alpha) - mean(gs*alphas))^2



(var(g-gs)/var(y-gs))*(Eoxa/Eox - 1)*var(y-gs)*var(alphas)
Ba2*Bg2

Eoxa/p.f^2
var(alpha)
Eox/p.f^2
var(alphas)
var(alpha)- var(alphas)
(var(alpha-alphas)/Ba2)*Bg2

Eoxa/p.f^2 - Eox/p.f^2

# alpha0 <-
