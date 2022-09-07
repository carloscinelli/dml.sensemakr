
# bounds functions --------------------------------------------------------
bias.factor <- function(r2ya.dx = 0.04, r2.rr = 0.03, rho2 = 1){
  sqrt(rho2*r2ya.dx*(r2.rr/(1-r2.rr)))
}


bounds <- function(short.results, r2ya.dx = 0.04, r2.rr = 0.03, rho2 = 1){

  # short estimates
  theta.s  <- short.results$estimates$theta.s
  sigma2.s <- short.results$estimates$sigma2.s
  nu2.s    <- short.results$estimates$nu2.s

  # bias bound
  bf <- bias.factor(r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  S  <- sqrt(sigma2.s*nu2.s)
  bias.bound <- S*bf

  # plug-in bounds
  theta.m <- theta.s - bias.bound
  theta.p <- theta.s + bias.bound

  # short IFs
  psi.theta.s  <- short.results$psis$psi.theta.s
  psi.sigma2.s <- short.results$psis$psi.sigma2.s
  psi.nu2.s    <- short.results$psis$psi.nu2.s

  # bounds IFs
  psi.bias.bound <- (bf/2) * (1/S)*(sigma2.s*(psi.nu2.s) + nu2.s*psi.sigma2.s)
  psi.theta.m    <- psi.theta.s - psi.bias.bound
  psi.theta.p    <- psi.theta.s + psi.bias.bound

  out <- list(
    psis      = list(psi.theta.s    = psi.theta.s,
                     psi.sigma2.s   = psi.sigma2.s,
                     psi.nu2.s      = psi.nu2.s,
                     psi.bias.bound = psi.bias.bound,
                     psi.theta.m    = psi.theta.m,
                     psi.theta.p    = psi.theta.p),

    estimates = list(theta.s = theta.s,
                     se.theta.s = psi.sd(psi.theta.s),
                     bias.bound = bias.bound,
                     se.bias.bound = psi.sd(psi.bias.bound),
                     theta.m = theta.m,
                     se.theta.m = psi.sd(psi.theta.m),
                     theta.p = theta.p,
                     se.theta.p = psi.sd(psi.theta.p))
  )
  return(out)
}





extract_coefs <- function(bounds.results){
  coefs.names <- c("theta.s", "bias.bound", "theta.m", "theta.p")
  names(coefs.names) <- coefs.names
  lapply(coefs.names,
         function(param) combine.cross.fits(bounds.results, param = param))
}

get_bounds <- function(bounds, combine.method = "mean"){
  f <- function(coefs) t(sapply(coefs, function(x) x[combine.method, ]))
  lapply(bounds$coefs, f)
}

##'@export
dml_bounds <- function(dml.fit, r2ya.dx, r2.rr, rho2 = 1){

  # bounds for
  out <- list()
  out$info <- list(r2ya.dx = r2ya.dx,
                     r2.rr = r2.rr,
                     rho2 = rho2)

  main <- dml.fit$results$main
  bounds.results   <- lapply(main, bounds, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  out$results$main <- bounds.results

  main.coefs <- extract_coefs(bounds.results)
  out$coefs$main <- main.coefs

  groups <- dml.fit$results$groups
  if(!is.null(groups)){
    groups.bounds <- lapply(groups,
                            function(x) lapply(x,
                                               bounds, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2))
    out$results$groups <- groups.bounds
    out$coefs$groups <- lapply(groups.bounds, extract_coefs)
    }

  class(out) <- "dml.bounds"
  return(out)
}

##'@export
confidence_bounds.dml <- function(object,
                                  r2ya.dx,
                                  r2.rr,
                                  rho2 = 1,
                                  level = 0.95,
                                  combine.method = "median", ...){
  object <- dml_bounds(object, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  confidence_bounds(object, level = level,combine.method = combine.method, ...)
}


rv_fun <- function(rv,dml.fit, par, side = "lwr",theta = 0){
  (confidence_bounds(dml.fit,  r2ya.dx = rv,r2.rr = rv)[par,side] - theta)^2
}



##' @export
robustness_value <- function(object, ...){
  UseMethod("robustness_value")
}

##' Robustness Value DML
##'
##'@exportS3Method sensemakr::robustness_value dml
##'@exportS3Method dml.sensemakr::robustness_value dml
robustness_value.dml <- function(object, theta = 0, alpha = 0.05, ...){
  conf <- confint(object, level = 1-alpha)
  out <- setNames(rep(NA,nrow(conf)), rownames(conf))
  for(i in 1:nrow(conf)){
    if(conf[i,1] <= theta & theta <= conf[i,2]){
      out[i] <- 0
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    fn <- function(rv) rv_fun(rv, dml.fit = object,par=names(out)[i], side = side, theta = theta)
    out[i] <- optim(par = c(0.01), fn, lower=0, upper = 1, method = "Brent")$par
  }
  return(out)
  # grid <- seq(0, 0.99,by = 0.001)
  # values <- mapply(function(x,y) confidence_bounds(dml.fit, r2ya.dx= x, r2.rr = y), x = grid, y = grid)
  # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
  # grid[rv.idx]
}


# ##' Robustness Value DML
# ##'
# ##'@exportS3Method sensemakr::robustness_value dml.bounds
# ##'@exportS3Method dml.sensemakr::robustness_value dml.bounds
# robustness_value.dml.bounds <- function(object, theta = 0, alpha = 0.05, ...){
#   conf <- confint(object, parm = "theta.s", level = 1-alpha)
#   if(conf[,1] <= theta & theta <= conf[,2]){
#     return(0)
#   }
#   side <- ifelse(theta < conf[,1], "lwr", "upr")
#   fn <- function(rv) rv_fun(rv, dml.fit = object, side = side, theta = theta)
#   out <- optim(par = c(0.01), fn, lower=0, upper = 1, method = "Brent")$par
#   return(out)
#   # grid <- seq(0, 0.99,by = 0.001)
#   # values <- mapply(function(x,y) confidence_bounds(dml.fit, r2ya.dx= x, r2.rr = y), x = grid, y = grid)
#   # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
#   # grid[rv.idx]
# }
