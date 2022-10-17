
# bounds functions --------------------------------------------------------
bias.factor <- function(r2ya.dx = 0.03, r2.rr = 0.04, rho2 = 1){
  sqrt(rho2*r2ya.dx*(r2.rr/(1 - r2.rr)))
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
  psi.S2         <- (sigma2.s*(psi.nu2.s) + nu2.s*psi.sigma2.s)
  psi.bias.bound <- (bf/2) * (1/S) * psi.S2
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

  out$dml.fit <- dml.fit

  main <- dml.fit$results$main
  bounds.results   <- lapply(main, bounds, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  out$results$main <- bounds.results

  main.coefs <- extract_coefs(bounds.results)
  out$coefs$main <- main.coefs

  groups <- dml.fit$results$groups
  if (!is.null(groups)) {
    groups.bounds <- lapply(groups,
                            function(x) lapply(x,
                                               bounds, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2))
    out$results$groups <- groups.bounds
    out$coefs$groups <- lapply(groups.bounds, extract_coefs)
    }

  class(out) <- "dml.bounds"
  return(out)
}


#' Compute confidence bounds
#' @description
#'
#' @export
confidence_bounds <- function(model, ...){
  UseMethod("confidence_bounds")
}



#' @export
#' @rdname confidence_bounds
confidence_bounds.numeric <- function(theta.s, S2,
                                      se.theta.s, se.S2,
                                      cov.theta.S2,
                                      r2ya.dx, r2.rr,
                                      rho2 = 1,
                                      combine.method = "median",
                                      level = 0.95){
  k = bias.factor(r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  se.m <- sqrt((se.theta.s)^2 + (k^2/(4*S2))*se.S2^2 - (k/sqrt(S2))*cov.theta.S2)
  se.p <- sqrt((se.theta.s)^2 + (k^2/(4*S2))*se.S2^2 + (k/sqrt(S2))*cov.theta.S2)
  theta.m <- theta.s - k*sqrt(S2)
  theta.p <- theta.s + k*sqrt(S2)
  level[level < 0.5] <- 0.5
  t_crit <- qnorm(level)
  lwr <- combine.median(theta.m, se.m)
  upr <- combine.median(theta.p, se.p)
  lwr <- unname(lwr["estimate"] - t_crit*lwr["se"])
  upr <- unname(upr["estimate"] + t_crit*upr["se"])
  c(lwr = lwr, upr = upr)
}

#' @export
#' @rdname confidence_bounds
confidence_bounds.dml <- function(model,
                                  r2ya.dx,
                                  r2.rr,
                                  rho2 = 1,
                                  level = 0.95,
                                  combine.method = "median", ...){
  object <- dml_bounds(model, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
  confidence_bounds(object, level = level,combine.method = combine.method, ...)
}



#' @export
#' @rdname confidence_bounds
confidence_bounds.dml.bounds <- function(model,
                                         r2ya.dx = NULL,
                                         r2.rr = NULL,
                                         rho2 = NULL,
                                         level = 0.95,
                                         combine.method = "median",
                                         return = c("lwr", "upr"),
                                         ...){

  if (!is.null(r2ya.dx) | !is.null(r2.rr) | !is.null(rho2)) {

    if (is.null(r2ya.dx)) {
      r2ya.dx <- model$info$r2ya.dx
    }

    if (is.null(r2.rr)) {
      r2.rr <- model$info$r2.rr
    }

    if (is.null(rho2)) {
      rho2 <- model$info$rho2
    }

    new_bounds <- dml_bounds(model$dml.fit, r2ya.dx = r2ya.dx, r2.rr = r2.rr, rho2 = rho2)
    return(confidence_bounds(new_bounds, combine.method = combine.method, return = return))

  }

  level2 = max(0, 1 - (1 - level)*2)

  confs <- confint(model, level = level2, combine.method = combine.method)

  if (is.list(confs)) {
    out <- t(sapply(confs, function(x) c(lwr = x["theta.m",1], upr = x["theta.p",2])))
  } else {
    out <- rbind(ate = c(lwr = confs["theta.m",1], upr = confs["theta.p",2]))
  }
  out <- out[, return, drop = F]
  attr(out, "conf.levels") <- c(point = level, region = level2)
  attr(out, "sens.param")  <- model$info
  class(out) <- c("confidence.bounds", "matrix")
  out
}


rv_fun <- function(dml.fit, rv, par, side = "lwr", theta = 0, alpha = 0.05){
  (confidence_bounds(dml.fit,  r2ya.dx = rv,r2.rr = rv, level = 1 - alpha)[par,side] - theta)^2
}


##' Computes Robustness Values for Debiased Machine Learning
##'
##' @export
robustness_value <- sensemakr::robustness_value

##' @rdname robustness_value
##'@exportS3Method sensemakr::robustness_value dml
##'@exportS3Method dml.sensemakr::robustness_value dml
robustness_value.dml <- function(model, theta = 0, alpha = 0.05, ...){
  conf <- confint(model, level = 1 - alpha,...)
  out <- setNames(rep(NA,nrow(conf)), rownames(conf))
  for (i in 1:nrow(conf)) {
    if (conf[i,1] <= theta & theta <= conf[i,2]) {
      out[i] <- 0
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    fn <- function(rv) rv_fun(rv, dml.fit = model, par = names(out)[i],
                              side = side, theta = theta, alpha = alpha)
    out[i] <- optim(par = c(0.01), fn, lower = 0, upper = 1, method = "Brent")$par
  }
  return(out)
  # grid <- seq(0, 0.99,by = 0.001)
  # values <- mapply(function(x,y) confidence_bounds(dml.fit, r2ya.dx= x, r2.rr = y), x = grid, y = grid)
  # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
  # grid[rv.idx]
}

##' @rdname robustness_value
##'@exportS3Method sensemakr::robustness_value dml.bounds
##'@exportS3Method dml.sensemakr::robustness_value dml.bounds
robustness_value.dml.bounds <- function(model, theta = 0, alpha = 0.05, ...){
  model <- model$dml.fit
  conf <- confint(model, level = 1 - alpha,...)
  out <- setNames(rep(NA,length(conf)), names(conf))
  for (i in 1:nrow(conf)) {
    if (conf[i,1] <= theta & theta <= conf[i,2]) {
      out[i] <- 0
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    fn <- function(rv) rv_fun(rv, dml.fit = model, par = names(out)[i],
                              side = side, theta = theta, alpha = alpha)
    out[i] <- optim(par = c(0.01), fn, lower = 0, upper = 1, method = "Brent")$par
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
