
# bounds functions --------------------------------------------------------
bias.factor <- function(cf.y = 0.03, cf.d = 0.04, rho2 = 1){
  sqrt(rho2*cf.y*(cf.d/(1 - cf.d)))
}


bounds <- function(short.results, cf.y = 0.04, cf.d = 0.03, rho2 = 1){

  # short estimates
  theta.s  <- short.results$estimates$theta.s
  sigma2.s <- short.results$estimates$sigma2.s
  nu2.s    <- short.results$estimates$nu2.s

  if (nu2.s < 0) {
    warning("Unable to compute bounds because nu^2 is negative,
            this is an indication of a very poor fitting of the treatment model.
            Consider choosing a different learner for the treatment.")
  }

  # bias bound
  bf <- bias.factor(cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
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

##' Bounds on the omitted variable bias for causal machine learning
##'
##' @param model an object of class \code{\link{dml}} or \code{\link{dml_bounds}}.
##' @param cf.y (nonparametric) partial R2 of the omitted variables with the outcome. Must be a number between (0, 1).
##' @param cf.d how much variation latent variables create in the Riesz Representer of the target parameters. Must be a number between (0, 1). When the target of interest is the ATE in a partially linear model, this corresponds to the partial R2 of omitted variables with the treatment. When the target of interest is the ATE in a non-parametric model with a binary treatment, this corresponds to the gains in precision (i.e, 1/variance) when predicting who is assigned to treatment.
##' @param rho2 degree of adversity. Default is \code{rho=1}, which assumes the maximum degree of adversity of confounding.
##' @param ... arguments passed to other methods.
##' @param combine.method method to combine the results of each repetition. Options are \code{mean} and \code{median}. Default is \code{median}.
##' @param level confidence level. Default is \code{0.95}.
##' @returns For \code{dml_bounds}: an object of class \code{dml.bounds}. For \code{confidence_bounds}: a matrix or numeric vector of confidence bounds.
##' @export
dml_bounds <- function(model, cf.y, cf.d, rho2 = 1){

  # bounds for
  out <- list()
  out$info <- list(cf.y = cf.y,
                     cf.d = cf.d,
                     rho2 = rho2)

  out$dml.fit <- model

  # main <- model$results$main
  # bounds.results   <- lapply(main, bounds, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
  # out$results$main <- bounds.results
  #
  # main.coefs <- extract_coefs(bounds.results)
  # out$coefs$main <- main.coefs
  main <- model$results$main
  if (!is.null(main)) {
    main.bounds <- lapply(main,
                            function(x) lapply(x,
                                               bounds, cf.y = cf.y, cf.d = cf.d, rho2 = rho2))
    out$results$main <- main.bounds
    out$coefs$main <- lapply(main.bounds, extract_coefs)
  }

  groups <- model$results$groups
  if (!is.null(groups)) {
    groups.bounds <- lapply(groups,
                            function(x) lapply(x,
                                               bounds, cf.y = cf.y, cf.d = cf.d, rho2 = rho2))
    out$results$groups <- groups.bounds
    out$coefs$groups <- lapply(groups.bounds, extract_coefs)
    }

  class(out) <- "dml.bounds"
  return(out)
}


#' Compute confidence bounds
#' @description Computes confidence bounds on the target parameter of interest accounting for omitted variable biases.
#'
#' @rdname dml_bounds
#' @export
confidence_bounds <- function(...){
  UseMethod("confidence_bounds")
}



##' @param theta.s short estimate of the target parameter (used as the first argument for the numeric method).
##' @param S2 estimated variance product (sigma2 * nu2).
##' @param se.theta.s standard error of the short estimate.
##' @param se.S2 standard error of S2.
##' @param cov.theta.S2 covariance of theta.s and S2.
##' @param return character vector specifying which bounds to return. Options are \code{"lwr"} and \code{"upr"}.
#' @export
#' @rdname dml_bounds
confidence_bounds.numeric <- function(theta.s, S2,
                                      se.theta.s, se.S2,
                                      cov.theta.S2,
                                      cf.y, cf.d,
                                      rho2 = 1,
                                      combine.method = "median",
                                      level = 0.95, ...){
  k = bias.factor(cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
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
#' @rdname dml_bounds
confidence_bounds.dml <- function(model,
                                  cf.y,
                                  cf.d,
                                  rho2 = 1,
                                  level = 0.95,
                                  combine.method = "median", ...){
  object <- dml_bounds(model, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
  confidence_bounds(object, level = level,combine.method = combine.method, ...)
}



#' @export
#' @rdname dml_bounds
confidence_bounds.dml.bounds <- function(model,
                                         cf.y = NULL,
                                         cf.d = NULL,
                                         rho2 = NULL,
                                         level = 0.95,
                                         combine.method = "median",
                                         return = c("lwr", "upr"),
                                         ...){

  if (!is.null(cf.y) | !is.null(cf.d) | !is.null(rho2)) {

    if (is.null(cf.y)) {
      cf.y <- model$info$cf.y
    }

    if (is.null(cf.d)) {
      cf.d <- model$info$cf.d
    }

    if (is.null(rho2)) {
      rho2 <- model$info$rho2
    }

    new_bounds <- dml_bounds(model$dml.fit, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
    return(confidence_bounds(new_bounds, combine.method = combine.method, return = return))

  }

  level2 = max(0, 1 - (1 - level)*2)

  confs <- confint(model, level = level2, combine.method = combine.method)

  if (is.list(confs)) {
    out <- t(sapply(confs, function(x) c(lwr = x["theta.m",1], upr = x["theta.p",2])))
  } else {
    out <- rbind(ate = c(lwr = confs["theta.m",1], upr = confs["theta.p",2]))
  }
  out <- out[, return, drop = FALSE]
  attr(out, "conf.levels") <- c(point = level, region = level2)
  attr(out, "sens.param")  <- model$info
  class(out) <- c("confidence.bounds", "matrix")
  out
}


rv_fun <- function(dml.fit, rv, par, side = "lwr", theta = 0, alpha = 0.05){
  (confidence_bounds(dml.fit,  cf.y = rv,cf.d = rv, level = 1 - alpha)[par,side] - theta)^2
}

xrv_fun <- function(dml.fit, xrv, par, side = "lwr", theta = 0, alpha = 0.05, rho2 = 1){
  (confidence_bounds(dml.fit, rho2 = rho2, cf.y = 1,cf.d = xrv, level = 1 - alpha)[par,side] - theta)^2
}

xrv_fun_2D <- function(dml.fit, yrv, xrv, par, side = "lwr", theta = 0, alpha = 1){
  (confidence_bounds(dml.fit,  cf.y = yrv,cf.d = xrv, level = 1 - alpha)[par,side] - theta)^2
}


##' Computes Robustness Values for Debiased Machine Learning
##'
##' @description
##' This function computes the robustness value of a target parameter estimated via debiased machine learning.
##'
##' The robustness value describes the minimum strength of association (parameterized in terms of  R2) that omitted variables would need to have both with the outcome and with the Riesz Representer so that the confidence bounds for the target parameter includes zero (or another threshold of interest).
##'
##'
##' @returns A named numeric vector of robustness values.
##' @export
robustness_value <- sensemakr::robustness_value

##' @rdname robustness_value
##' @param model an object of class \code{\link{dml}} or \code{\link{dml_bounds}}.
##' @param theta the null hypothesis of interest for the target parameter theta. Default is \code{theta =0} (zero null hypothesis).
##' @param alpha significance level. Default is \code{alpha = 0.05}.
##' @inheritParams summary.dml
##' @exportS3Method sensemakr::robustness_value dml
robustness_value.dml <- function(model, theta = 0, alpha = 0.05, ...){
  conf <- confint(model, level = 1 - alpha,...)
  out <- setNames(rep(NA,nrow(conf)), rownames(conf))
  for (i in 1:nrow(conf)) {
    if (conf[i,1] <= theta & theta <= conf[i,2]) {
      out[i] <- 0
      next
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    fn <- function(rv) rv_fun(rv, dml.fit = model, par = names(out)[i],
                              side = side, theta = theta, alpha = alpha)
    out[i] <- optim(par = c(0.01), fn, lower = 0, upper = 1, method = "Brent")$par
  }
  return(out)
  # grid <- seq(0, 0.99,by = 0.001)
  # values <- mapply(function(x,y) confidence_bounds(dml.fit, cf.y= x, cf.d = y), x = grid, y = grid)
  # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
  # grid[rv.idx]
}

###################################################################################################################
##' Computes Extreme Robustness Values for Debiased Machine Learning
##'
##' @description
##' This function computes the extreme robustness value of a target parameter estimated via debiased machine learning.
##'
##' The extreme robustness value describes the minimum strength of association (parameterized in terms of partial R2) that omitted variables would need to have with the Riesz Representer alone so that the confidence bounds for the target parameter includes zero (or another threshold of interest).
##'
##'
##' @export
extreme_robustness_value <- function(model, ...) {
  UseMethod("extreme_robustness_value")
}

##' @rdname extreme_robustness_value
##' @param model an object of class \code{\link{dml}} or \code{\link{dml.bounds}}.
##' @param theta the null hypothesis of interest for the target parameter theta. Default is \code{theta =0} (zero null hypothesis).
##' @param alpha significance level. Default is \code{alpha = 0.05}.
##' @inheritParams summary.dml
##' @exportS3Method extreme_robustness_value dml
extreme_robustness_value.dml <- function(model, theta = 0, alpha = 0.05, rho2 = 1,...){
  conf <- confint(model, level = 1 - alpha,...)
  out <- setNames(rep(NA,nrow(conf)), rownames(conf))
  for (i in 1:nrow(conf)) {
    if (conf[i,1] <= theta & theta <= conf[i,2]) {
      out[i] <- 0
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    # print(side)
    if (alpha == 1) {
      # closed-form point XRV for row i's target: f0 = |theta - theta.s|/sqrt(S2),
      # XRV = f0^2/(1+f0^2). Map the row's target (rowname) to its results slot
      # (treat/all/untr) and combine S2 across cf.reps -- multi-target / multi-rep
      # safe (main[[1]] and whole-vector coef are not).
      slot.i <- unname(.target_to_slot[rownames(conf)[i]])
      S2.i   <- stats::median(extract_estimate(model$results$main[[slot.i]], "S2"))
      f0     <- unname(abs(theta - coef(model)[rownames(conf)[i]]) / sqrt(S2.i))
      out[i] <- f0^2 / (1 + f0^2)
    } else {
      fn <- function(xrv) xrv_fun(xrv, dml.fit = model, par = names(out)[i],
                                  side = side, theta = theta, alpha = alpha,
                                  rho2 = rho2)
      out[i] <- optim(par = c(0.01), fn, lower = 0, upper = 1, method = "Brent")$par

      # fn <- function(par) {
      #   xrv <- par[1]
      #   yrv <- par[2]
      #   xrv_fun_2D(yrv = yrv, xrv = xrv, dml.fit = model,
      #                                  par = names(out)[i],
      #                             side = side, theta = theta, alpha = alpha)
      # }
      # # out[i] <- optim(par = c(0.01, 0.01), fn, lower = c(0, 0), upper = c(1, 1),
      # #                 method = "L-BFGS-B")$par[2]
      #
      # starts <- expand.grid(yrv = c(seq(0.01,0.99,0.05),1), xrv = c(seq(0.01,0.99,0.05),0.99))
      # best_val <- Inf
      # best_par <- NULL
      # for(j in 1:nrow(starts)){
      #   res <- optim(par = as.numeric(starts[j,]), fn, lower = c(0,0), upper=c(1-1e-8,1), method="L-BFGS-B")
      #   if(res$value < best_val){
      #     best_val <- res$value
      #     best_par <- res$par
      #   }
      # }
      # out[i] <- best_par[1]
    }
  }

  return(out)
  # grid <- seq(0, 0.99,by = 0.001)
  # values <- mapply(function(x,y) confidence_bounds(dml.fit, cf.y= x, cf.d = y), x = grid, y = grid)
  # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
  # grid[rv.idx]
}
########################################################################################

##' @rdname robustness_value
##' @exportS3Method sensemakr::robustness_value dml.bounds
robustness_value.dml.bounds <- function(model, theta = 0, alpha = 0.05, ...){
  model <- model$dml.fit
  conf <- confint(model, level = 1 - alpha,...)
  out <- setNames(rep(NA, nrow(conf)), rownames(conf))
  for (i in 1:nrow(conf)) {
    if (conf[i,1] <= theta & theta <= conf[i,2]) {
      out[i] <- 0
      next
    }
    side <- ifelse(theta < conf[i,1], "lwr", "upr")
    fn <- function(rv) rv_fun(rv, dml.fit = model, par = names(out)[i],
                              side = side, theta = theta, alpha = alpha)
    out[i] <- optim(par = c(0.01), fn, lower = 0, upper = 1, method = "Brent")$par
  }
  return(out)
  # grid <- seq(0, 0.99,by = 0.001)
  # values <- mapply(function(x,y) confidence_bounds(dml.fit, cf.y= x, cf.d = y), x = grid, y = grid)
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
#   # values <- mapply(function(x,y) confidence_bounds(dml.fit, cf.y= x, cf.d = y), x = grid, y = grid)
#   # rv.idx <- which(values[1,] <= theta & theta <= values[2,])[1]
#   # grid[rv.idx]
# }
