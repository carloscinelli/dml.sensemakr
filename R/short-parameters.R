
# computes standard error using influence function psi
psi.sd <- function(psi){
  sqrt(mean(psi^2))/sqrt(length(psi))
}

# trims propensity score
trim.ps <- function(ps, trim = 0.02){
  ps[ps < trim] <- trim
  ps[ps > 1-trim] <- 1 - trim
  return(ps)
}


# computes ate for npm
ate.npm <- function(y, d, parameter = "all",
                    yhat1, yhat0, dhat,
                    trim = 0.02){
  # trim propensity score
  dhat.t       <- trim.ps(dhat, trim = trim)

  # l
  l            <- switch(parameter, all = 1, treat = d/mean(d), untr = (1-d)/(1-mean(d)))

  # lbar = E[l|X]
  lbar         <- switch(parameter, all = 1, treat = dhat.t/mean(d), untr = (1-dhat.t)/(1-mean(d)))


  # ate
  gs           <- (d * yhat1 + (1 - d) * yhat0)
  RRs          <- ((d/dhat.t - (1-d)/(1-dhat.t)))*lbar
  Ms           <- (yhat1 - yhat0)*l
  theta.s      <- mean(Ms +  (y - gs)*RRs)
  psi.theta.s  <- Ms +   (y - gs)*RRs - theta.s

  # Scaling terms (still as "global" parameters)

  # sigma2
  sigma2.s     <- mean( (y-gs)^2)
  psi.sigma2.s <- ( (y-gs)^2 - sigma2.s)

  # nu2
  nu2.s        <-  mean(2*( (1/dhat.t)*lbar + (1/(1-dhat.t))*lbar)*l  - RRs^2)
  psi.nu2.s    <-  2*( (1/dhat.t)*lbar + (1/(1-dhat.t))*lbar)*l  - RRs^2 - nu2.s

  # S2
  S2           <- sigma2.s*nu2.s
  psi.S2       <- (sigma2.s*(psi.nu2.s) + nu2.s*psi.sigma2.s)

  # output
  out <- list(
    # influence functions
    psis      = list(psi.theta.s = psi.theta.s,
                     psi.sigma2.s = psi.sigma2.s,
                     psi.nu2.s    = psi.nu2.s,
                     psi.S2        = psi.S2),

    # estimates and se
    estimates = list(theta.s     = theta.s,
                     se.theta.s  = psi.sd(psi.theta.s),
                     sigma2.s    = sigma2.s,
                     se.sigma2.s = psi.sd(psi.sigma2.s),
                     nu2.s       = nu2.s,
                     se.nu2.s    = psi.sd(psi.nu2.s),
                     S2          = S2,
                     se.S2       = psi.sd(psi.S2),
                     cov.theta.S2 = mean(psi.theta.s*psi.S2)/length(psi.S2)))
}


# computes ate for plm
ate.plm <- function(y, d, yhat, dhat){

  # residuals
  resY         <- (y - yhat)
  resD         <- (d - dhat)

  # ate
  RRs          <- (resD/mean(resD^2))
  theta.s      <- mean(resY*RRs)
  eresY        <- (resY - theta.s*resD)
  psi.theta.s  <- (eresY*RRs)

  # sigma2
  sigma2.s       <- mean(eresY^2)
  psi.sigma2.s   <- eresY^2 - sigma2.s

  # nu2
  nu2.s          <- 1/mean(resD^2)
  psi.nu2.s      <- nu2.s - (resD)^2 * (nu2.s)^2

  # S2
  S2           <- sigma2.s*nu2.s
  psi.S2       <- (sigma2.s*(psi.nu2.s) + nu2.s*psi.sigma2.s)


  # output
  out <- list(
    # influence functions
    psis      = list(psi.theta.s = psi.theta.s,
                     psi.sigma2.s = psi.sigma2.s,
                     psi.nu2.s    = psi.nu2.s,
                     psi.S2        = psi.S2),

    # estimates and se
    estimates = list(theta.s     = theta.s,
                     se.theta.s  = psi.sd(psi.theta.s),
                     sigma2.s    = sigma2.s,
                     se.sigma2.s = psi.sd(psi.sigma2.s),
                     nu2.s       = nu2.s,
                     se.nu2.s    = psi.sd(psi.nu2.s),
                     S2          = S2,
                     se.S2       = psi.sd(psi.S2),
                     cov.theta.S2 = mean(psi.theta.s*psi.S2)/length(psi.S2)))
  return(out)
}



# computes ate for each group npm
ate.att.atu.npm <- function(dml, target, trim = 0.02) {
  g       <- c(ate = "all", att = "treat", atu = "untr")[target]
  ate.g   <- list()
  cf.reps <- dml$info$cf.reps
  y       <- dml$data$y
  d       <- dml$data$d
  x       <- dml$data$x

  for(j in g){
    res <- list()
    for(i in 1:cf.reps){
      dhat   <- dml$fits[[i]]$preds$dhat
      yhat0  <- dml$fits[[i]]$preds$yhat0
      yhat1  <- dml$fits[[i]]$preds$yhat1
      res[[i]] <- ate.npm(y = y, d = d,
                          parameter =  j,
                          yhat1 = yhat1,
                          yhat0 = yhat0,
                          dhat  = dhat,
                          trim = trim)
    }
    ate.g[[j]] <- res
  }
  return(ate.g)
}


# computes ate for each group npm
group.ate.npm <- function(dml, groups, trim = 0.02) {
  g       <- levels(groups)
  ate.g   <- list()
  cf.reps <- dml$info$cf.reps
  y       <- dml$data$y
  d       <- dml$data$d
  x       <- dml$data$x
  for(j in g){
    idx <- groups == j
    res <- list()
    for(i in 1:cf.reps){
      dhat   <- dml$fits[[i]]$preds$dhat
      yhat0  <- dml$fits[[i]]$preds$yhat0
      yhat1  <- dml$fits[[i]]$preds$yhat1
      res[[i]] <- ate.npm(y = y[idx], d = d[idx],
                          yhat1 = yhat1[idx],
                          yhat0 = yhat0[idx],
                          dhat  = dhat[idx],
                          trim = trim)
    }
    ate.g[[j]] <- res
  }
  return(ate.g)
}

# computes ate for each group plm
group.ate.plm <- function(dml, groups) {
  g     <- levels(groups)
  ate.g <- list()
  cf.reps <- dml$info$cf.reps
  y      <- dml$data$y
  d      <- dml$data$d
  for (j in g) {
    idx <- groups == j
    res <- list()
    for (i in 1:cf.reps) {
      dhat  <-   dml$fits[[i]]$preds$dhat
      yhat  <-   dml$fits[[i]]$preds$yhat
      res[[i]] <- ate.plm(y[idx], d[idx], yhat[idx], dhat[idx])
    }
    ate.g[[j]] <- res
  }
  return(ate.g)
}
