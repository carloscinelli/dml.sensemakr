
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
ate.npm <- function(y, d,
                    yhat1, yhat0, dhat, idx = 1,
                    trim = 0.02){

  # trim propensity score
  dhat.t       <- trim.ps(dhat, trim = 0.02)

  # ate
  gs           <- d * yhat1 + (1 - d) * yhat0
  RRs          <- (d/dhat.t - (1-d)/(1-dhat.t))*(idx/mean(idx))
  Ms           <- (yhat1 - yhat0)*(idx/mean(idx))
  theta.s      <- mean(Ms + RRs * (y - gs))
  psi.theta.s  <- Ms + RRs * (y - gs) - theta.s

  # sigma2
  sigma2.s     <- mean((y-gs)^2)
  psi.sigma2.s <- (y-gs)^2 - sigma2.s

  # nu2
  nu2.s        <-  mean(2*(1/dhat.t + 1/(1-dhat.t)) - RRs^2)
  psi.nu2.s    <-  2*(1/dhat.t + 1/(1-dhat.t)) - RRs^2 - nu2.s

  # output
  out <- list(
    # influence functions
    psis      = list(psi.theta.s = psi.theta.s,
                     psi.sigma2.s = psi.sigma2.s,
                     psi.nu2.s    = psi.nu2.s),
    # estimates and se
    estimates = list(theta.s     = theta.s,
                     se.theta.s  = psi.sd(psi.theta.s),
                     sigma2.s    = sigma2.s,
                     se.sigma2.s  = psi.sd(psi.sigma2.s),
                     nu2.s       = nu2.s,
                     se.nu2.s     = psi.sd(psi.nu2.s)))
}


# computes ate for plm
ate.plm <- function(y, d, yhat, dhat, idx= 1){

  # residuals
  resY         <- (y - yhat)
  # resY         <- resY - mean(resY)
  resD         <- (d - dhat)
  # resD         <- resD - mean(resD)

  # ate
  RRs          <- (resD/mean(resD^2))*idx/mean(idx)
  theta.s      <- mean(resY*RRs)
  eresY        <- (resY - theta.s*resD)
  psi.theta.s  <- (eresY*RRs)

  # theta.s      <- mean(resY*resD)/mean(resD^2)
  # eresY        <- (resY - theta.s*resD)
  # psi.theta.s  <- (eresY*resD)/mean(resD^2)

  # sigma2
  sigma2.s       <- mean(eresY^2)
  psi.sigma2.s   <- eresY^2 - sigma2.s

  # nu2
  nu2.s          <- 1/mean(resD^2)
  psi.nu2.s      <- nu2.s - (resD)^2 * (nu2.s)^2

  # output
  out <- list(
    # influence functions
    psis      = list(psi.theta.s = psi.theta.s,
                     psi.sigma2.s = psi.sigma2.s,
                     psi.nu2.s    = psi.nu2.s),
    # estimates and se
    estimates = list(theta.s     = theta.s,
                     se.theta.s  = psi.sd(psi.theta.s),
                     sigma2.s    = sigma2.s,
                     se.sigma2.s  = psi.sd(psi.sigma2.s),
                     nu2.s       = nu2.s,
                     se.nu2.s     = psi.sd(psi.nu2.s)))
  return(out)
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
      res[[i]] <- ate.npm(y[idx], d[idx], yhat1[idx], yhat0[idx], dhat[idx], trim = trim)
      # res[[i]] <- ate.npm(y, d, yhat1, yhat0, dhat,idx = idx, trim = trim)
      # res[[i]] <- ate.npm(y, d*idx, yhat1/mean(idx), yhat0/mean(idx), dhat*idx, trim = trim)
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
  x      <- dml$data$x
  for(j in g){
    idx <- groups == j
    res <- list()
    for(i in 1:cf.reps){
      dhat  <-   dml$fits[[i]]$preds$dhat
      yhat  <-   dml$fits[[i]]$preds$yhat
      # res[[i]] <- ate.plm(y, d, yhat, dhat, idx=idx)
      # res[[i]] <- ate.plm(y*idx, d*idx, yhat*idx, dhat*idx)
      # res[[i]] <- ate.plm(y, d*idx, yhat, dhat*idx)
      res[[i]] <- ate.plm(y[idx], d[idx], yhat[idx], dhat[idx])
    }
    ate.g[[j]] <- res
  }
  return(ate.g)
}
