
# computes standard error using influence function psi (optionally weighted)
psi.sd <- function(psi, w = NULL){
  if (is.null(w)) {
    w <- rep(1, length(psi))
  }
  w_norm <- w / sum(w)
  Ew_psi2 <- sum(w_norm * psi^2)
  n_eff <- (sum(w)^2) / sum(w^2)
  sqrt(Ew_psi2) / sqrt(n_eff)
}

# computes conditional ATT for npm (conditional/DiD version)
# returns estimates and influence functions for theta.s, sigma2.0s, nu2.0s, S02
att.npm.cond <- function(y, d,
                         yhat0,
                         yhat1 = NULL,
                         dhat, phat,
                         trim   = 0.02,
                         centered = FALSE,
                         nu2.weight = NULL) {

  trim.summary <- trim.ps(dhat, trim = trim)
  dhat.t       <- trim.summary$ps

  l    <- d / phat
  lc   <- (1 - d) / (1 - phat)
  lbar <- dhat.t / phat

  gs        <- rep(NA, length(y))
  gs[d == 1] <- if (is.null(yhat1)) y[d == 1] else yhat1[d == 1]
  gs[d == 0] <- yhat0[d == 0]

  # ATT (same score as unconditional)
  RRs         <- d / phat - ((1 - d) / (1 - dhat.t)) * lbar
  Ms          <- (y - yhat0) * l
  theta.s     <- mean(Ms + (1 - d) * (y - gs) * RRs) / mean(l)
  psi.theta.s <- (Ms + (1 - d) * (y - gs) * RRs - theta.s * l) / mean(l)

  # E[g0s | D=1]  and  E[Y | D=1]
  theta.0s     <- mean(l * yhat0 + (((1 - d) * dhat.t) / ((1 - dhat.t) * phat)) * (y - yhat0)) / mean(l)
  psi.theta.0s <- (l * (yhat0 - theta.0s) + (((1 - d) * dhat.t) / ((1 - dhat.t) * phat)) * (y - yhat0)) / mean(l)

  theta.1s     <- mean(l * y) / mean(l)
  psi.theta.1s <- (l * (y - theta.1s)) / mean(l)

  # sigma2.0s  =  E[(Y - g0s)^2 | D=0]
  sigma2.0s     <- mean(lc * (y - yhat0)^2) / mean(lc)
  psi.sigma2.0s <- lc * ((y - yhat0)^2 - sigma2.0s) / mean(lc)

  # sigma2.1s  (optional, requires yhat1)
  if (!is.null(yhat1)) {
    sigma2.1s     <- mean(l * (y - yhat1)^2, na.rm = TRUE) / mean(l)
    psi.sigma2.1s <- l * ((y - yhat1)^2 - sigma2.1s) / mean(l)
  }

  # nu2.0s  =  conditional imbalance (propensity-score based)
  if (is.null(nu2.weight)) nu2.weight <- rep(1, length(d))

  if (centered) {
    nu2.0s <- weighted.mean(
      (d * (phat - dhat.t) * (phat + dhat.t - 2) +
         (phat^2) * (1 - 2 * dhat.t) -
         (dhat.t^2) * (1 - 2 * phat)) /
        ((phat^2) * (1 - phat) * (1 - dhat.t)^2),
      w = nu2.weight) / weighted.mean(2 * l - lc, w = nu2.weight)

    psi.nu2.0s <- (
      (d * (phat - dhat.t) * (phat + dhat.t - 2) +
         (phat^2) * (1 - 2 * dhat.t) -
         (dhat.t^2) * (1 - 2 * phat)) /
        ((phat^2) * (1 - phat) * (1 - dhat.t)^2) -
        (2 * l - lc) * nu2.0s
    ) / weighted.mean(2 * l - lc, w = nu2.weight)
  } else {
    OX <- dhat.t / (1 - dhat.t)
    O  <- phat   / (1 - phat)
    nu2.0s     <- weighted.mean(2 * l * (OX / O) - lc * (OX / O)^2, w = nu2.weight) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
    psi.nu2.0s <- (2 * l * (OX / O) - lc * (OX / O)^2 - (2 * l - lc) * nu2.0s) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
  }

  # S02 = sigma2.0s * nu2.0s
  S02     <- sigma2.0s * nu2.0s
  psi.S02 <- sigma2.0s * psi.nu2.0s + nu2.0s * psi.sigma2.0s

  list(
    psis = list(
      psi.theta.s   = psi.theta.s,
      psi.theta.0s  = psi.theta.0s,
      psi.theta.1s  = psi.theta.1s,
      psi.sigma2.s  = psi.sigma2.0s,
      psi.sigma2.1s = if (is.null(yhat1)) 0 else psi.sigma2.1s,
      psi.nu2.s     = psi.nu2.0s,
      psi.S2        = psi.S02
    ),
    estimates = list(
      theta.s      = theta.s,
      theta.0s     = theta.0s,
      theta.1s     = theta.1s,
      se.theta.s   = psi.sd(psi.theta.s),
      se.theta.0s  = psi.sd(psi.theta.0s),
      se.theta.1s  = psi.sd(psi.theta.1s),
      sigma2.s     = sigma2.0s,
      se.sigma2.s  = psi.sd(psi.sigma2.0s),
      nu2.s        = nu2.0s,
      se.nu2.s     = psi.sd(psi.nu2.0s, w = nu2.weight),
      S2           = S02,
      se.S2        = psi.sd(psi.S02),
      cov.theta.S2 = mean(psi.theta.s * psi.S02) / length(psi.S02)
    ),
    trim.summary = trim.summary
  )
}

# computes conditional ATU for npm (conditional/DiD version)
# Mirror of att.npm.cond() with the treated/untreated roles swapped:
#   D -> (1 - D),  p -> (1 - p),  pi(X) -> (1 - pi(X)),  g0s -> g1s.
# The outcome regression is fit on the TREATED units (yhat1 = g1s), and the
# control regression (yhat0) is not used.
# returns estimates and influence functions for theta.s, sigma2.1s, nu2.1s, S12
atu.npm.cond <- function(y, d,
                         yhat1,
                         yhat0 = NULL,
                         dhat, phat,
                         trim   = 0.02,
                         centered = FALSE,
                         nu2.weight = NULL) {

  trim.summary <- trim.ps(dhat, trim = trim)
  dhat.t       <- trim.summary$ps

  # target = untreated: l is the (1-D)/(1-p) weight; lc is its complement
  l    <- (1 - d) / (1 - phat)
  lc   <- d / phat
  lbar <- (1 - dhat.t) / (1 - phat)

  gs         <- rep(NA, length(y))
  gs[d == 0] <- if (is.null(yhat0)) y[d == 0] else yhat0[d == 0]
  gs[d == 1] <- yhat1[d == 1]

  # ATU (same score as unconditional untr)
  RRs         <- (d / dhat.t) * lbar - ((1 - d) / (1 - phat))
  Ms          <- (yhat1 - y) * l
  theta.s     <- mean(Ms + d * (y - gs) * RRs) / mean(l)
  psi.theta.s <- (Ms + d * (y - gs) * RRs - theta.s * l) / mean(l)

  # E[g1s | D=0]  and  E[Y | D=0]
  theta.1s     <- mean(l * yhat1 + (((d * (1 - dhat.t)) / (dhat.t * (1 - phat)))) * (y - yhat1)) / mean(l)
  psi.theta.1s <- (l * (yhat1 - theta.1s) + (((d * (1 - dhat.t)) / (dhat.t * (1 - phat)))) * (y - yhat1)) / mean(l)

  theta.0s     <- mean(l * y) / mean(l)
  psi.theta.0s <- (l * (y - theta.0s)) / mean(l)

  # sigma2.1s  =  E[(Y - g1s)^2 | D=1]
  sigma2.1s     <- mean(lc * (y - yhat1)^2) / mean(lc)
  psi.sigma2.1s <- lc * ((y - yhat1)^2 - sigma2.1s) / mean(lc)

  # sigma2.0s  (optional, requires yhat0)
  if (!is.null(yhat0)) {
    sigma2.0s     <- mean(l * (y - yhat0)^2, na.rm = TRUE) / mean(l)
    psi.sigma2.0s <- l * ((y - yhat0)^2 - sigma2.0s) / mean(l)
  }

  # nu2.1s  =  conditional imbalance (propensity-score based)
  if (is.null(nu2.weight)) nu2.weight <- rep(1, length(d))

  if (centered) {
    # ATT centered imbalance with p -> 1-p, pi -> 1-pi, D -> 1-D
    pa <- 1 - phat     # 1 - p
    ba <- 1 - dhat.t   # 1 - pi(X)
    da <- 1 - d        # 1 - D
    num.nu <- (da * (pa - ba) * (pa + ba - 2) +
                 (pa^2) * (1 - 2 * ba) -
                 (ba^2) * (1 - 2 * pa)) /
              ((pa^2) * (1 - pa) * (1 - ba)^2)

    nu2.1s     <- weighted.mean(num.nu, w = nu2.weight) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
    psi.nu2.1s <- (num.nu - (2 * l - lc) * nu2.1s) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
  } else {
    OX <- (1 - dhat.t) / dhat.t   # (1 - pi)/pi  = 1 / O_X
    O  <- (1 - phat)   / phat     # (1 - p)/p    = 1 / O
    nu2.1s     <- weighted.mean(2 * l * (OX / O) - lc * (OX / O)^2, w = nu2.weight) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
    psi.nu2.1s <- (2 * l * (OX / O) - lc * (OX / O)^2 - (2 * l - lc) * nu2.1s) /
                  weighted.mean(2 * l - lc, w = nu2.weight)
  }

  # S12 = sigma2.1s * nu2.1s
  S12     <- sigma2.1s * nu2.1s
  psi.S12 <- sigma2.1s * psi.nu2.1s + nu2.1s * psi.sigma2.1s

  list(
    psis = list(
      psi.theta.s   = psi.theta.s,
      psi.theta.0s  = psi.theta.0s,
      psi.theta.1s  = psi.theta.1s,
      psi.sigma2.s  = psi.sigma2.1s,
      psi.sigma2.0s = if (is.null(yhat0)) 0 else psi.sigma2.0s,
      psi.nu2.s     = psi.nu2.1s,
      psi.S2        = psi.S12
    ),
    estimates = list(
      theta.s      = theta.s,
      theta.0s     = theta.0s,
      theta.1s     = theta.1s,
      se.theta.s   = psi.sd(psi.theta.s),
      se.theta.0s  = psi.sd(psi.theta.0s),
      se.theta.1s  = psi.sd(psi.theta.1s),
      sigma2.s     = sigma2.1s,
      se.sigma2.s  = psi.sd(psi.sigma2.1s),
      nu2.s        = nu2.1s,
      se.nu2.s     = psi.sd(psi.nu2.1s, w = nu2.weight),
      S2           = S12,
      se.S2        = psi.sd(psi.S12),
      cov.theta.S2 = mean(psi.theta.s * psi.S12) / length(psi.S12)
    ),
    trim.summary = trim.summary
  )
}

# trims propensity score
trim.ps <- function(ps, trim = 0.02){
  if (is.numeric(trim)) {
    if (length(trim) == 1) {
      trim <- list(lower = trim, upper = 1 - trim)
    } else {
      stop("'trim' must be a single number, or a named list.")
    }
  } else if (is.list(trim)) {
    if (!all(c("lower", "upper") %in% names(trim))) {
      stop("'trim' list must be named with 'lower' and 'upper'.")
    }
  }
  n <- length(ps)

  # Identify indices of trimmed observations
  trimmed_low <- which(ps < trim$lower)
  trimmed_high <- which(ps > trim$upper)
  trimmed_indices <- sort(unique(c(trimmed_low, trimmed_high)))
  trimmed_num <- length(trimmed_indices)
  trimmed_prop <- trimmed_num/length(ps)

  ps[ps < trim$lower] <- trim$lower
  ps[ps > trim$upper] <- trim$upper
  return(list(
    ps = ps,
    trim = trim,
    trimmed_indices = list(low = unique(trimmed_low),
                           high = unique(trimmed_high),
                           all = trimmed_indices),
    trimmed_num = list(low = length(unique(trimmed_low)),
                       high = length(unique(trimmed_high)),
                       all = trimmed_num),
    trimmed_prop = list(low = length(unique(trimmed_low))/n,
                        high = length(unique(trimmed_high))/n,
                        all = trimmed_num/n)
  ))
}


# computes ate for npm
ate.npm <- function(y, d, parameter = "all",
                    yhat1, yhat0, dhat, phat,
                    trim = 0.02){
  # trim propensity score
  trim.summary <- trim.ps(dhat, trim = trim)
  dhat.t       <- trim.summary$ps

  # l
  l            <- switch(parameter, all = 1, treat = d/phat, untr = (1-d)/(1-phat))

  # lbar = E[l|X]
  lbar         <- switch(parameter, all = 1, treat = dhat.t/phat, untr = (1-dhat.t)/(1-phat))


  # ate
  gs           <- rep(NA, length(y))
  gs[d == 1]   <- yhat1[d == 1]
  gs[d == 0]   <- yhat0[d == 0]
  RRs          <- switch(parameter,
                         all   = ((d/dhat.t - (1-d)/(1-dhat.t)))*lbar,
                         treat = d/phat - ((1-d)/(1-dhat.t))*lbar,
                         untr  = (d/dhat.t)*lbar - ((1-d)/(1-phat)))
  Ms           <- switch(parameter,
                         all   = (yhat1 - yhat0)*l,
                         treat = (y - yhat0)*l,
                         untr  = (yhat1 - y)*l)
  theta.s      <- switch(parameter,
                         all   = mean(Ms +  (y - gs)*RRs),
                         treat = mean(Ms +  (1-d)*(y - gs)*RRs),
                         untr  = mean(Ms +  d*(y - gs)*RRs)) / mean(l)
  psi.theta.s  <- switch(parameter,
                         all   = Ms + (y - gs)*RRs - theta.s * l,
                         treat = (Ms + (1-d)*(y - gs)*RRs - theta.s * l),
                         untr  = Ms + d*(y - gs)*RRs - theta.s * l) / mean(l)

  # Scaling terms (still as "global" parameters)

  # sigma2
  sigma2.s     <- mean( (y-gs)^2)
  psi.sigma2.s <- ( (y-gs)^2 - sigma2.s)

  # nu2
  Msa          <- switch(parameter,
                         all   = ( (1/dhat.t)*lbar + (1/(1-dhat.t))*lbar)*l,
                         treat = ( (1/phat) + (1/(1-dhat.t))*lbar)*l,
                         untr  = ( (1/dhat.t)*lbar + (1/(1-phat)))*l)
  nu2.s        <-  mean(2*Msa - RRs^2) / mean(2 * l - 1)
  psi.nu2.s    <-  (2*Msa  - RRs^2 - (2 * l - 1) * nu2.s) / mean(2 * l - 1)

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
                     cov.theta.S2 = mean(psi.theta.s*psi.S2)/length(psi.S2)),

    trim.summary = trim.summary)
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
      phat   <- dml$fits[[i]]$preds$phat
      dhat   <- dml$fits[[i]]$preds$dhat
      yhat0  <- dml$fits[[i]]$preds$yhat0
      yhat1  <- dml$fits[[i]]$preds$yhat1
      res[[i]] <- ate.npm(y = y, d = d,
                          parameter =  j,
                          yhat1 = yhat1,
                          yhat0 = yhat0,
                          dhat  = dhat,
                          phat  = phat,
                          trim  = trim)

      trimmed.all.idx <- res[[i]]$trim.summary$trimmed_indices$all
      trimmed.low.idx <- res[[i]]$trim.summary$trimmed_indices$low
      trimmed.high.idx <- res[[i]]$trim.summary$trimmed_indices$high
      res[[i]]$trim.summary$trimmed_obs <- list(
        all = list(y.trimmed = y[trimmed.all.idx],
                   d.trimmed = d[trimmed.all.idx],
                   x.trimmed = x[trimmed.all.idx, ]),
        low = list(y.trimmed = y[trimmed.low.idx],
                   d.trimmed = d[trimmed.low.idx],
                   x.trimmed = x[trimmed.low.idx, ]),
        high = list(y.trimmed = y[trimmed.high.idx],
                    d.trimmed = d[trimmed.high.idx],
                    x.trimmed = x[trimmed.high.idx, ])
      )
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
      phat   <- dml$fits[[i]]$preds$phat
      dhat   <- dml$fits[[i]]$preds$dhat
      yhat0  <- dml$fits[[i]]$preds$yhat0
      yhat1  <- dml$fits[[i]]$preds$yhat1
      res[[i]] <- ate.npm(y = y[idx], d = d[idx],
                          yhat1 = yhat1[idx],
                          yhat0 = yhat0[idx],
                          dhat  = dhat[idx],
                          phat  = phat[idx],
                          trim = trim)

      trimmed.all.idx <- res[[i]]$trim.summary$trimmed_indices$all
      trimmed.low.idx <- res[[i]]$trim.summary$trimmed_indices$low
      trimmed.high.idx <- res[[i]]$trim.summary$trimmed_indices$high
      res[[i]]$trim.summary$trimmed_obs <- list(
        all = list(y.trimmed = y[idx][trimmed.all.idx],
                   d.trimmed = d[idx][trimmed.all.idx],
                   x.trimmed = x[idx, ][trimmed.all.idx, ]),
        low = list(y.trimmed = y[idx][trimmed.low.idx],
                   d.trimmed = d[idx][trimmed.low.idx],
                   x.trimmed = x[idx, ][trimmed.low.idx, ]),
        high = list(y.trimmed = y[idx][trimmed.high.idx],
                    d.trimmed = d[idx][trimmed.high.idx],
                    x.trimmed = x[idx, ][trimmed.high.idx, ])
      )
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
