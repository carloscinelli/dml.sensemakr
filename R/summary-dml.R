#' @export
summary.dml <- function(object, combine.method = "median", ...){

  out <- list()
  out$info <- object$info
  out$info <- object$info
  out$combine.method <- combine.method

  # goodness of fits
  comb_fun <- get(combine.method)
  out$r2y <- comb_fun(sapply(object$fits, function(x) r2(x$preds$yhat, object$data$y)))
  out$r2d <- comb_fun(sapply(object$fits, function(x) r2(x$preds$dhat, object$data$d)))

  # main coefs
  main <- rbind(object$coefs$main[combine.method,])
  rownames(main) <- "ate"
  main <- expand.cmat(main)
  out$main <- main

  # check for groups
  no.groups <- is.null(object$coefs$groups)
  if(!no.groups){
    groups <- lapply(object$coefs$groups, function(x) x[combine.method, ])
    groups <- do.call("rbind", groups)
    rownames(groups) <- paste0("gate.", rownames(groups))
    groups <- expand.cmat(groups)
    out$groups <- groups
  }

  class(out) <- "summary_dml"
  return(out)
}


#' @export
coef.dml <- function(object, combine.method = "median", ...){
  ate <- object$coefs$main[combine.method, "estimate"]
  if(!is.null(object$coef$groups)){
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "estimate"])
  } else{
    gate = NULL
  }
  c(ate = ate, gate = gate)
}

#' @export
se <- function(object, ...){
  UseMethod("se")
}

#' @export
se.dml <- function(object, combine.method = "median", ...){
  ate <- object$coefs$main[combine.method, "se"]
  if(!is.null(object$coefs$groups)){
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "se"])
  } else{
    gate = NULL
  }
  c(ate= ate, gate = gate)
}


#' @export
confint.dml <- function(object, parm, level = 0.95, combine.method = "median", ...){
  cf  <- coef(object, combine.method = combine.method)
  ses <- se(object, combine.method = combine.method)
  calc_confint(cf =cf, ses =ses,  parm = parm, level = level)
}


calc_confint <- function(cf, ses, parm, level) {
  pnames <- names(ses)
  if (is.matrix(cf))
    cf <- setNames(as.vector(cf), pnames)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- stats:::format.perc(a, 3)
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}


expand.cmat <- function(cmat){
  cmat <- cbind(cmat, cmat[, 1]/cmat[, 2])
  cmat <- cbind(cmat, 2*pnorm(abs(cmat[, 3]), lower.tail = F))
  colnames(cmat) <-  c("estimate", "se", "t.value", "p.value")
  class(cmat) <- "cmat"
  return(cmat)
}
