##' coef, se, confint, print and summary methods for DML
##'
##' The \code{print} and \code{summary} methods provide descriptions of the results obtained with the function \code{\link{dml}}.
##' @param object an object of class \code{\link{dml}}.
##' @param combine.method method to combine the results of each repetition of the DML fit. Options are \code{mean} and \code{median}. Default is \code{median}.
##' @param ... arguments passed to other methods.
##' @examples
##' # loads package
##' library(dml.sensemakr)
##'
##' ## loads data
##' data("pension")
##'
##' # set the outcome
##' y <- pension$net_tfa  # net total financial assets
##'
##' # set the treatment
##' d <- pension$e401    # 401K eligibility
##'
##' # set the covariates (a matrix)
##' x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)
##'
##' ## compute income quartiles for group ATE.
##' g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE),
##'           labels = c("q1", "q2", "q3", "q4"), include.lowest = T)
##'
##' # run DML (nonparametric model)
##' ## 2 folds (change as needed)
##' ## 1 repetition (change as needed)
##' dml.401k <- dml(y, d, x, model = "npm", groups = g1, cf.folds = 2, cf.reps = 1)
##'
##'
##' summary(dml.401k)
##' summary(dml.401k, combine.method = "mean")
##' coef(dml.401k)
##' coef(dml.401k, combine.method = "mean")
##' se(dml.401k)
##' confint(dml.401k, combine.method = "mean")
##'
##' @export
summary.dml <- function(object, combine.method = "median", ...){

  out <- list()
  out$info <- object$info
  out$combine.method <- combine.method

  # goodness of fits
  comb_fun <- get(combine.method)
  out$r2y <- comb_fun(sapply(object$fits, function(x) r2(x$preds$yhat, object$data$y)))
  out$r2d <- comb_fun(sapply(object$fits, function(x) r2(x$preds$dhat, object$data$d)))

  # main coefs
  # main <- rbind(object$coefs$main[combine.method,])
  # rownames(main) <- "ate"
  main <- lapply(object$coefs$main, function(x) x[combine.method, ])
  main <- do.call("rbind", main)
  rownames(main) <- paste0("ate.", rownames(main))
  main <- expand.cmat(main)
  out$main <- main

  # check for groups
  no.groups <- is.null(object$coefs$groups)
  if (!no.groups) {
    groups <- lapply(object$coefs$groups, function(x) x[combine.method, ])
    groups <- do.call("rbind", groups)
    rownames(groups) <- paste0("gate.", rownames(groups))
    groups <- expand.cmat(groups)
    out$groups <- groups
  }

  class(out) <- "summary_dml"
  return(out)
}


##' @rdname summary.dml
##' @description  The \code{coef} function extracts the coefficients.
##' @export
coef.dml <- function(object, combine.method = "median", ...){
  ate <- sapply(object$coefs$main, function(x) x[combine.method, "estimate"])
    #object$coefs$main[combine.method, "estimate"]
  if (!is.null(object$coef$groups)) {
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "estimate"])
  } else{
    gate = NULL
  }
  c(ate = ate, gate = gate)
}

##' @rdname summary.dml
##' @export
se <- function(object, ...){
  UseMethod("se")
}

##' @rdname summary.dml
##' @description  The \code{se} function extracts the standard errors.
##' @export
se.dml <- function(object, combine.method = "median", ...){
  ate <- sapply(object$coefs$main, function(x) x[combine.method, "se"])
    #object$coefs$main[combine.method, "se"]
  if(!is.null(object$coefs$groups)){
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "se"])
  } else{
    gate = NULL
  }
  c(ate= ate, gate = gate)
}

##' @rdname summary.dml
##' @description  The \code{confint} function extracts the standard errors.
##' @param level confidence level. Default is \code{0.95}.
##' @param params character vector with the names of parameters.
##' @export
confint.dml <- function(object, params = NULL, level = 0.95, combine.method = "median", ...){
  cf  <- coef(object, combine.method = combine.method)
  ses <- se(object, combine.method = combine.method)
  calc_confint(cf =cf, ses =ses,  params = params, level = level)
}
                   
format.perc <- function (probs, digits) paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")

calc_confint <- function(cf, ses, params=NULL, level) {
  pnames <- names(ses)
  if (is.matrix(cf))
    cf <- setNames(as.vector(cf), pnames)
  if (is.null(params))
    params <- pnames
  else if (is.numeric(params))
    params <- pnames[params]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- format.perc(a, 3)
  ci <- array(NA_real_, dim = c(length(params), 2L), dimnames = list(params, pct))
  ci[] <- cf[params] + ses[params] %o% fac
  ci
}


expand.cmat <- function(cmat){
  cmat <- cbind(cmat, cmat[, 1]/cmat[, 2])
  cmat <- cbind(cmat, 2*pnorm(abs(cmat[, 3]), lower.tail = F))
  colnames(cmat) <-  c("estimate", "se", "t.value", "p.value")
  class(cmat) <- "cmat"
  return(cmat)
}


##' @rdname summary.dml
##' @export
print.summary_dml <- function(x, digits = max(3L, getOption("digits") - 3L), interpret = T, ...){
  cat("\n")
  cat("Debiased Machine Learning\n")
  cat("\n")
  cat("", "Model:", ifelse(x$info$model == "plm", "Partially Linear", "Nonparametric"), "\n")
  cat("", "Cross-Fitting:",x$info$cf.folds, "folds,", x$info$cf.reps, "reps", "\n")
  cat("", "ML Method:",
      "outcome", paste0("(", attr(x$info$yreg$method, "name"), ", R2 = ", round(x$r2y*100,3), "%),"),
      "treatment", paste0("(", attr(x$info$dreg$method,"name"), ", R2 = ", round(x$r2d*100,3), "%)\n"))
  cat("", "Tuning:", ifelse(x$info$dirty.tuning, "dirty", "clean"), "\n")

  cat("\n")

  cat("Average Treatment Effect:", "\n\n")
  print(x$main, digits = digits)

  no.groups <- is.null(x$groups)
  if (!no.groups) {
    cat("\n")
    cat("Group Average Treatment Effect:", "\n\n")
    print(x$groups, digits = digits)
    cat("\n")
  }
  cat("Note: DML estimates combined using the", x$combine.method, "method.")

  if (interpret) {
    yreg.method <- x$info$yreg$method$label
    yreg.lib    <- x$info$yreg$method$library[[1]]
    dreg.method <- x$info$dreg$method$label
    dreg.lib    <- x$info$dreg$method$library[[1]]
    cf.folds    <- x$info$cf.folds
    cf.reps     <- x$info$cf.reps
    c.method    <- x$combine.method

    cat("\n\nVerbal interpretation of DML procedure:")
    cat(paste0("\n\n-- Average treatment effects were estimated using DML with ",cf.folds,"-fold cross-fitting. In order to reduce the variance that stems from sample splitting, we repeated the procedure ", cf.reps ," times. Estimates are combined using the ", c.method, " as the final estimate, incorporating variation across experiments into the standard error as described in Chernozhukov et al. (2018). The outcome regression uses ", yreg.method, " from the R package ", yreg.lib,"; the treatment regression uses ", dreg.method," from the R package ", dreg.lib, "."))
  }
}

##' @param x an object of class \code{\link{dml}}.
##' @param digits minimal number of significant digits.
##' @rdname summary.dml
##' @export
print.dml <- function(x, digits = max(3L, getOption("digits") - 3L), combine.method = "median", ...){
  cat("\n")
  cat("Debiased Machine Learning\n")
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  if (length(coef(x))) {
    cat("Estimates:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}



#' @export
print.cmat <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  colnames(x) <-  c("Estimate", "Std. Error", "t value", "P(>|t|)")
  # rownames(x) <-  toupper(rownames(x))
  # rownames(x) <- sapply(strsplit(rownames(x), split = "\\."), function(x) paste(x, collapse = " "))
  printCoefmat(x, has.Pvalue = T, P.values = T, signif.stars = T, digits = digits, ...)
}

# vcov.dml <- function(object, ...){
#   se(object, ...)^2
# }
