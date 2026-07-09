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
##'           labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE)
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
##' @returns For \code{summary}: an object of class \code{summary_dml}. For \code{coef}: a named numeric vector of coefficients. For \code{se}: a named numeric vector of standard errors. For \code{confint}: a matrix with confidence intervals. For \code{print}: the input object, invisibly.
##' @export
summary.dml <- function(object, combine.method = "median", ...){

  out <- list()
  out$info <- object$info
  out$combine.method <- combine.method

  # goodness of fits
  comb_fun <- get(combine.method)
  is_cond <- isTRUE(object$info$conditional)
  if (is_cond && identical(object$info$target, "atu")) {
    # conditional ATU: yhat0 is NULL; report R2 of yhat1 on treated units only
    d1_idx <- object$data$d == 1
    out$r2y <- comb_fun(sapply(object$fits,
                               function(f) r2(f$preds$yhat1[d1_idx], object$data$y[d1_idx])))
  } else if (is_cond) {
    # conditional ATT: yhat1 is NULL; report R2 of yhat0 on control units only
    d0_idx <- object$data$d == 0
    out$r2y <- comb_fun(sapply(object$fits,
                               function(f) r2(f$preds$yhat0[d0_idx], object$data$y[d0_idx])))
  } else {
    out$r2y <- comb_fun(sapply(object$fits, function(x) r2(x$preds$yhat, object$data$y)))
  }
  out$r2d <- comb_fun(sapply(object$fits, function(x) r2(x$preds$dhat, object$data$d)))

  # main coefs — only show slots matching the requested target(s)
  main <- lapply(.target_coefs(object), function(x) x[combine.method, ])
  main <- do.call("rbind", main)
  rownames(main) <- .slot_to_target[rownames(main)]
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


# Maps internal slot names to user-facing target labels and vice-versa.
.slot_to_target <- c(all = "ate", treat = "att", untr = "atu")
.target_to_slot <- c(ate = "all", att = "treat", atu = "untr")

# Internal: return only the coefs$main slots that match the requested target(s).
.target_coefs <- function(object) {
  keep <- unname(.target_to_slot[object$info$target])
  keep <- keep[keep %in% names(object$coefs$main)]
  object$coefs$main[keep]
}

##' @rdname summary.dml
##' @description  The \code{coef} function extracts the coefficients.
##' @export
coef.dml <- function(object, combine.method = "median", ...){
  tc   <- .target_coefs(object)
  ate  <- sapply(tc, function(x) x[combine.method, "estimate"])
  names(ate) <- .slot_to_target[names(ate)]
  if (!is.null(object$coefs$groups)) {
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "estimate"])
  } else{
    gate = NULL
  }
  c(ate, gate = gate)
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
  tc   <- .target_coefs(object)
  ate  <- sapply(tc, function(x) x[combine.method, "se"])
  names(ate) <- .slot_to_target[names(ate)]
  if(!is.null(object$coefs$groups)){
    gate <- sapply(object$coefs$groups, function(x) x[combine.method, "se"])
  } else{
    gate = NULL
  }
  c(ate, gate = gate)
}

##' @rdname summary.dml
##' @description  The \code{confint} function extracts the standard errors.
##' @param level confidence level. Default is \code{0.95}.
##' @param parm character vector with the names of parameters.
##' @param interpret logical. Should a verbal interpretation of the DML procedure be printed? Default is \code{TRUE}.
##' @export
confint.dml <- function(object, parm = NULL, level = 0.95, combine.method = "median", ...){
  cf  <- coef(object, combine.method = combine.method)
  ses <- se(object, combine.method = combine.method)
  calc_confint(cf =cf, ses =ses,  parm = parm, level = level)
}

format_perc <- function (probs, digits) paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits), "%")

calc_confint <- function(cf, ses, parm=NULL, level) {
  pnames <- names(ses)
  if (is.matrix(cf))
    cf <- setNames(as.vector(cf), pnames)
  if (is.null(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  pct <- format_perc(a, 3)
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}


expand.cmat <- function(cmat){
  cmat <- cbind(cmat, cmat[, 1]/cmat[, 2])
  cmat <- cbind(cmat, 2*pnorm(abs(cmat[, 3]), lower.tail = FALSE))
  colnames(cmat) <-  c("estimate", "se", "t.value", "p.value")
  class(cmat) <- "cmat"
  return(cmat)
}


##' @rdname summary.dml
##' @export
print.summary_dml <- function(x, digits = max(3L, getOption("digits") - 3L), interpret = TRUE, ...){
  cat("\n")
  cat("Debiased Machine Learning\n")
  cat("\n")
  cat("", "Model:", ifelse(x$info$model == "plm", "Partially Linear", "Nonparametric"), "\n")
  cat("", "Cross-Fitting:",x$info$cf.folds, "folds,", x$info$cf.reps, "reps", "\n")
  yreg0_name <- if (is.null(x$info$yreg$yreg0)) "(not used)" else attr(x$info$yreg$yreg0$method, "name")
  yreg1_name <- if (is.null(x$info$yreg$yreg1)) "(not used)" else attr(x$info$yreg$yreg1$method, "name")
  cat("", "ML Method:",
      "outcome", paste0("(yreg0:", yreg0_name,
                        ", yreg1:", yreg1_name, ", R2 = ", round(x$r2y*100,3), "%),"),
      "treatment", paste0("(", attr(x$info$dreg$method,"name"), ", R2 = ", round(x$r2d*100,3), "%)\n"))
  cat("", "Tuning:", ifelse(x$info$dirty.tuning, "dirty", "clean"), "\n")

  cat("\n")

  target_label <- if (isTRUE(x$info$conditional)) {
    if (identical(x$info$target, "atu")) "Conditional ATU" else "Conditional ATT"
  } else "Average Treatment Effect"
  cat(target_label, ":", "\n\n")
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
    # yreg is stored as list(yreg0, yreg1); report the side actually used
    # (yreg0 for ATE/ATT/PLM, yreg1 for conditional ATU).
    yreg.used   <- if (!is.null(x$info$yreg$yreg0)) x$info$yreg$yreg0 else x$info$yreg$yreg1
    yreg.method <- yreg.used$method$label
    yreg.lib    <- yreg.used$method$library[[1]]
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
  printCoefmat(x, has.Pvalue = TRUE, P.values = TRUE, signif.stars = TRUE, digits = digits, ...)
}

# vcov.dml <- function(object, ...){
#   se(object, ...)^2
# }
