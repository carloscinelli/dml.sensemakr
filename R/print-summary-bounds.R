
##' Print, summary, coef, se, and confint methods for DML bounds
##'
##' @description Methods for objects of class \code{dml.bounds} created with \code{\link{dml_bounds}}.
##' @param x an object of class \code{dml.bounds} or \code{summary_dml.bounds}.
##' @param object an object of class \code{dml.bounds}.
##' @param combine.method method to combine the results of each repetition. Options are \code{mean} and \code{median}. Default is \code{median}.
##' @param digits number of significant digits to print.
##' @param ... arguments passed to other methods.
##' @param parm character vector with the names of parameters.
##' @param level confidence level. Default is \code{0.95}.
##' @returns For \code{print} and \code{summary}: the object, printed to console. For \code{coef}: a matrix of coefficient estimates. For \code{se}: a matrix of standard errors. For \code{confint}: a list of confidence interval matrices.
##' @name print.dml.bounds
NULL

#'@rdname print.dml.bounds
#'@export
print.dml.bounds <- function(x, ...){
  print(summary(x,...))
}

#'@rdname print.dml.bounds
#'@export
summary.dml.bounds <- function(object, combine.method = "median", ...){

  out <- list()
  out$info <- object$info

  out$combine.method <- combine.method

  # main coefs
  # main <- t(sapply(object$coefs$main, function(x)x[combine.method,]))
  # rownames(main) <- c("Short Estimate", "|Bias| Bound", "Lower Bound", "Upper Bound")
  # main <- expand.cmat(main)
  # out$main <- main

  # check for main
  no.main <- is.null(object$coefs$main)
  if(!no.main){
    main <- lapply(object$coefs$main, function(x) t(sapply(x, function(x)x[combine.method,])))
    main <- lapply(main, function(x){
      rownames(x) <- c("Short Estimate", "|Bias| Bound", "Lower Bound", "Upper Bound")
      x
    })
    main <- lapply(main, expand.cmat)
    out$main <- main
  }


  # check for groups
  no.groups <- is.null(object$coefs$groups)
  if(!no.groups){
    groups <- lapply(object$coefs$groups, function(x) t(sapply(x, function(x)x[combine.method,])))
    groups <- lapply(groups, function(x){
      rownames(x) <- c("Short Estimate", "|Bias| Bound", "Lower Bound", "Upper Bound")
      x
    })
    groups <- lapply(groups, expand.cmat)
    out$groups <- groups
  }

  class(out) <- "summary_dml.bounds"
  return(out)
}

##'@rdname print.dml.bounds
##'@export
print.summary_dml.bounds <- function(x, digits = 2, ...){
  cat("\n")
  cat("Debiased Machine Learning: Bounds on Omitted Variable Bias\n")
  cat("\n")
  # cat("Short Estimates and Bounds on Omitted Variable Bias\n")
  # cat("\n")
  cat("Sensitivity Parameters\n",
      "", "cf.y =", paste0(x$info$cf.y, "\n"),
      "", "cf.d =", paste0(x$info$cf.d, "\n"),
      "", "rho2 =", paste0(x$info$rho2, ""), "\n")
  # cat("\nBounds on Average Treatment Effect:", "\n\n")
  # print()
  main <- x$main
  if (!is.null(main)) {
    cat("\n")
    for (i in seq_along(main)) {
      # names(main) are the internal slots (all/treat/untr); name the estimand
      tg  <- .slot_to_target[names(main)[i]]
      lab <- c(ate = "Average Treatment Effect",
               att = "Average Treatment Effect on the Treated",
               atu = "Average Treatment Effect on the Untreated")[tg]
      if (is.na(lab)) lab <- paste("Average Treatment Effect:", names(main)[i])
      cat("\nBounds on ", lab, "\n\n", sep = "")
      print(main[[i]], digits = digits)
    }
  }
  groups <- x$groups

  if (!is.null(groups)) {
    cat("\n")
    for (i in seq_along(groups)) {
      cat("\nBounds on Group Average Treatment Effect:", "Group",
          names(groups)[i], "\n\n")
      print(groups[[i]], digits = digits)
    }
  }
  cat("\nNote: DML estimates combined using the", x$combine.method, "method.")
}

#' @rdname print.dml.bounds
#' @export
coef.dml.bounds <- function(object, combine.method = "median", ...){
  if (!is.null(object$coefs$main)) {
    ate <- lapply(object$coefs$main, function(x) sapply(x, function(x) x[combine.method, "estimate"]))
    ate <- do.call("rbind", ate)
    rownames(ate) <- .slot_to_target[rownames(ate)]
  } else{
    ate = NULL
  }

  if (!is.null(object$coefs$groups)) {
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "estimate"]))
    gate <- do.call("rbind", gate)
    rownames(gate) <- paste0(.group_marker, rownames(gate))
  } else{
    gate = NULL
  }

  t(rbind(ate = ate, gate = gate))
}

#' @rdname print.dml.bounds
#' @export
se.dml.bounds <- function(object, combine.method = "median", ...){
  if (!is.null(object$coefs$main)) {
    ate <- lapply(object$coefs$main, function(x) sapply(x, function(x) x[combine.method, "se"]))
    ate <- do.call("rbind", ate)
    rownames(ate) <- .slot_to_target[rownames(ate)]
  } else{
    ate = NULL
  }
  if (!is.null(object$coefs$groups)) {
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "se"]))
    gate <- do.call("rbind", gate)
    rownames(gate) <- paste0(.group_marker, rownames(gate))
  } else{
    gate = NULL
  }
  t(rbind(ate = ate, gate = gate))
}


#' @rdname print.dml.bounds
#' @export
confint.dml.bounds <- function(object, parm = NULL, level = 0.95, combine.method = "median", ...){
  cf  <- t(coef(object, combine.method = combine.method))
  ses <- t(se(object, combine.method = combine.method))
  loop <- setNames(rownames(cf), rownames(cf))
  out <- lapply(loop , function(x)calc_confint(cf = cf[x,], parm = parm, ses = ses[x,], level = level))
  # if (length(out) == 1 ) {
  #   out <- out[[1]]
  # }
  out
}




#'@rdname print.dml.bounds
#'@export
print.confidence.bounds <- function(x, ...){
   print.table(x)
  maximized <- isTRUE(attributes(x)$sens.param$max)
  cat("\nConfidence level: point =",
      paste0(attributes(x)$conf.levels["point"]*100, "%;"),
      "region =",
      paste0(attributes(x)$conf.levels["region"]*100, "%."))
  cat(if (maximized) {
        "\nMaximum sensitivity parameters: cf.y ="
      } else {
        "\nSensitivity parameters: cf.y ="
      },
      paste0(attributes(x)$sens.param["cf.y"], ";"),
      "cf.d =",
      paste0(attributes(x)$sens.param["cf.d"], ";"),
      "rho2 =",
      paste0(attributes(x)$sens.param["rho2"], ".\n"))
}
