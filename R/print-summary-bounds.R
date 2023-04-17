
#'@export
print.dml.bounds <- function(x, ...){
  print(summary(x,...))
}

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

##'@export
print.summary_dml.bounds <- function(x, ...){
  cat("\n")
  cat("Debiased Machine Learning: Bounds on Omitted Variable Bias\n")
  cat("\n")
  # cat("Short Estimates and Bounds on Omitted Variable Bias\n")
  # cat("\n")
  cat("Sensitivity Parameters\n",
      "","r2ya.dx =", paste0(x$info$r2ya.dx,"\n"),
      "","r2rr =", paste0(x$info$r2.rr, "\n"),
      "", "rho =", paste0(x$info$rho,""), "\n")
  # cat("\nBounds on Average Treatment Effect:", "\n\n")
  # print()
  main <- x$main
  if (!is.null(main)) {
    cat("\n")
    for (i in seq_along(main)) {
      cat("\nBounds on Average Treatment Effect:", names(main)[i], "\n\n")
      print(main[[i]])
    }
  }
  groups <- x$groups

  if (!is.null(groups)) {
    cat("\n")
    for (i in seq_along(groups)) {
      cat("\nBounds on Group Average Treatment Effect:","Group", names(groups)[i], "\n\n")
      print(groups[[i]])
    }
  }
  cat("\nNote: DML estimates combined using the", x$combine.method, "method.")
}

#' @export
coef.dml.bounds <- function(object, combine.method = "median", ...){
  # ate <- rbind(ate = sapply(object$coefs$main, function(x) x[combine.method, "estimate"]))
  if (!is.null(object$coef$main)) {
    ate <- lapply(object$coefs$main, function(x) sapply(x, function(x) x[combine.method, "estimate"]))
    ate <- do.call("rbind", ate)
    rownames(ate) <- paste0("ate.", rownames(ate))
  } else{
    ate = NULL
  }

  if (!is.null(object$coef$groups)) {
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "estimate"]))
    gate <- do.call("rbind", gate)
    rownames(gate) <- paste0("gate.", rownames(gate))
  } else{
    gate = NULL
  }

  t(rbind(ate = ate, gate = gate))
}

#' @export
se <- function(object, ...){
  UseMethod("se")
}

#' @export
se.dml.bounds <- function(object, combine.method = "median", ...){
  # ate <- rbind(ate = sapply(object$coefs$main, function(x) x[combine.method, "se"]))
  if (!is.null(object$coef$main)) {
    ate <- lapply(object$coefs$main, function(x) sapply(x, function(x) x[combine.method, "se"]))
    ate <- do.call("rbind", ate)
    rownames(ate) <- paste0("ate.", rownames(ate))
  } else{
    ate = NULL
  }
  if (!is.null(object$coef$groups)) {
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "se"]))
    gate <- do.call("rbind", gate)
    rownames(gate) <- paste0("gate.", rownames(gate))
  } else{
    gate = NULL
  }
  t(rbind(ate = ate, gate = gate))
}


#' @export
confint.dml.bounds <- function(object, params = NULL, level = 0.95, combine.method = "median", ...){
  cf  <- t(coef(object, combine.method = combine.method))
  ses <- t(se(object, combine.method = combine.method))
  loop <- setNames(rownames(cf), rownames(cf))
  out <- lapply(loop , function(x)calc_confint(cf = cf[x,], params = params, ses = ses[x,], level = level))
  if (length(out) == 1 ) {
    out <- out[[1]]
  }
  out
}




#'@export
print.confidence.bounds <- function(x, ...){
   print.table(x)
  cat("\nConfidence level: point =",
      paste0(attributes(x)$conf.levels["point"]*100, "%;"),
      "region =",
      paste0(attributes(x)$conf.levels["region"]*100, "%."))
  cat("\nSensitivity parameters: r2ya.dx =",
      paste0(attributes(x)$sens.param["r2ya.dx"], ";"),
      "r2.rr =",
      paste0(attributes(x)$sens.param["r2.rr"], ";"),
      "rho2 =",
      paste0(attributes(x)$sens.param["rho2"], ".\n"))
}
