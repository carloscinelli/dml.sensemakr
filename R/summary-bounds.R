
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
  main <- t(sapply(object$coefs$main, function(x)x[combine.method,]))
  rownames(main) <- c("Short Estimate", "|Bias| Bound", "Lower Bound", "Upper Bound")
  main <- expand.cmat(main)
  out$main <- main

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
  cat("\nBounds on Average Treatment Effect:", "\n\n")
  print(x$main)
  groups <- x$groups
  if(!is.null(groups)){
    cat("\n")
    for(i in seq_along(groups)){
      cat("\nBounds on Group Average Treatment Effect:","Group", names(groups)[i], "\n\n")
      print(groups[[i]])
    }
  }
  cat("\nNote: DML estimates combined using the", x$combine.method, "method.")
}

#' @export
coef.dml.bounds <- function(object, combine.method = "median", ...){
  ate <- rbind(ate = sapply(object$coefs$main, function(x) x[combine.method, "estimate"]))
  if(!is.null(object$coef$groups)){
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "estimate"]))
    gate <- do.call("rbind", gate)
    rownames(gate)<- paste0("gate.", rownames(gate))
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
  ate <- rbind(ate = sapply(object$coefs$main, function(x) x[combine.method, "se"]))
  if(!is.null(object$coef$groups)){
    gate <- lapply(object$coefs$groups, function(x) sapply(x, function(x) x[combine.method, "se"]))
    gate <- do.call("rbind", gate)
    rownames(gate)<- paste0("gate.", rownames(gate))
  } else{
    gate = NULL
  }
  t(rbind(ate = ate, gate = gate))
}


#' @export
confint.dml.bounds <- function(object, parm = NULL, level = 0.95, combine.method = "median", ...){
  cf  <- t(coef(object, combine.method = combine.method))
  ses <- t(se(object, combine.method = combine.method))
  loop <- setNames(rownames(cf), rownames(cf))
  out <- lapply(loop , function(x)calc_confint(cf =cf[x,], parm = parm, ses =ses[x,], level = level))
  if(length(out) ==1 ){
    out <- out[[1]]
  }
  out
}


#'@export
confidence_bounds <- function(object, ...){
  UseMethod("confidence_bounds")
}

#'@export
confidence_bounds.dml.bounds <- function(object,
                                         level = 0.95,
                                         combine.method = "median", ...){
  level2 = max(0, 1 - (1-level)*2)
  confs <- confint(object, level = level2, combine.method = combine.method)
  if(is.list(confs)){
    out <- t(sapply(confs, function(x) c(lwr = x["theta.m",1], upr = x["theta.p",2])))
  }else{
    out <- rbind(ate = c(lwr = confs["theta.m",1], upr = confs["theta.p",2]))
  }
  attr(out, "conf.levels") <- c(point = level, region = level2)
  class(out) <- "confidence.bounds"
  out
}


#'@export
print.confidence.bounds <- function(x, ...){
   print.table(x)
  cat("\nConfidence levels: point =",
      paste0(attributes(x)$conf.levels["point"]*100, "%;"),
      "region =",
      paste0(attributes(x)$conf.levels["region"]*100, "%."))
}
