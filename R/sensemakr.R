##' @export
sensemakr <- sensemakr::sensemakr


##'@exportS3Method sensemakr::sensemakr dml
##'@exportS3Method dml.sensemakr::sensemakr dml
sensemakr.dml <- function(model,
                          benchmark_covariates = NULL,
                          cf.y = 0.03, cf.d = cf.y,
                          rho2 = 1,
                          bound_label = "Confounding Scenario",
                          theta = 0, alpha = 0.05){

  out <- list()

  out$info <- list(cf.y = cf.y,
                   cf.d = cf.d,
                   rho2 = rho2,
                   theta = theta,
                   alpha = alpha)

  # original model
  out$model <- model

  # robustness values
  rv   <- robustness_value(model, theta = theta, alpha = 1)
  rva  <- robustness_value(model, theta = theta, alpha = alpha)
  rvs <- cbind(rv, rva)
  out$sensitivity_stats <- rvs

  # confidence bounds
  if (!is.null(cf.y)) {
    conf.bounds <- confidence_bounds(model, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
    out$conf.bounds <- conf.bounds
  }

  # benchmarks
  if (!is.null(benchmark_covariates) & !is.null(model$results$main$all)) {
    bench.bounds <- dml_benchmark(model = model, benchmark_covariates = benchmark_covariates)
    out$bench.bounds <- bench.bounds
  }

  class(out) <- "dml.sensemakr"

  return(out)

}

##' @export
print.dml.sensemakr <- function(x,
                                digits = 2,
                                ...) {

  cat("==== Original Analysis ====\n")
  print(x$model)

  cat("==== Sensitivity Analysis ====\n\n")
  cat("Null hypothesis:", "theta =", x$info$theta,"\n")
  cat("Signif. level:", "alpha =", x$info$alpha,"\n\n")
  cat("Robustness Values:\n")
  rvs <- round(x$sensitivity_stats*100, digits = digits)
  colnames(rvs) <- c("RV (%)", "RVa (%)")
  print(rvs)

  if (!is.null(x$conf.bounds)) {
    cat("\nConfidence Bounds for Sensitivity Scenario:\n")
    print(x$conf.bounds)
    cat("\n")
  }

  cat("For more information, check summary.")
}

##' @export
summary.dml.sensemakr <- function(object,  digits = max(3L, getOption("digits") - 3L), ...) {
  cat("==== Original Analysis ====\n")
  print(summary(object$model), digits = digits, ...)
  cat("\n\n")

  cat("==== Sensitivity Analysis ====\n\n")

  cat("Null hypothesis:", "theta =", object$info$theta,"\n")
  cat("Signif. level:", "alpha =", object$info$alpha,"\n\n")

  cat("Robustness Values:\n")
  rvs <- round(object$sensitivity_stats*100, digits = digits)
  colnames(rvs) <- c("RV (%)", "RVa (%)")
  print(rvs)
  cat("\nVerbal interpretation of robustness values:\n")
  if (object$model$info$model == "plm"){
    cat(paste0("\n-- Robustness Value for the Bound (RV): omitted variables that explain more than RV% of the residual variation both of the treatment (cf.d) and of the outcome (cf.y) are sufficiently strong to make the estimated bounds include ", object$info$theta, ". Conversely, omitted variables that do not explain more than RV% of the residual variation of both the treatment and the outcome are not sufficiently strong to do so.\n"))
    cat(paste0("\n-- Robustness Value for the Confidence Bound (RVa): omitted variables that explain more than RV% of the residual variation both of the treatment (cf.d) and of the outcome (cf.y) are sufficiently strong to make the confidence bounds include ", object$info$theta, ", at the  significance level of alpha = " , object$info$alpha, ". Conversely, omitted variables that do not explain more than RV% of the residual variation of both the treatment and the outcome are not sufficiently strong to do so.\n"))
  }

  if (object$model$info$model == "npm"){
    cat(paste0("\n-- Robustness Value for the Bound (RV): omitted variables that explain more than RV% of the residual variation of the outcome (cf.y) and generate an additional RV% of variation on the Riesz Representer (cf.d) are sufficiently strong to make the estimated bounds include ", object$info$theta, ". Conversely, omitted variables that do not explain more than RV% of the residual variation of the outcome nor generate an additional RV% of variation on the Riesz Representer are not sufficiently strong to do so.\n"))
    cat(paste0("\n-- Robustness Value for the Confidence Bound (RVa): omitted variables that explain more than RV% of the residual variation of the the outcome (cf.y) and generate an additional RV% of variation on the Riesz Representer (cf.d) are sufficiently strong to make the confidence bounds include ", object$info$theta, ", at the  significance level of alpha = " , object$info$alpha, ". Conversely, omitted variables that do not explain more than RV% of the residual variation of the outcome nor generate an additional RV% of variation on the Riesz Representer are not sufficiently strong to do so. \n"))
    cat("\n The interpretation of sensitivity parameters can be further refined for each target quantity. See more below.\n")
  }


  if (!is.null(object$conf.bounds)) {
    cat("\nConfidence Bounds for Sensitivity Scenario:\n")
    print(round(object$conf.bounds, digits = digits))
    cat("\nVerbal interpretation of confidence bounds:\n\n")
    cat("-- The table shows the lower (lwr) and upper (upr) limits of the confidence bounds on the target quantity, considering omitted variables with postulated sensitivity parameters cf.y, cf.d and rho2. The confidence level \"point\" is the relevant coverage for most use cases, and stands for the coverage rate for the true target quantity. The confidence level \"region\" stands for the coverage rate of the true bounds.")
  }
  if (object$model$info$model == "npm") {
  cat("\n\nInterpretation of sensitivity parameters:\n")
  cat(paste0("\n-- cf.y: percentage of the residual variation of the outcome explained by latent variables."))
  cat(paste0("\n-- cf.d: percentage gains in the variation of the Riesz Representer generated by latent variables:\n"))
  if (!is.null(object$model$results$main$all))
    cat("   ATE: cf.d measures the percentage gains in the average precision on the treatment regression.\n")
  if (!is.null(object$model$results$main$treat))
    cat("   ATT: cf.d measures the percentage gains in the average odds of getting treatment. \n")
  if (!is.null(object$model$results$main$untr))
    cat("   ATU: cf.d measures the percentage gains in the average odds of not getting treatment. \n")
  if (!is.null(object$model$results$groups))
    cat("-- For Group Average Treatment Effects (GATE), parameters are conditional on the relevant group.")
  }
}

##' @export
plot.dml.sensemakr <- function(model,
                               parameter = c("ate", "att", "atu"),
                               which.bound = c("lwr", "upr"),
                               level = 0.95,
                               combine.method = "median",
                               ...){
  ovb_contour_plot(model$model,
                   parameter = parameter,
                   which.bound = which.bound,
                   level = level,
                   rho2 = model$info$rho2,
                   cf.y = model$info$cf.y,
                   cf.d = model$info$cf.d,
                   combine.method = combine.method, ...)
}
