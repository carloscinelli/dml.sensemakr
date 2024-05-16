##' Sensitivity Analysis for Causal Machine Learning
##' @description
##' This function performs sensitivity analysis of causal effect estimates as discussed in Chernozhukov et al (2023).
##' The main input is an object of class \code{\link{dml}}. It returns an object of class \code{dml.sensemakr} with several pre-computed sensitivity statistics for reporting. After running \code{sensemakr} you may directly use the \code{plot}, \code{print} and \code{summary} methods in the returned object.
##'
##' @returns An object of class \code{dml.sensemakr}, containing sensitivity analysis results.
##'
##' @export
sensemakr <- sensemakr::sensemakr

##' @param model a model created with the function \code{\link{dml}}.
##' @param benchmark_covariates  character vector of the names of covariates that will be used to bound the plausible strength of the latent variables.
##' @param cf.y (optional) R2 based strength of confounding in the outcome regression. It corresponds to the parameter R^2_\{y-g_s ~ g-g_s\} in Chernozhukov et al (2023). Generally, it is equal by the (nonparametric) partial R2 of the confounders with the outcome. Default is NULL.
##' @param cf.d (optional) R2 based strength of confounding in the Riesz representer (RR). It corresponds to the parameter 1-R^2_\{alpha ~ alpha_s\} in Chernozhukov et al (2023). It quantifies how much variation latent variables create in the RR. This interpretation can be refined for specific cases. For instance, if the target is the ATE in a partially linear model, this quantity reduces to the (nonparametric) partial R2 of the confounders with the treatment. If the target is the ATE in a nonparametric model with a binary treatment, this quantity reduces to the gains in precision in the treatment model due to latent variables.
##' @param bound_label label to bounds provided manually in \code{cf.y} and \code{cf.d}.
##' @param theta null hypothesis.
##' @param alpha significance level.
##'
##' @examples
##' # loads package
##' library(dml.sensemakr)
##'
##' # loads data
##' data("pension")
##'
##' # set treatment, outcome and covariates
##' y <- pension$net_tfa  # net total financial assets
##' d <- pension$e401     # 401K eligibility
##' x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)
##'
##' # run DML (nonparametric model)
##' dml.401k <- dml(y, d, x, model = "npm")
##'
##' # sensitivity analysis
##' sens.401k <- sensemakr(dml.401k, cf.y = 0.04, cf.d = 0.03)
##'
##' # summary
##' summary(sens.401k)
##'
##' # contout plots
##' plot(sens.401k)
##'
##'@exportS3Method sensemakr::sensemakr dml
##'@exportS3Method dml.sensemakr::sensemakr dml
##' @rdname sensemakr
sensemakr.dml <- function(model,
                          benchmark_covariates = NULL,
                          cf.y = NULL, cf.d = cf.y,
                          rho2 = 1,
                          bound_label = "Confounding Scenario",
                          theta = 0, alpha = 0.05, ...){

  out <- list()

  out$info <- list(cf.y = cf.y,
                   cf.d = cf.d,
                   rho2 = rho2,
                   bound.label = bound_label,
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

##' Sensitivity analysis print and summary methods for \code{dml.sensemakr}
##'
##' @description
##' The \code{print} and \code{summary} methods provide verbal descriptions of the sensitivity analysis results
##' obtained with the function \code{\link{sensemakr}}.
##'
##' @param ... arguments passed to other methods.
##' @param object an object of class \code{\link{sensemakr}}.
##' @param x an object of class \code{\link{sensemakr}}.
##' @param digits minimal number of \emph{significant} digits.
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
##' @rdname print.dml.sensemakr
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

##' Sensitivity analysis plots for dml.sensemakr
##'
##' This function provides the contour plots of the sensitivity analysis results obtained with the function \code{\link{sensemakr}} for IV. It is basically a dispatcher to the core plot function \code{\link{ovb_contour_plot}}.
##'
##' @param x an object of class \code{dml.sensemakr} created with the \code{\link{sensemakr}} function.
##' @inheritParams ovb_contour_plot
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
                   bound.label = model$info$bound.label,
                   combine.method = combine.method, ...)
}
