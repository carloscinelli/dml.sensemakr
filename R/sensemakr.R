##' Sensitivity Analysis for Causal Machine Learning
##' @description
##' This function performs sensitivity analysis of causal effect estimates as discussed in Chernozhukov et al (2026).
##' The main input is an object of class \code{\link{dml}}. It returns an object of class \code{dml.sensemakr} with several pre-computed sensitivity statistics for reporting. After running \code{sensemakr} you may directly use the \code{plot}, \code{print} and \code{summary} methods in the returned object.
##'
##' @returns An object of class \code{dml.sensemakr}, containing sensitivity analysis results.
##'
##' @export
sensemakr <- function(model, ...) {
  UseMethod("sensemakr")
}

##' @param model a model created with the function \code{\link{dml}}.
##' @param rho2 degree of adversity. Default is \code{rho2 = 1}, which assumes the maximum degree of adversity of confounding.
##' @param ... arguments passed to other methods.
##' @param benchmark_covariates  character vector of the names of covariates that will be used to bound the plausible strength of the latent variables.
##' @param cf.y (optional) R2 based strength of confounding in the outcome regression. It corresponds to the parameter R^2_\{y-g_s ~ g-g_s\} in Chernozhukov et al (2026). Generally, it is equal by the (nonparametric) partial R2 of the confounders with the outcome. Default is NULL.
##' @param cf.d (optional) R2 based strength of confounding in the Riesz representer (RR). It corresponds to the parameter 1-R^2_\{alpha ~ alpha_s\} in Chernozhukov et al (2026). It quantifies how much variation latent variables create in the RR. This interpretation can be refined for specific cases. For instance, if the target is the ATE in a partially linear model, this quantity reduces to the (nonparametric) partial R2 of the confounders with the treatment. If the target is the ATE in a nonparametric model with a binary treatment, this quantity reduces to the gains in precision in the treatment model due to latent variables.
##' @param kD numeric vector of multipliers for the benchmark gains on the treatment
##'   side: each value k adds one bounds row per benchmark covariate, postulating a
##'   latent variable whose gain in the Riesz Representer is k times the covariate's
##'   observed gain. Default \code{1}.
##' @param kY same as \code{kD}, for the outcome side. Default is \code{kD}.
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
##' # contour plots
##' plot(sens.401k)
##'
##' @method sensemakr dml
##' @export
##' @rdname sensemakr
sensemakr.dml <- function(model,
                          benchmark_covariates = NULL,
                          cf.y = NULL, cf.d = cf.y,
                          rho2 = 1,
                          kD = 1, kY = kD,
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

  # bounds on omitted variable bias, in two tables: the postulated scenario
  # (cf.y/cf.d/rho2, one row per target, groups included), and the benchmark
  # scenarios (one row per multiplier per covariate, labelled "<k>x <name>",
  # carrying both the fixed-benchmark and benchmark-uncertainty CIs).
  rows <- NULL

  if (!is.null(cf.y)) {
    conf.bounds <- confidence_bounds(model, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
    out$conf.bounds <- conf.bounds       # kept for backward compatibility
    pt <- dml_bounds(model, cf.y = cf.y, cf.d = cf.d, rho2 = rho2)
    cf <- coef(pt)
    tg <- rownames(conf.bounds)
    rows <- data.frame(target = tg,
                       cf.y = cf.y, cf.d = cf.d, rho2 = rho2,
                       theta.minus = unname(cf["theta.m", tg]),
                       theta.plus  = unname(cf["theta.p", tg]),
                       lwr = unname(conf.bounds[tg, "lwr"]),
                       upr = unname(conf.bounds[tg, "upr"]),
                       row.names = NULL)
  }

  if (!is.null(benchmark_covariates)) {
    bench.slot <- unname(.target_to_slot[model$info$target])
    single <- length(bench.slot) == 1L && !is.na(bench.slot) &&
              !is.null(model$results$main[[bench.slot]])
    if (!single) {
      warning("Benchmarks apply to one target at a time, and this fit has ",
              length(model$info$target), " (",
              paste(model$info$target, collapse = ", "),
              "). The bounds table reports only the manual scenario.",
              call. = FALSE)
    } else {
      bench <- dml_benchmark(model = model,
                             benchmark_covariates = benchmark_covariates)
      out$bench.bounds <- bench
      gains <- summary(bench)$benchmarks
      kD.v  <- kD
      kY.v  <- rep_len(kY, length(kD.v))
      bt    <- NULL
      for (i in seq_along(kD.v)) {
        bb <- as.data.frame(benchmark_bounds(model, bench,
                                             kY = kY.v[i], kD = kD.v[i]))
        bt <- rbind(bt, data.frame(
          target = model$info$target,
          bound.label = if (kY.v[i] == kD.v[i]) {
            paste0(kD.v[i], "x ", rownames(gains))
          } else {
            paste0(kY.v[i], "xY/", kD.v[i], "xD ", rownames(gains))
          },
          cf.y = unname(kY.v[i] * gains[, "gain.Y"]),
          cf.d = unname(kD.v[i] * gains[, "gain.D"]),
          rho  = unname(gains[, "rho"]),
          theta.minus = bb$theta.minus, theta.plus = bb$theta.plus,
          lwr.fixed = bb$lwr.fixed, upr.fixed = bb$upr.fixed,
          lwr = bb$lwr, upr = bb$upr, row.names = NULL))
      }
      class(bt) <- c("dml_bench_bounds", "data.frame")
      out$bench.table <- bt
    }
  }

  if (!is.null(rows)) {
    class(rows) <- c("dml_ovb_bounds", "data.frame")
    out$bounds <- rows
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
##' @returns For \code{print}: the object, invisibly. For \code{summary}: the summary is printed to the console.
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

  if (!is.null(x$bounds)) {
    cat("\nBounds on omitted variable bias (postulated scenario):\n\n")
    .print_ovb_bounds(x$bounds, digits)
    cat("\n")
  }

  if (!is.null(x$bench.table)) {
    cat("\nBenchmark bounds (latent variable k times as strong as the covariate):\n\n")
    .print_ovb_bounds(x$bench.table, digits)
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
    cat(paste0("\n-- Robustness Value for the Confidence Bound (RVa): omitted variables that explain more than RV% of the residual variation of the outcome (cf.y) and generate an additional RV% of variation on the Riesz Representer (cf.d) are sufficiently strong to make the confidence bounds include ", object$info$theta, ", at the  significance level of alpha = " , object$info$alpha, ". Conversely, omitted variables that do not explain more than RV% of the residual variation of the outcome nor generate an additional RV% of variation on the Riesz Representer are not sufficiently strong to do so. \n"))
    cat("\n The interpretation of sensitivity parameters can be further refined for each target quantity. See more below.\n")
  }


  if (!is.null(object$bounds)) {
    cat("\nBounds on omitted variable bias (postulated scenario):\n\n")
    .print_ovb_bounds(object$bounds, digits)
    cat("\n-- Bounds on each target parameter under latent variables with the postulated sensitivity parameters cf.y, cf.d and rho2. theta.minus and theta.plus are the point bounds; lwr and upr are one-sided 95% confidence bounds treating those parameters as fixed.\n")
  }

  if (!is.null(object$bench.table)) {
    cat("\nBenchmark bounds (latent variable k times as strong as the covariate):\n\n")
    .print_ovb_bounds(object$bench.table, digits)
    cat("\n-- Each row postulates a latent variable whose gains in explanatory power are k times the observed gains of the benchmark covariate; rho is the covariate's estimated alignment. [lwr.fixed, upr.fixed] are confidence bounds treating the benchmark as fixed; [lwr, upr] account for benchmark uncertainty.\n")
  }

  if (object$model$info$model == "npm") {
  cat("\n\nInterpretation of sensitivity parameters:\n")
  cat(paste0("\n-- cf.y: percentage of the residual variation of the outcome explained by latent variables."))
  cat(paste0("\n-- cf.d: percentage gains in the variation of the Riesz Representer generated by latent variables:\n"))
  is_cond_model <- isTRUE(object$model$info$conditional)
  if (!is.null(object$model$results$main$all) && !is_cond_model)
    cat("   ATE: cf.d measures the percentage gains in the average precision on the treatment regression.\n")
  if (!is.null(object$model$results$main$treat))
    cat("   ATT", if (is_cond_model) "(conditional)" else "(unconditional)",
        ": cf.d measures the percentage gains in the average odds of getting treatment. \n")
  if (!is.null(object$model$results$main$untr))
    cat("   ATU", if (is_cond_model) "(conditional)" else "(unconditional)",
        ": cf.d measures the percentage gains in the average odds of not getting treatment. \n")
  if (!is.null(object$model$results$groups))
    cat("-- For Group Average Treatment Effects (GATE), parameters are conditional on the relevant group.")
  }
}

##' Sensitivity analysis plots for dml.sensemakr
##'
##' This function provides the contour plots of the sensitivity analysis results obtained with the function \code{\link{sensemakr}} for DML. It is basically a dispatcher to the core plot function \code{\link{ovb_contour_plot}}.
##'
##' @param x an object of class \code{dml.sensemakr} created with the \code{\link{sensemakr}} function.
##' @param parameter the target parameter to plot. Options are \code{"ate"}, \code{"att"}, and \code{"atu"}.
##' @inheritParams ovb_contour_plot
##' @returns No return value, called for side effects (plotting).
##' @export
plot.dml.sensemakr <- function(x,
                               parameter = c("ate", "att", "atu"),
                               which.bound = c("lwr", "upr"),
                               level = 0.95,
                               combine.method = "median",
                               ...){
  # default to the fit's own (single) target, mirroring ovb_contour_plot.dml
  if (missing(parameter) && length(x$model$info$target) >= 1L &&
      x$model$info$target[1] %in% c("ate", "att", "atu")) {
    parameter <- x$model$info$target[1]
  }
  if (!"bound.label" %in% names(list(...))) {
    ovb_contour_plot(x$model,
                     parameter = parameter,
                     which.bound = which.bound,
                     level = level,
                     rho2 = x$info$rho2,
                     cf.y = x$info$cf.y,
                     cf.d = x$info$cf.d,
                     bound.label = x$info$bound.label,
                     combine.method = combine.method, ...)
  } else {
    ovb_contour_plot(x$model,
                     parameter = parameter,
                     which.bound = which.bound,
                     level = level,
                     rho2 = x$info$rho2,
                     cf.y = x$info$cf.y,
                     cf.d = x$info$cf.d,
                     combine.method = combine.method, ...)
  }

}


# Format the bounds-on-OVB table: round numerics, drop the target column when
# the fit has a single target, print without row names.
.print_ovb_bounds <- function(b, digits = 4) {
  tb <- as.data.frame(b)
  num <- vapply(tb, is.numeric, logical(1))
  tb[num] <- lapply(tb[num], round, digits = digits)
  print(tb, row.names = FALSE)
}
