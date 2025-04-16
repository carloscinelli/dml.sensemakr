##' Debiased Machine Learning
##'
##' Estimates a target parameter of interest, such as an average treatment effect (ATE), using Debiased Machine #Learning (DML).
##'
##' @param y \code{\link{numeric}} vector with the outcome.
##' @param d \code{\link{numeric}} vector with the treatment. If the treatment is binary, it needs to be encoded as as: zero = absence of treatment, one = presence of treatment.
##' @param x \code{\link{numeric}} vector or \code{\link{matrix}} with covariates. We suggest constructing \code{x} using \code{\link{model.matrix}}.
##' @param model specifies the model. Current available options are \code{plm} for a partially linear model, and \code{npm} for a fully non-parametric model.
##' @param target specifies the target causal quantity of interest. Current available option is \code{ate} (ATE - average treatment effect). Note that for the partially linear model with a continuous treatment the ATE also equals the average causal derivative (ACD). For the nonparametric model, the ATE is only available for binary treatments. Other options (eg., ACD for the nonparametric model, ATT) will be available soon.
##' @param groups a \code{\link{factor}} or \code{\link{numeric}} vector indicating group membership. Groups must be a deterministic function of \code{x}.
##' @param  cf.folds number of cross-fitting folds. Default is \code{2}.
##' @param cf.reps number of cross-fitting repetitions. Default is \code{1}.
##' @param ps.trim trims propensity scores lower than \code{ps.trim} and greater than \code{1-ps.trim}, in order to obtain more stable estimates. This is only relevant for the case of a binary treatment.
##' @param reg details of the machine learning method to be used for estimating the nuisance parameters (e.g, regression functions of the treatment and the outcome). Currently, this should be specified using the same arguments as \code{\link{caret}}'s \code{\link{train}} function. The default is random forest using \code{\link{ranger}}. The default method is fast and usually works well for many applications.
##' @param yreg same as \code{reg}, but specifies arguments for the outcome regression alone. Default is the same value of \code{reg}.
##' @param dreg same as \code{reg}, but specifies arguments for the treatment regression alone. Default is the same value of \code{reg}.
##' @param dirty.tuning should the tuning of the machine learning method happen within each cross-fit fold ("clean"), or using all the data ("dirty")? Default is dirty tuning (\code{dirty.tuning = T}). As long as the number of choices for the tuning parameters is not too big, dirty tuning is faster and should not affect the asymptotic guarantees of DML.
##' @param save.models should the fitted models of each iterated be saved? Default is \code{FALSE}. Note that setting this to true could end up using a lot of memory.
##' @param y.class when \code{y} is binary, should the outcome regression be treated as a classification problem? Default is \code{FALSE}. Note that for DML we need the class probabilities, and regression gives us that. If you change to classification, you need to make sure the method outputs class probabilities.
##' @param d.class when \code{d} is binary, should the outcome regression be treated as a classification problem? Default is \code{FALSE}. Note that for DML we need the class probabilities, and regression gives us that. If you change to classification, you need to make sure the method outputs class probabilities.
##' @param verbose if \code{TRUE} (default) prints steps of the fitting procedure.
##' @param warning should \code{caret}'s warnings be printed? Default is \code{FALSE}. Note \code{caret} has many inconsistent and unnecessary warnings.
##' @return
##' An object of class \code{dml} with the results of the DML procedure. The object is a \code{\link{list}} containing:
##' \describe{
##'  \item{\code{data}}{A \code{list} with the data used.}
##'  \item{\code{call}}{The original call used to fit the model.}
##'  \item{\code{info}}{A \code{list} with general information and arguments of the DML fitting procedure.}
##'  \item{\code{fits}}{A \code{list} with the the predictions of each repetition.}
##'  \item{\code{results}}{A \code{list} with the results (influence functions and estimates) for each repetition.}
##'  \item{\code{coefs}}{A \code{list} with the estimates and standard errors for each repetition.}
##' }
##' @references Chernozhukov, V., Cinelli, C., Newey, W., Sharma A., and Syrgkanis, V. (2021). "Long Story Short: Omitted Variable Bias in Causal Machine Learning."
##' @examples
##'# loads package
##'library(dml.sensemakr)
##'
##'## loads data
##'data("pension")
##'
##'# set the outcome
##'y <- pension$net_tfa  # net total financial assets
##'
##'# set the treatment
##'d <- pension$e401    # 401K eligibility
##'
##'# set the covariates (a matrix)
##'x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)
##'
##'## compute income quartiles for group ATE.
##'g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE),
##'          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)
##'# run DML (nonparametric model)
##'## 2 folds (change as needed)
##'## 1 repetition (change as needed)
##'dml.401k <- dml(y, d, x, model = "npm", groups = g1, cf.folds = 2, cf.reps = 1)
##'
##'# summary of results with median method (default)
##'summary(dml.401k, combine.method = "median")
##'
##'# coef median method (default)
##'coef(dml.401k, combine.method = "median")
##'
##'# se median method (default)
##'se(dml.401k, combine.method = "median")
##'
##'# confint median method
##'confint(dml.401k, combine.method = "median")
##'
##'## Sensitivity Analysis
##'
##'### Robustness Values
##'robustness_value(dml.401k, alpha = 0.05)
##'
##'### Confidence Bounds
##'confidence_bounds(dml.401k, cf.y = 0.03, cf.d = 0.04, level = 0.95)
##'
##'### Contour Plots
##'ovb_contour_plot(dml.401k, cf.y = 0.03, cf.d = 0.04,
##'                 bound.label = "Max Match (3x years)")
##'
##' @importFrom caret train
##' @import caret
##' @export
dml <- function(y, d, x,
                model  = c("plm", "npm"),
                target = "ate",
                groups = NULL,
                cf.folds = 5,
                cf.reps  = 1,
                ps.trim = 0.01,
                reg = "ranger",
                yreg = reg,
                dreg = reg,
                dirty.tuning = TRUE,
                save.models = FALSE,
                y.class = FALSE,
                d.class = FALSE,
                verbose = TRUE,
                warnings = FALSE){

  # check arguments
  model   <- match.arg(model)
  target  <- match.arg(target,
                       choices = c("ate", "att", "atu"),
                       several.ok = T)
  # check if x is numeric but not a matrix and converts to matrix.
  # this is for the case where x is a single covariate
  if (is.numeric(x) && !is.matrix(x)) {
    x <- as.matrix(x)
    colnames(x) <- "x"
  }

  # error if x is not a matrix
  if (!is.matrix(x)) stop("x must be a numeric vector or a matrix.")

  # if x doesn't have column names, include column names.
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("x", 1:ncol(x))
  }

  if (!is.null(groups)) {
    groups  <- as.factor(groups)
  }

  # check treatment condition for npm.
  if (model == "npm" & any(target %in% c("ate","att", "atu"))) {
    d.value <- unique(d)
    binary <- all(d.value %in% c(0,1))
    if (!binary) stop("Treatment must be binary for ATE/ATT/ATU in nonparametric model.")
  }

  if (cf.folds < 2) {
    cf.folds <- 2
    warning("cf.folds set to 2 (number of cross-fitting folds need to be at least 2).")
  }


  out <- list()
  out$data <- list(y = y, d = d, x = x)
  out$call <-   match.call()

  if (y.class) {
    y <- factor(y, levels = c(0,1), labels = c("zero", "one"))
  }

  if (d.class) {
    d <- factor(d, levels = c(0,1), labels = c("zero", "one"))
  }


  yreg <- caretArgs(yreg)
  dreg <- caretArgs(dreg)

  out$info <- list(model = model,
                   target = target,
                   cf.folds = cf.folds,
                   cf.reps = cf.reps,
                   dirty.tuning = dirty.tuning,
                   yreg = yreg,
                   dreg = dreg)

  if (verbose) {
    cat("Debiased Machine Learning\n")
    cat("\n")
    cat("", "Model:", ifelse(out$info$model == "plm", "Partially Linear", "Nonparametric"), "\n")
    cat("", "Target:", out$info$target , "\n")
    cat("", "Cross-Fitting:", out$info$cf.folds, "folds,", out$info$cf.reps, "reps", "\n")
    cat("", "ML Method:",
        "outcome", paste0("(", attr(out$info$yreg$method, "name"), "),"),
        "treatment", paste0("(", attr(out$info$dreg$method,"name"), ")\n"))
    cat("", "Tuning:", ifelse(out$info$dirty.tuning, "dirty", "clean"), "\n")
    cat("\n")
  }



  if (dirty.tuning) {

    if (verbose) {
      cat("\n")
      cat("====================================\n")
      cat("Tuning parameters using all the data\n")
      cat("====================================\n\n")
    }

    if (is.numeric(y)) {
      ytil <- y
      muy <- min(ytil)
      sdy <- max(ytil) - min(ytil)
      ytil <- (ytil - muy)/sdy
    } else {
      ytil <- y
      muy <- 0
      sdy <- 1
    }

    if (is.numeric(d)) {
      dtil <- d
      mud <- min(dtil)
      sdd <- max(dtil) - min(dtil)
      dtil <- (dtil - mud)/sdd
    } else {
      dtil <- d
      mud <- 0
      sdd <- 1
    }

    if (verbose) cat("- Tuning Model for D.\n")

      dreg  <- tune_model(x =  x, y = dtil, args = dreg)
      if (verbose) {
        cat("-- Best Tune:\n")
        print(dreg$tuneGrid)
        cat("\n")
      }



    if (model == "plm") {
      if (verbose) cat("- Tuning Model for Y (partially linear).\n")
      yreg  <- tune_model(x =  x, y = ytil, args = yreg)
      if (verbose) {
        cat("-- Best Tune:\n")
        print(yreg$tuneGrid)
        cat("\n")
      }

    }

    if (model == "npm") {
      if (verbose) cat("- Tuning Model for Y (non-parametric).\n")
      dx = cbind(d, x)
      yreg <- tune_model(x = dx, y = ytil, args = yreg)
      if (verbose) cat("-- Best Tune:\n")
      print(yreg$tuneGrid)
      cat("\n")
    }
      out$dreg <- dreg
      out$yreg <- yreg

  }

  fits <- results <-  list()

  if (verbose) {
    cat("\n")
    cat("======================================\n")
    cat("Repeating", paste0(cf.folds,"-fold"), "cross-fitting", cf.reps, "times\n")
    cat("======================================\n\n")
  }
  for (i in 1:cf.reps) {
    if (verbose) cat("-- Rep", i)
    cross.fit.i    <- cross.fitting(y            = y,
                                    d            = d,
                                    x            = x,
                                    d1           = ifelse(d.class, "one", 1),
                                    d0           = ifelse(d.class, "zero", 0),
                                    model        = model,
                                    cf.folds     = cf.folds,
                                    yreg         = yreg,
                                    dreg         = dreg,
                                    verbose      = verbose,
                                    warnings     = warnings,
                                    save.models  = save.models)

    fits[[i]] <- cross.fit.i


    if (model == "plm") {
      dhat   <- cross.fit.i$preds$dhat
      yhat <- cross.fit.i$preds$yhat
      results[[i]] <- ate.plm(num(y),
                              num(d),
                              yhat,
                              dhat)
    }

    if (model == "npm") {
      dhat   <- cross.fit.i$preds$dhat
      yhat0  <- cross.fit.i$preds$yhat0
      yhat1  <- cross.fit.i$preds$yhat1
      results[[i]] <- ate.npm(num(y),
                              num(d),
                              parameter = "all",
                              yhat1, yhat0, dhat, trim = ps.trim)
    }
    if (verbose) cat("\n")
  }

  out$fits <- fits
  out$results$main$all <- results
  out$coefs$main$all   <- combine.cross.fits(results)

  if (model == "npm") {
    out$results$main <- ate.att.atu.npm(out, target = target, trim = ps.trim)
    out$coefs$main <- lapply(out$results$main, combine.cross.fits)
  }

  # Group ATE
  if (!is.null(groups)) {
    groups  <- as.factor(groups)

    if (model == "npm") {
      out$results$groups <- group.ate.npm(out, groups)
      out$coefs$groups   <- lapply(out$results$groups, combine.cross.fits)
    }

    # deactivate group PLM for now
    if (model == "plm") {
      out$results$groups <- group.ate.plm(out, groups)
      out$coefs$groups   <- lapply(out$results$groups, combine.cross.fits)
    }

  }

  class(out) <- "dml"

  return(out)

}

##' @description The function \code{dml.gate} is a convenience function that adds groups to a \code{dml} object after the model is fit.
##'
##' @rdname dml
##' @export
dml_gate <- function(dml.fit, groups,...){
  call2 <- match.call()
  groups  <- as.factor(groups)
  model    <- dml.fit$info$model
  dml.fit$call$groups <- call2$groups
  gate.fun <- switch(model,
                     plm = group.ate.plm,
                     npm = group.ate.npm)
  dml.fit$results$groups <- gate.fun(dml.fit, groups, ...)
  dml.fit$coefs$groups   <- lapply(dml.fit$results$groups,
                               combine.cross.fits)
  return(dml.fit)
}

num <- function(v){
  if (is.factor(v)) {
    return(ifelse(v == "zero", 0, 1))
  } else {
    return(v)
  }
}

# function to tune the model outside cross-fitting
# returns the best tune
tune_model <- function(x, y, args) {
  original.args  <- c(list(x = x, y = y), args)
  best.model     <- silent.do.call(what = "train", args = original.args)
  args$trControl <- trainControl(method = "none", classProbs = T)
  args$tuneGrid  <- best.model$bestTune
  return(args)
}

# extracts estimates from dml.fit
extract_estimate <- function(results, param) sapply(results, function(x) x$estimates[[param]])

# r2
r2 <- function(pred, obs){
  max(1 - var(obs - pred) / var(obs), 0)
}

# function to get the goodness of fit metric used by caret
get_metric <- function(fits, obs, metric = "metric.y"){
  metric       <- unique(sapply(fits, function(x) x[[metric]]))
  pred         <- ifelse(metric == "metric.y", "yhat", "dhat")
  metric.func  <- get(metric)
  metric.value <- mean(sapply(fits, function(x) metric.func(obs, pred = x$preds[[pred]])))
  names(metric.value) <- metric
  metric.value
}

# function to combine the results of repeated DML estimates
combine.cross.fits <- function(results, param = "theta.s"){

  # extract estimates and se's
  thetas <- sapply(results, function(x) x$estimates[[param]])
  ses    <- sapply(results, function(x) x$estimates[[paste0("se.", param)]])

  out <- rbind(mean =   combine.mean(thetas, ses),
               median = combine.median(thetas, ses))
  out
}

combine.median <- function(thetas, ses){
  median.theta  <- median(thetas)
  ss <- c((thetas - mean(thetas)) %*% (thetas - mean(thetas)))
  se.median.theta <- sqrt(median(ses^2 + c(ss)))
  c(estimate = median.theta, se = se.median.theta)
}

combine.mean <- function(thetas, ses){
  mean.theta    <- mean(thetas)
  ss <- c((thetas - mean(thetas)) %*% (thetas - mean(thetas)))
  se.mean.theta   <- sqrt(mean(ses^2) + ss/length(ses))
  c(estimate = mean.theta,   se = se.mean.theta)
}

caretArgs <- function(reg){

  if (is.character(reg)) {
    reg_name <- reg
    reg <- list(method = reg)
  }

  if (is.character(reg$method)) {
    reg_name <- reg$method
  }

  if (!is.null(models[[reg$method]])) {
    reg$method <- models[[reg$method]]
  } else {
    reg$method <- getModelInfo(reg$method)[[reg$method]]
  }

  if (is.null(reg$trControl)) {
    reg$trControl <- trainControl(method = "none")
  } else {
    reg$trControl <- do.call("trainControl", reg$trControl)
  }
  if (is.null(reg$preProcess)) {
    reg$preProcess <- "range"
  }
  attr(reg$method, "name") <- reg_name
  reg$trControl$classProbs <- T
  return(reg)
}

