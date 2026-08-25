##' Debiased Machine Learning
##'
##' Estimates a target parameter of interest, such as an average treatment effect (ATE), using Debiased Machine #Learning (DML).
##'
##' @param y \code{\link{numeric}} vector with the outcome.
##' @param d \code{\link{numeric}} vector with the treatment. If the treatment is binary, it needs to be encoded as as: zero = absence of treatment, one = presence of treatment.
##' @param x \code{\link{numeric}} vector or \code{\link{matrix}} with covariates. We suggest constructing \code{x} using \code{\link{model.matrix}}.
##' @param model specifies the model. Current available options are \code{plm} for a partially linear model, and \code{npm} for a fully non-parametric model.
##' @param target specifies the target causal quantity of interest. Available options are \code{ate} (ATE - average treatment effect), \code{att} (ATT - average treatment effect on the treated), and \code{atu} (ATU - average treatment effect on the untreated). Note that for the partially linear model with a continuous treatment the ATE also equals the average causal derivative (ACD). For the nonparametric model, these are only available for binary treatments.
##' @param groups a \code{\link{factor}} or \code{\link{numeric}} vector indicating group membership. Groups must be a deterministic function of \code{x}.
##' @param cf.folds number of cross-fitting folds. Default is \code{5}.
##' @param cf.reps number of cross-fitting repetitions. Default is \code{1}.
##' @param cf.seed optional integer. A random seed for reproducibility of fold assignments.
##' @param ps.trim trims propensity scores lower than \code{ps.trim} and greater than \code{1-ps.trim}, in order to obtain more stable estimates. Alternatively, a named list with elements \code{lower} and \code{upper} specifying the lower and upper bounds for trimming. This is only relevant for the case of a binary treatment and when \code{model = "npm"}.
##' @param reg details of the machine learning method to be used for estimating the nuisance parameters (e.g, regression functions of the treatment and the outcome). Currently, this should be specified using the same arguments as \code{caret}'s \code{\link[caret]{train}} function. The default is random forest using \code{ranger}. The default method is fast and usually works well for many applications.
##' @param yreg same as \code{reg}, but specifies arguments for the outcome regression alone. Default is the same value of \code{reg}. Alternatively, a named list with elements \code{yreg0} and \code{yreg1} specifying separate methods for each.
##' @param dreg same as \code{reg}, but specifies arguments for the treatment regression alone. Default is the same value of \code{reg}.
##' @param dirty.tuning should the tuning of the machine learning method happen within each cross-fit fold ("clean"), or using all the data ("dirty")? Default is dirty tuning (\code{dirty.tuning = TRUE}). As long as the number of choices for the tuning parameters is not too big, dirty tuning is faster and should not affect the asymptotic guarantees of DML.
##' @param save.models should the fitted models of each iterated be saved? Default is \code{FALSE}. Note that setting this to true could end up using a lot of memory.
##' @param y.class when \code{y} is binary, should the outcome regression be treated as a classification problem? Default is \code{FALSE}. Note that for DML we need the class probabilities, and regression gives us that. If you change to classification, you need to make sure the method outputs class probabilities.
##' @param d.class when \code{d} is binary, should the outcome regression be treated as a classification problem? Default is \code{FALSE}. Note that for DML we need the class probabilities, and regression gives us that. If you change to classification, you need to make sure the method outputs class probabilities.
##' @param conditional logical or \code{NULL}. If \code{TRUE}, estimates the \emph{conditional} version of the target, in which the outcome regression is fit on a single treatment arm and used to impute the counterfactual mean for the other arm. Only supported for \code{model = "npm"} with a single \code{target} of \code{"att"} or \code{"atu"}. When \code{NULL} (default), it is set to \code{TRUE} automatically for those single-target npm cases and \code{FALSE} otherwise. The conditional parameterization is the recommended one (Wang et al., 2026); \code{conditional = FALSE} gives the unconditional parameterization of Chernozhukov et al. (2026), retained so those results can be reproduced.
##' @param centered logical. Controls the parameterization of the Riesz-representer imbalance \code{nu2.s} in the conditional model. When \code{FALSE} (default), \code{nu2.s} is the full second moment of the Riesz representer, \eqn{\chi^2 + 1} (uncentered); when \code{TRUE}, it is the centered version, the \eqn{\chi^2} divergence (that moment minus 1). Only relevant when \code{conditional = TRUE}. The uncentered parameterization is the recommended one (Wang et al., 2026); the centered version corresponds to the parameterization of Huang and Pimentel (2025), retained so those results can be reproduced. Note the centered version can yield negative estimates of \code{nu2.s} in finite samples.
##' @param verbose if \code{TRUE} (default) prints steps of the fitting procedure.
##' @param warnings should \code{caret}'s warnings be printed? Default is \code{FALSE}. Note \code{caret} has many inconsistent and unnecessary warnings.
##' @returns
##' An object of class \code{dml} with the results of the DML procedure. The object is a \code{\link{list}} containing:
##' \describe{
##'  \item{\code{data}}{A \code{list} with the data used.}
##'  \item{\code{call}}{The original call used to fit the model.}
##'  \item{\code{info}}{A \code{list} with general information and arguments of the DML fitting procedure.}
##'  \item{\code{fits}}{A \code{list} with the predictions of each repetition.}
##'  \item{\code{results}}{A \code{list} with the results (influence functions and estimates) for each repetition.}
##'  \item{\code{coefs}}{A \code{list} with the estimates and standard errors for each repetition.}
##' }
##' @references Chernozhukov, V., Cinelli, C., Newey, W., Sharma A., and Syrgkanis, V. (2026). "Long Story Short: Omitted Variable Bias in Causal Machine Learning." \emph{Review of Economics and Statistics}. \doi{10.1162/REST.a.1705}
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
##'          labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE)
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
                cf.seed = NULL,
                ps.trim = 0.01,
                reg = "ranger",
                yreg = reg,
                dreg = reg,
                dirty.tuning = TRUE,
                save.models = FALSE,
                y.class = FALSE,
                d.class = FALSE,
                conditional = NULL,
                centered = FALSE,
                verbose = TRUE,
                warnings = FALSE){

  # check arguments
  model   <- match.arg(model)
  target  <- match.arg(target,
                       choices = c("ate", "att", "atu"),
                       several.ok = TRUE)

  # plm estimates a single average effect; "att"/"atu" targets are not available
  if (model == "plm" && any(target %in% c("att", "atu"))) {
    warning("model = 'plm' estimates a single average effect and cannot ",
            "target 'att'/'atu'; reporting the ATE.")
    target <- "ate"
  }
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

  # check whether any columns in x are redundant because of an invalid reference group
  var_names <- list()
  for (name in colnames(x)) {
    if ((length(unique(x[,name])) == 1) && (name != "(Intercept)")) {
      warning("Variable '", name,
              "' shows no variation.", call. = FALSE)
    }
    prefix <- sub("[0-9]+$", "", name)
    var_names[[prefix]] <- unique(c(var_names[[prefix]], name))
  }
  factor_var <- Filter(function(g) length(g) >= 2, var_names)
  for (var_name in names(factor_var)) {
    factor_cols <- factor_var[[var_name]]
    rowsums <- rowSums(x[, factor_cols, drop = FALSE])
    if (all(rowsums == 1)) {
      warning("The reference category for the variable '", var_name,
              "' contains no observations.", call. = FALSE)
    }
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

  # auto-set conditional: TRUE whenever the target admits it (single "att"/"atu"
  # target under "npm"). This is the parameterization of Wang et al. (2026); the
  # unconditional one is only ever used when explicitly requested.
  if (is.null(conditional)) {
    conditional <- (model == "npm" &&
                      (identical(target, "att") || identical(target, "atu")))
  }
  if (conditional && model != "npm") {
    warning("conditional = TRUE is only supported for model = 'npm'; setting conditional = FALSE.")
    conditional <- FALSE
  }
  if (conditional && !(identical(target, "att") || identical(target, "atu"))) {
    warning("conditional = TRUE requires a single target = 'att' or 'atu'; setting conditional = FALSE.")
    conditional <- FALSE
  }


  out <- list()
  out$data <- list(y = y, d = d, x = x)
  out$call <-   match.call()
  # Snapshot the variables the stored call names, so functions that re-evaluate
  # it (e.g. dml_benchmark) resolve the user's objects. Copy the named variables
  # rather than the caller's frame, which would keep every local alive and would
  # be serialized with the fit.
  out$call.env <- snapshot.call.env(out$call, parent.frame())

  if (y.class) {
    y <- factor(y, levels = c(0,1), labels = c("zero", "one"))
  }

  if (d.class) {
    d <- factor(d, levels = c(0,1), labels = c("zero", "one"))
  }

  d0 <- ifelse(d.class, "zero", 0)
  d1 <- ifelse(d.class, "one", 1)

  if (is.list(yreg) &&
      isTRUE(all.equal(tolower(names(yreg)), c("yreg0", "yreg1")))) {
    yreg0 <- yreg$yreg0
    yreg1 <- yreg$yreg1
  } else {
    yreg0 <- yreg1 <- yreg
  }

  yreg0 <- caretArgs(yreg0)
  yreg1 <- caretArgs(yreg1)
  yreg <- list(yreg0 = yreg0, yreg1 = yreg1)
  dreg <- caretArgs(dreg)

  out$info <- list(model = model,
                   target = target,
                   conditional = conditional,
                   centered = centered,
                   cf.folds = cf.folds,
                   cf.reps = cf.reps,
                   dirty.tuning = dirty.tuning,
                   yreg = yreg,
                   dreg = dreg)

  if (verbose) {
    cat("Debiased Machine Learning\n")
    cat("\n")
    cat("", "Model:", ifelse(out$info$model == "plm", "Partially Linear", "Nonparametric"), "\n")
    cat("", "Target:", out$info$target, if (conditional) "(conditional)" else "(unconditional)", "\n")
    cat("", "Cross-Fitting:", out$info$cf.folds, "folds,", out$info$cf.reps, "reps", "\n")
    # conditional ATT uses yreg0 only; conditional ATU uses yreg1 only -- unless
    # groups are requested, which need both arms for the group estimand
    drop.arm    <- conditional
    yreg0_label <- if (drop.arm && identical(target, "atu")) "(not used)" else attr(out$info$yreg$yreg0$method, "name")
    yreg1_label <- if (drop.arm && identical(target, "att")) "(not used)" else attr(out$info$yreg$yreg1$method, "name")
    cat("", "ML Method:",
        "outcome", paste0("(yreg0:", yreg0_label,
                          ", yreg1:", yreg1_label, "),"),
        "treatment", paste0("(", attr(out$info$dreg$method,"name"), ")\n"))
    cat("", "Tuning:", ifelse(out$info$dirty.tuning, "dirty", "clean"), "\n")
    cat("\n")
  }



  if (dirty.tuning) {

    if (!is.null(cf.seed)) set.seed(cf.seed)

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

      if (model == "npm" & any(target %in% c("ate","att", "atu"))) {
        ytil0 <- y[d == d0]
        ytil1 <- y[d == d1]

        muy0 <- min(ytil0)
        sdy0 <- max(ytil0) - min(ytil0)
        ytil0 <- (ytil0 - muy0)/sdy0

        muy1 <- min(ytil1)
        sdy1 <- max(ytil1) - min(ytil1)
        ytil1 <- (ytil1 - muy1)/sdy1
      }
    } else {
      ytil <- y
      muy <- 0
      sdy <- 1

      if (model == "npm" & any(target %in% c("ate","att", "atu"))) {
        ytil0 <- y[d == d0]
        ytil1 <- y[d == d1]
        muy0 <- muy1 <- 0
        sdy0 <- sdy1 <- 1
      }
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
      if (!isTRUE(all.equal(yreg0, yreg1))) {
        warning("Only one method should be specified for yreg when using 'plm'; setting 'yreg' to 'yreg0'.")
      }
      yreg  <- tune_model(x =  x, y = ytil, args = yreg0)
      if (verbose) {
        cat("-- Best Tune:\n")
        print(yreg$tuneGrid)
        cat("\n")
      }

    }

    if (model == "npm") {
      if (verbose) cat("- Tuning Model for Y (non-parametric).\n")
      # conditional ATT tunes yreg0 (controls) only; conditional ATU tunes
      # yreg1 (treated) only; otherwise both sides are tuned.
      cond.att <- conditional && identical(target, "att")
      cond.atu <- conditional && identical(target, "atu")

      if (!cond.atu) {
        yreg0 <- tune_model(x = x[d == d0, ,drop = FALSE], y = ytil0, args = yreg0)
      } else {
        yreg0 <- NULL
      }
      if (!cond.att) {
        yreg1 <- tune_model(x = x[d == d1, ,drop = FALSE], y = ytil1, args = yreg1)
      } else {
        yreg1 <- NULL
      }
      yreg <- list(yreg0 = yreg0, yreg1 = yreg1)
      if (verbose) {
        cat("-- Best Tune:\n")
        if (!is.null(yreg0)) print(yreg0$tuneGrid)
        if (!is.null(yreg1)) print(yreg1$tuneGrid)
        cat("\n")
      }
    }
      out$dreg <- dreg
      out$yreg <- yreg
      # update info to reflect the final tuned specs; plm tuning returns a flat
      # spec, so re-wrap it in the yreg0/yreg1 structure the record expects
      out$info$yreg <- if (model == "plm") list(yreg0 = yreg, yreg1 = yreg) else yreg

  }

  # conditional fits never use the counterfactual arm's outcome model; drop its
  # spec on every path (not just under dirty.tuning) so it is never trained
  # A conditional fit needs one outcome arm, so drop the other arm's spec on every
  # path, not only under dirty tuning. The group rows report the same estimand
  # within each group, so they do not need the dropped arm either.
  if (model == "npm" && conditional) {
    if (is.list(yreg) &&
        isTRUE(all.equal(tolower(names(yreg)), c("yreg0", "yreg1")))) {
      yreg0.use <- yreg$yreg0
      yreg1.use <- yreg$yreg1
    } else {
      yreg0.use <- yreg1.use <- yreg
    }
    yreg <- if (identical(target, "att")) {
      list(yreg0 = yreg0.use, yreg1 = NULL)
    } else {
      list(yreg0 = NULL, yreg1 = yreg1.use)
    }
    out$info$yreg <- yreg
  }

  if (!is.null(cf.seed)) set.seed(cf.seed)
  cf.seeds <- sample.int(1e6, size = cf.reps)

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
                                    target       = target,
                                    cf.folds     = cf.folds,
                                    cf.seed      = cf.seeds[i],
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

    # a conditional npm fit builds its own results below, so skip the score here
    if (model == "npm" && !conditional) {
      phat   <- cross.fit.i$preds$phat
      dhat   <- cross.fit.i$preds$dhat
      yhat0  <- cross.fit.i$preds$yhat0
      yhat1  <- cross.fit.i$preds$yhat1
      results[[i]] <- ate.npm(num(y),
                              num(d),
                              parameter = "all",
                              yhat1, yhat0, dhat, phat, trim = ps.trim)

      results[[i]]$trim.summary$trimmed_obs <-
        collect.trimmed.obs(y, d, x, results[[i]]$trim.summary$trimmed_indices)
    }
    if (verbose) cat("\n")
  }

  out$fits <- fits
  # a conditional npm fit replaces main below, so skip the score it would discard
  if (!(model == "npm" && conditional)) {
    out$results$main$all <- results
    out$coefs$main$all   <- combine.cross.fits(results)
  }

  if (model == "npm") {
    if (conditional) {
      # conditional path: att.npm.cond() (target "att") or atu.npm.cond()
      # (target "atu") for each rep. Results stored in the matching slot
      # ("treat" for ATT, "untr" for ATU).
      cond_results <- vector("list", cf.reps)
      for (i in seq_len(cf.reps)) {
        phat  <- fits[[i]]$preds$phat
        dhat  <- fits[[i]]$preds$dhat
        yhat0 <- fits[[i]]$preds$yhat0
        yhat1 <- fits[[i]]$preds$yhat1
        if (identical(target, "att")) {
          cond_results[[i]] <- att.npm.cond(
            y      = num(y),
            d      = num(d),
            yhat0  = yhat0,
            yhat1  = NULL,
            dhat   = dhat,
            phat   = phat,
            trim   = ps.trim,
            centered = centered
          )
        } else {
          cond_results[[i]] <- atu.npm.cond(
            y      = num(y),
            d      = num(d),
            yhat1  = yhat1,
            yhat0  = NULL,
            dhat   = dhat,
            phat   = phat,
            trim   = ps.trim,
            centered = centered
          )
        }
        cond_results[[i]]$trim.summary$trimmed_obs <-
          collect.trimmed.obs(y, d, x, cond_results[[i]]$trim.summary$trimmed_indices)
      }
      cond_slot <- if (identical(target, "att")) "treat" else "untr"
      # the conditional target is the only estimand actually computed; replace
      # main entirely so the half-fit unconditional "all" slot does not linger
      out$results$main <- setNames(list(cond_results), cond_slot)
      out$coefs$main   <- setNames(list(combine.cross.fits(cond_results)), cond_slot)
    } else {
      out$results$main <- ate.att.atu.npm(out, target = target, trim = ps.trim)
      out$coefs$main   <- lapply(out$results$main, combine.cross.fits)
    }
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

##' @description The function \code{dml_gate} is a convenience function that adds groups to a \code{dml} object after the model is fit.
##'
##' @param dml.fit an object of class \code{\link{dml}}.
##' @param ... arguments passed to other methods.
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
  args$trControl <- trainControl(method = "none", classProbs = TRUE)
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

# Copies the variables a stored call names, so a fit can be re-evaluated later
# without holding a reference to the caller's frame (which would keep every
# local alive and would be serialized with the fit).
snapshot.call.env <- function(call, env) {
  vars  <- all.vars(call)
  found <- mget(vars, envir = env, inherits = TRUE,
                ifnotfound = vector("list", length(vars)))
  found <- found[!vapply(found, is.null, logical(1))]
  list2env(found, parent = globalenv())
}

# records the observations dropped by propensity-score trimming
collect.trimmed.obs <- function(y, d, x, trimmed_indices) {
  grab <- function(idx) list(y.trimmed = y[idx],
                             d.trimmed = d[idx],
                             x.trimmed = x[idx, , drop = FALSE])
  list(all  = grab(trimmed_indices$all),
       low  = grab(trimmed_indices$low),
       high = grab(trimmed_indices$high))
}

# Chernozhukov et al. (2018), Def. 3.3: the across-repetition term is the
# squared deviation of each repetition from the aggregated median, and it is
# taken inside the median, not added to it as a constant.
combine.median <- function(thetas, ses, na.rm = FALSE){
  median.theta    <- median(thetas, na.rm = na.rm)
  se.median.theta <- sqrt(median(ses^2 + (thetas - median.theta)^2, na.rm = na.rm))
  c(estimate = median.theta, se = se.median.theta)
}

combine.mean <- function(thetas, ses, na.rm = FALSE){
  mean.theta    <- mean(thetas, na.rm = na.rm)
  ss <- sum((thetas - mean.theta)^2, na.rm = na.rm)
  R  <- if (na.rm) sum(!is.na(thetas)) else length(thetas)
  se.mean.theta   <- sqrt(mean(ses^2, na.rm = na.rm) + ss/R)
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
  reg$trControl$classProbs <- TRUE
  return(reg)
}

