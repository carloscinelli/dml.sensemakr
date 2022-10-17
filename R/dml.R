#' @importFrom caret train
#' @import caret
#' @export
dml <- function(y, d, x,
                model = c("plm", "npm"),
                groups = NULL,
                binary.y = F,
                binary.d = F,
                cf.folds = 5,
                cf.reps  = 5,
                ps.trim = 0.01,
                yreg = list(method = "ranger"),
                dreg = yreg,
                dirty.tuning = FALSE,
                save.models = F,
                verbose = TRUE,
                warnings = FALSE){

  # check arguments
  model   <- match.arg(model)


  if (is.numeric(x) && !is.matrix(x)) {
    x <- as.matrix(x)
    colnames(x) <- "x"
  }

  if (is.null(colnames(x))) {
    colnames(x) <- paste0("x", 1:ncol(x))
  }

  if (model == "npm") {
    d.value <- unique(d)
    binary <- all(d.value %in% c(0,1))
    if (!binary) stop("Treatment must be binary for nonparametric model (model = npm).")
  }

  if (cf.folds < 2) {
    cf.folds <- 2
    warning("cf.folds set to 2 (number of cross-fitting folds need to be at least 2).")
  }


  out <- list()
  out$data <- list(y = y, d = d, x = x)
  out$call <-   match.call()

  if (binary.y) {
    y <- factor(y, levels = c(0,1), labels = c("zero", "one"))
  }

  if (binary.d) {
    d <- factor(d, levels = c(0,1), labels = c("zero", "one"))
  }


  out$info <- list(model = model,
                   cf.folds = cf.folds,
                   cf.reps = cf.reps,
                   dirty.tuning = dirty.tuning,
                   yreg = yreg,
                   dreg = dreg)

  yreg <- caretArgs(yreg)
  dreg <- caretArgs(dreg)

  if (verbose) {
    cat("Debiased Machine Learning\n")
  }

  if (dirty.tuning) {

    if(verbose){
      cat("\n")
      cat("====================================\n")
      cat("Tuning parameters using all the data\n")
      cat("====================================\n\n")
    }

    if (verbose) cat("- Tuning Model for D.\n\n")

      dreg  <- tune_model(x =  x, y = d, args = dreg)

    if(model == "plm") {
      if (verbose) cat("- Tuning Model for Y (partially linear).\n\n")
      yreg  <- tune_model(x =  x, y = y, args = yreg)
    }

    if(model == "npm") {
      if (verbose) cat("- Tuning Model for Y (non-parametric).\n")
      dx = cbind(d, x)
      yreg <- tune_model(x = dx, y = y, args = yreg)
    }

  }

  fits <- results <- preds <-  list()

  if(verbose){
    cat("\n")
    cat("======================================\n")
    cat("Repeating", paste0(cf.folds,"-fold"), "cross-fitting", cf.reps, "times\n")
    cat("======================================\n\n")
  }
  for(i in 1:cf.reps){
    cat("-- Rep", i)
    cross.fit.i    <- cross.fitting(y            = y,
                                    d            = d,
                                    x            = x,
                                    d1           = ifelse(binary.d, "one", 1),
                                    d0           = ifelse(binary.d, "zero", 0),
                                    model        = model,
                                    cf.folds     = cf.folds,
                                    yreg         = yreg,
                                    dreg         = dreg,
                                    verbose      = verbose,
                                    warnings     = warnings,
                                    save.models  = save.models)

    fits[[i]] <- cross.fit.i


    if(model == "plm"){
      dhat   <- cross.fit.i$preds$dhat
      yhat <- cross.fit.i$preds$yhat
      results[[i]] <- ate.plm(num(y),
                              num(d),
                              yhat,
                              dhat)
    }

    if(model == "npm"){
      dhat   <- cross.fit.i$preds$dhat
      yhat0  <- cross.fit.i$preds$yhat0
      yhat1  <- cross.fit.i$preds$yhat1
      results[[i]] <- ate.npm(num(y),
                              num(d),
                              yhat1, yhat0, dhat, trim = ps.trim)
    }
    if(verbose) cat("\n")
  }

  out$fits <- fits
  out$results$main <- results
  out$coefs$main   <- combine.cross.fits(results)

  # Group ATE
  if(!is.null(groups)){
    groups  <- as.factor(groups)

    if(model == "npm"){
      out$results$groups <- group.ate.npm(out, groups)
      out$coefs$groups   <- lapply(out$results$groups, combine.cross.fits)
    }

    if(model == "plm"){
      out$results$groups <- group.ate.plm(out, groups)
      out$coefs$groups   <- lapply(out$results$groups, combine.cross.fits)
    }

  }

  class(out) <- "dml"

  return(out)

}

#' @export
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
  if(is.factor(v)){
    return(ifelse(v == "zero", 0, 1))
  } else {
    return(v)
  }
}

# function to tune the model outside cross-fitting
# returns the best tune
tune_model <- function(x, y, args) {
  original.args <- c(list(x = x, y = y), args)
  best.model    <- silent.do.call(what = "train", args = original.args)
  tuned.args      <- list(method = original.args$method,
                          trControl = trainControl(method = "none"),
                          tuneGrid = best.model$bestTune)
  return(tuned.args)
}

# extracts estimates from dml.fit
extract_estimate <- function(results, param) sapply(results, function(x) x$estimates[[param]])

# r2
r2 <- function(pred, obs){
  max(1-var(obs-pred)/var(obs), 0)
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
    reg <- list(method = reg)
  }
  if (is.character(reg$method)) {

    if (!is.null(models[[reg$method]])) {
       reg$method <- models[[reg$method]]
    }
  }

  if (is.null(reg$trControl)) {
    reg$trControl <- trainControl(method = "none")
  } else {
    reg$trControl <- do.call("trainControl", reg$trControl)
  }
  if (is.null(reg$preProcess)){
    reg$preProcess <- "range"
  }

  reg$trControl$classProbs <- T

  return(reg)
}

