#' @importFrom caret train
#' @export
dml <- function(y, d, x,
                model = c("plm", "npm"),
                groups = NULL,
                cf.folds = 5,
                cf.reps  = 1,
                ps.trim = 0.02,
                yreg = list(method = "ranger",
                            trControl = list(method = "none"),
                            tuneGrid  = data.frame(mtry = sqrt(ncol(x)), splitrule = "variance", min.node.size = 5)),
                dreg = list(method    = "ranger",
                            trControl = list(method = "none"),
                            tuneGrid  = data.frame(mtry = sqrt(ncol(x)), splitrule = "variance", min.node.size = 5)),
                dirty.tuning = FALSE,
                save.models = F,
                verbose = TRUE,
                warnings = FALSE){

  # check arguments
  model   <- match.arg(model)

  if(model == "npm"){
    d.value <- unique(d)
    binary <- all(d.value %in% c(0,1))
    if(!binary) stop("Treatment must be binary for nonparametric model (model = npm).")
  }

  if(cf.folds < 2){
    cf.folds <- 2
    warning("cf.folds set to 2 (number of cross-fitting folds need to be at least 2).")
  }
  out <- list()
  out$call <-   match.call()


  out$info <- list(model = model,
                   cf.folds = cf.folds,
                   cf.reps = cf.reps,
                   dirty.tuning = dirty.tuning,
                   yreg = yreg,
                   dreg = dreg)

  out$data <- list(y = y, d= d, x = x)


  yreg$trControl <- do.call("trainControl", yreg$trControl)
  dreg$trControl <- do.call("trainControl", dreg$trControl)


  if(verbose) cat("Debiased Machine Learning\n")

  if (dirty.tuning){

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
    cat("-- Rep", i, "--")
    cross.fit.i    <- cross.fitting(y            = y,
                                    d            = d,
                                    x            = x,
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
      results[[i]] <- ate.plm(y, d, yhat, dhat)
    }

    if(model == "npm"){
      dhat   <- cross.fit.i$preds$dhat
      yhat0  <- cross.fit.i$preds$yhat0
      yhat1  <- cross.fit.i$preds$yhat1
      results[[i]] <- ate.npm(y, d,
                              yhat1, yhat0, dhat, trim = ps.trim)
    }
    cat("\n")
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

# r2
r2 <- function(pred, obs) max(1-var(obs-pred)/var(obs), 0)

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

  # mean and median
  mean.theta    <- mean(thetas)
  median.theta  <- median(thetas)

  # var mean / var median
  ss <- c((thetas - mean(thetas)) %*% (thetas - mean(thetas)))
  se.mean.theta   <- sqrt(mean(ses^2) + ss/length(ses))
  se.median.theta <- sqrt(median(ses^2 + c(ss)))
  out <- rbind(mean =   c(estimate = mean.theta,   se = se.mean.theta),
               median = c(estimate = median.theta, se = se.median.theta))
  out
}
