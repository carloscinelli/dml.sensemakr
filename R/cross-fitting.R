
##'@importFrom  caret trainControl
cross.fitting <- function(y, d, x,
                          model = c("plm","npm"),
                          target = "ate",
                          d1 = 1, d0 = 0,
                          cf.folds = 5,
                          cf.seed = NULL,
                          yreg = list(method = "ranger",
                                      trControl = trainControl(method = "none"),
                                      tuneGrid = data.frame(mtry = sqrt(ncol(x)), splitrule = "variance", min.node.size = 5)),
                          dreg = list(method = "ranger",
                                      trControl = trainControl(method = "none"),
                                      tuneGrid = data.frame(mtry = sqrt(ncol(x)), splitrule = "variance", min.node.size = 5)),
                          verbose = TRUE,
                          warnings = FALSE,
                          save.models = FALSE
){

  model   <- match.arg(model)

  out <- list()

  if (!is.null(cf.seed)) set.seed(cf.seed)

  # sample splitting
  nobs     <- nrow(x) # number of observations
  fold.id  <- rep.int(1:cf.folds, times = ceiling(nobs/cf.folds))[sample.int(nobs)] # define folds indices
  Id       <- split(1:nobs, fold.id)  # split observation indices into folds

  if (is.list(yreg) &&
      isTRUE(all.equal(tolower(names(yreg)), c("yreg0", "yreg1")))) {
    yreg0 <- yreg$yreg0
    yreg1 <- yreg$yreg1
  } else {
    yreg0 <- yreg1 <- yreg
  }

  # predictions
  dhat <- yhat <- yhat1 <-  yhat0 <- phat <- rep(NA, nobs)

  # data for npm
  dx   <- data.frame(d, x)
  dx0  <- data.frame("d" = rep(d0, nobs), x)
  dx1  <- data.frame("d" = rep(d1, nobs), x)
  if (verbose) cat(" -- Folds: ")

  for (b in 1:length(Id)) {

    if (verbose) cat(b," ")

    phat[Id[[b]]] <- mean(d[ -Id[[b]] ])

    if (model == "plm") {
      # d model
      if (is.numeric(d)) {
        dtil <- d[ -Id[[b]] ]
        mud <- min(dtil)
        sdd <- max(dtil) - min(dtil)
        dtil <- (dtil - mud)/sdd
      } else {
        dtil <- d[ -Id[[b]] ]
        mud <- 0
        sdd <- 1
      }

      args.dx  <- c(list(x = x[ -Id[[b]], ,drop = FALSE],  y = dtil ), dreg)
      model.dx <- silent.do.call(what = "train", args = args.dx, warnings = warnings)
      metric.d <- model.dx$metric

      # predictions
      dhat[Id[[b]]]  <-  safe.predict(model.dx, newdata =   x[ Id[[b]], ,drop = FALSE])*sdd + mud

      # y model for plm
      if (is.numeric(y)) {
        ytil <- y[ -Id[[b]] ]
        # min-max normalization
        muy <- min(ytil)
        sdy <- max(ytil) - min(ytil)
        ytil <- (ytil - muy)/sdy
      } else {
        ytil <- y[ -Id[[b]] ]
        muy <- 0
        sdy <- 1
      }

      if (!isTRUE(all.equal(yreg0, yreg1))) {
        yreg <- yreg0
        warning("Only one method should be specified for yreg when using 'plm'; setting 'yreg' to 'yreg0'.")
      }

      args.yx  <- c(list(x = x[ -Id[[b]], ,drop = FALSE], y = ytil ), yreg)
      model.yx <- silent.do.call(what = "train", args = args.yx, warnings = warnings)
      metric.y <- model.yx$metric

      if(save.models){
        out$model.d[[b]] <- model.dx
        out$model.y[[b]] <- model.yx
      }


      # predictions for plm
      yhat[Id[[b]]]    <- safe.predict(model.yx, x[Id[[b]], ,drop = FALSE])*sdy + muy #predict the left-out fold
    }


    if(model == "npm"){

      if (is.numeric(d)) {
        dtil <- d[ -Id[[b]] ]
        # min-max normalization
        mud <- min(dtil)
        sdd <- max(dtil) - min(dtil)
        dtil <- (dtil - mud)/sdd
      } else {
        dtil <- d[ -Id[[b]] ]
        mud <- 0
        sdd <- 1
      }

      # d model
      args.dx  <- c(list(x = x[ -Id[[b]], ,drop = FALSE],  y = dtil ), dreg)
      model.dx <- silent.do.call(what = "train", args = args.dx, warnings = warnings)
      metric.d <- model.dx$metric

      # predictions
      dhat[Id[[b]]]  <-  safe.predict(model.dx, newdata =   x[ Id[[b]], ,drop = FALSE])*sdd + mud

      # y model for npm
      if (is.numeric(y)) {
        ytil0 <- y[-Id[[b]]][dtil == d0]
        muy0 <- min(ytil0)
        sdy0 <- max(ytil0) - min(ytil0)
        ytil0 <- (ytil0 - muy0)/sdy0

        ytil1 <- y[-Id[[b]]][dtil == d1]
        muy1 <- min(ytil1)
        sdy1 <- max(ytil1) - min(ytil1)
        ytil1 <- (ytil1 - muy1)/sdy1
      } else {
        ytil0 <- y[-Id[[b]]][dtil == d0]
        ytil1 <- y[-Id[[b]]][dtil == d1]
        muy0 <- muy1 <- 0
        sdy0 <- sdy1 <- 1
      }

      args.y0x  <- c(
        list(x = x[ -Id[[b]], ,drop = FALSE][dtil == d0, ,drop = FALSE],
             y = ytil0), yreg0)
      model.y0x <-
        silent.do.call(what = "train", args = args.y0x, warnings = warnings)
      metric.y0 <- model.y0x$metric

      args.y1x  <- c(
        list(x = x[ -Id[[b]], ,drop = FALSE][dtil == d1, ,drop = FALSE],
             y = ytil1), yreg1)
      model.y1x <-
        silent.do.call(what = "train", args = args.y1x, warnings = warnings)
      metric.y1 <- model.y1x$metric

      metric.y <- list(metric.y0 = metric.y0, metric.y1 = metric.y1)

      if (save.models) {
        out$model.y[[b]] <- list(model.y0x = model.y0x, model.y1x = model.y1x)
        out$model.y0[[b]] <- model.y0x
        out$model.y1[[b]] <- model.y1x
        out$model.d[[b]] <- model.dx
      }

      if (all(target == "att")) {
        yhat0[Id[[b]]] <- safe.predict(model.y0x, newdata = x[Id[[b]], ,drop = FALSE])*sdy0 + muy0
        yhat1[Id[[b]]][num(d[Id[[b]]]) == 1] <- safe.predict(model.y1x, newdata = x[Id[[b]][num(d[Id[[b]]]) == 1], ,drop = FALSE])*sdy1 + muy1
      } else if (all(target == "atu")) {
        yhat0[Id[[b]]][num(d[Id[[b]]]) == 0] <- safe.predict(model.y0x, newdata = x[Id[[b]][num(d[Id[[b]]]) == 0], ,drop = FALSE])*sdy0 + muy0
        yhat1[Id[[b]]] <- safe.predict(model.y1x, newdata = x[Id[[b]], ,drop = FALSE])*sdy1 + muy1
      } else {
        yhat0[Id[[b]]] <- safe.predict(model.y0x, newdata = x[Id[[b]], ,drop = FALSE])*sdy0 + muy0
        yhat1[Id[[b]]] <- safe.predict(model.y1x, newdata = x[Id[[b]], ,drop = FALSE])*sdy1 + muy1
      }

      yhat[Id[[b]]][num(d[Id[[b]]]) == 0] <- yhat0[Id[[b]]][num(d[Id[[b]]]) == 0]
      yhat[Id[[b]]][num(d[Id[[b]]]) == 1] <- yhat1[Id[[b]]][num(d[Id[[b]]]) == 1]
    }

  }
  out$metric.d <- metric.d
  out$metric.y <- metric.y

  if(model == "plm"){
    out$preds$dhat <- dhat
    out$preds$yhat <- yhat

    rmse_dhat <- sqrt(mean((num(d) - dhat)^2))
    rmse_yhat <- sqrt(mean((num(y) - yhat)^2))
    out$rmse$dhat <- rmse_dhat
    out$rmse$yhat <- rmse_yhat
  }

  if(model == "npm"){
    out$metric.y0 <- metric.y0
    out$metric.y1 <- metric.y1

    out$preds$dhat  <- dhat
    out$preds$yhat  <- yhat
    out$preds$yhat0 <- yhat0
    out$preds$yhat1 <- yhat1

    rmse_dhat <- sqrt(mean((num(d) - dhat)^2))
    rmse_yhat0 <- sqrt(mean((num(y[num(d) == 0]) -
                               yhat0[num(d) == 0])^2))
    rmse_yhat1 <- sqrt(mean((num(y[num(d) == 1]) -
                               yhat1[num(d) == 1])^2))
    rmse_yhat <- sqrt(mean((num(y) - yhat)^2))
    out$rmse$dhat <- rmse_dhat
    out$rmse$yhat0 <- rmse_yhat0
    out$rmse$yhat1 <- rmse_yhat1
    out$rmse$yhat <- rmse_yhat
  }
  out$preds$phat <- phat
  if(verbose) cat("\n")

  return(out)
}

# function to suppress irrelevant caret warnings
silent.do.call <- function(..., warnings = FALSE) {
  if(warnings){
    out <- do.call(...)
  } else {
    messages <- capture.output(out <- suppressMessages(suppressWarnings(do.call(...))))
  }
  return(out)
}

safe.predict <- function(model, newdata){
  type <- model$modelType
  if(type == "Regression"){
    pred <- predict(object=model, newdata = newdata)
  } else {
    pred <- predict(object=model, newdata =newdata, type = "prob")
    pred <- pred$one
  }
  return(pred)
}
