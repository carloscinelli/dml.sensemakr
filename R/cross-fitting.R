
##'@importFrom  caret trainControl
cross.fitting <- function(y, d, x,
                          model = c("plm","npm"),
                          d1 = 1, d0 = 0,
                          cf.folds = 5,
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


  # sample splitting
  nobs     <- nrow(x) # number of observations
  fold.id  <- rep.int(1:cf.folds, times = ceiling(nobs/cf.folds))[sample.int(nobs)] # define folds indices
  Id       <- split(1:nobs, fold.id)  # split observation indices into folds


  # predictions
  dhat <- yhat <- yhat1 <-  yhat0 <- rep(NA, nobs)

  # data for npm
  dx   <- data.frame(d, x)
  dx0  <- data.frame("d" = rep(d0, nobs), x)
  dx1  <- data.frame("d" = rep(d1, nobs), x)
  if (verbose) cat(" -- Folds: ")

  for (b in 1:length(Id)) {

    if (verbose) cat(b," ")



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

      args.dx  <- c(list(x = x[ -Id[[b]], ,drop = F],  y = dtil ), dreg)
      model.dx <- silent.do.call(what = "train", args = args.dx, warnings = warnings)
      metric.d <- model.dx$metric

      # predictions
      dhat[Id[[b]]]  <-  safe.predict(model.dx, newdata =   x[ Id[[b]], ,drop = F])*sdd + mud

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

      args.yx  <- c(list(x = x[ -Id[[b]], ,drop = F], y = ytil ), yreg)
      model.yx <- silent.do.call(what = "train", args = args.yx, warnings = warnings)
      metric.y <- model.yx$metric

      if(save.models){
        out$model.d[[b]] <- model.dx
        out$model.y[[b]] <- model.yx
      }


      # predictions for plm
      yhat[Id[[b]]]    <- safe.predict(model.yx, x[Id[[b]], ,drop = F])*sdy + muy #predict the left-out fold
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
      args.dx  <- c(list(x = x[ -Id[[b]], ,drop = F],  y = dtil ), dreg)
      model.dx <- silent.do.call(what = "train", args = args.dx, warnings = warnings)
      metric.d <- model.dx$metric

      # predictions
      dhat[Id[[b]]]  <-  safe.predict(model.dx, newdata =   x[ Id[[b]], ,drop = F])*sdd + mud

      # y model for npm
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

      args.ydx  <- c(list(x = dx[ -Id[[b]], ,drop = F], y = ytil), yreg)
      model.ydx <- silent.do.call(what = "train", args = args.ydx, warnings = warnings)
      metric.y <- model.ydx$metric

      if (save.models) {
        out$model.y[[b]] <- model.ydx
        out$model.d[[b]] <- model.dx
      }

      # predictions for npm
      yhat[Id[[b]]]  <-  safe.predict(model.ydx, newdata =  dx[ Id[[b]], ,drop = F])*sdy + muy
      yhat0[Id[[b]]] <-  safe.predict(model.ydx, newdata = dx0[ Id[[b]], ,drop = F])*sdy + muy
      yhat1[Id[[b]]] <-  safe.predict(model.ydx, newdata = dx1[ Id[[b]], ,drop = F])*sdy + muy
    }

  }
  out$metric.d <- metric.d
  out$metric.y <- metric.y

  if(model == "plm"){
    out$preds$dhat <- dhat
    out$preds$yhat <- yhat
  }

  if(model == "npm"){
    out$preds$dhat  <- dhat
    out$preds$yhat  <- yhat
    out$preds$yhat0 <- yhat0
    out$preds$yhat1 <- yhat1
  }
  cat("\n")

  return(out)
}

# function to suppress irrelevant caret warnings
silent.do.call <- function(..., warnings = F) {
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
