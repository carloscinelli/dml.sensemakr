
##'@importFrom  caret trainControl
cross.fitting <- function(y, d, x,
                          model = c("plm","npm"),
                          d1 = 1,
                          d0 = 0,
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
  dx   <- cbind(d, x)
  dx0  <- cbind("d" = rep(d0, nobs), x)
  dx1  <- cbind("d" = rep(d1, nobs), x)

  if (verbose) cat(" Folds: ")
  for(b in 1:length(Id)){

    if (verbose) cat(b," ")

    # d model
    args.dx  <- c(list(x = x[ -Id[[b]], ,drop = F],  y = d[ -Id[[b]] ] ), dreg)
    model.dx <- silent.do.call(what = "train", args = args.dx, warnings = warnings)
    metric.d <- model.dx$metric

    if(save.models){
      out$model.d[[b]] <- model.dx
    }

    # predictions
    dhat[Id[[b]]]  <-  predict(model.dx, newdata =   x[ Id[[b]], ,drop = F])


    if(model == "plm"){
      # y model for plm
      args.yx  <- c(list(x = x[ -Id[[b]], ,drop = F], y = y[ -Id[[b]] ] ), yreg)
      model.yx <- silent.do.call(what = "train", args = args.yx, warnings = warnings)
      metric.y <- model.yx$metric

      if(save.models){
        out$model.y[[b]] <- model.yx
      }

      # predictions for plm
      yhat[Id[[b]]]    <- predict(model.yx, x[Id[[b]], ,drop = F]) #predict the left-out fold
    }


    if(model == "npm"){
      # y model for npm
      args.ydx  <- c(list(x = dx[ -Id[[b]], ,drop = F], y = y[ -Id[[b]] ] ), yreg)
      model.ydx <- silent.do.call(what = "train", args = args.ydx, warnings = warnings)
      metric.y <- model.ydx$metric

      if(save.models){
        out$model.y[[b]] <- model.ydx
      }

      # predictions for npm
      yhat[Id[[b]]]  <-  predict(model.ydx, newdata =  dx[ Id[[b]], ,drop = F])
      yhat0[Id[[b]]] <-  predict(model.ydx, newdata = dx0[ Id[[b]], ,drop = F])
      yhat1[Id[[b]]] <-  predict(model.ydx, newdata = dx1[ Id[[b]], ,drop = F])
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
    out <- suppressWarnings(do.call(...))
  }
  return(out)
}
