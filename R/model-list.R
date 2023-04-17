models <- list()



# rlasso ------------------------------------------------------------------
models$rlasso <- list(label = "Rigorous Lasso",
                   library = "hdm",
                   loop = NULL,
                   type = c('Regression'),
                   parameters = data.frame(parameter = c('parameter'),
                                           class = c('character'),
                                           label = c('parameter')),
                   grid = function(x, y, len = NULL, search = "grid") {
                     data.frame(parameter = "none")
                   },
                   fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                     require(rlasso)
                     theDots <- list(...)
                     modelArgs <- c(list(x = x, y = y), theDots)
                     out <- do.call(hdm::rlasso, modelArgs)
                     out

                   },
                   predict = function(modelFit, newdata, submodels = NULL) {
                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                     out <- predict(object = modelFit, newdata = newdata)
                     },
                   prob = NULL,
                   predictors = NULL,
                   levels = NULL,
                   varImp = NULL,
                   notes = NULL,
                   tags = c("Lasso"),
                   sort = function(x) x)



# GAM ---------------------------------------------------------------------


models$gam <- list(label = "Generalized Additive Model using Splines",
                   library = "mgcv",
                   loop = NULL,
                   type = c('Regression', 'Classification'),
                   parameters = data.frame(parameter = c('select', 'method'),
                                           class = c('logical', 'character'),
                                           label = c('Feature Selection', 'Method')),
                   grid = function(x, y, len = NULL, search = "grid") {
                     if(search == "grid") {
                       out <- expand.grid(select = c(TRUE, FALSE)[1:min(2,len)], method = "GCV.Cp")
                     } else {
                       out <- data.frame(select = sample(c(TRUE, FALSE), size = len, replace = TRUE),
                                         method = sample(c("GCV.Cp", "ML"), size = len, replace = TRUE))
                     }
                   },
                   fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                     require(mgcv)
                     dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
                     modForm <- caret:::smootherFormula(x)
                     if(is.factor(y)) {
                       dat$.outcome <- ifelse(y == lev[1], 0, 1)
                       dist <- binomial()
                     } else {
                       dat$.outcome <- y
                       dist <- gaussian()
                     }
                     modelArgs <- list(formula = modForm,
                                       data = dat,
                                       select = param$select,
                                       method = as.character(param$method))
                     ## Intercept family if passed in
                     theDots <- list(...)
                     if(!any(names(theDots) == "family")) modelArgs$family <- dist
                     modelArgs <- c(modelArgs, theDots)

                     out <- do.call(mgcv::gam, modelArgs)
                     out

                   },
                   predict = function(modelFit, newdata, submodels = NULL) {
                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                     if(modelFit$problemType == "Classification") {
                       probs <-  predict(modelFit, newdata, type = "response")
                       out <- ifelse(probs < .5,
                                     modelFit$obsLevel[1],
                                     modelFit$obsLevel[2])
                     } else {
                       out <- predict(modelFit, newdata, type = "response")
                     }
                     out
                   },
                   prob = function(modelFit, newdata, submodels = NULL){
                     if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                     out <- predict(modelFit, newdata, type = "response")
                     out <- cbind(1-out, out)
                     ## glm models the second factor level, we treat the first as the
                     ## event of interest. See Details in ?glm
                     colnames(out) <-  modelFit$obsLevels
                     out
                   },
                   predictors = function(x, ...) {
                     predictors(x$terms)
                   },
                   levels = function(x) x$obsLevels,
                   varImp = function(object, ...) {
                     smoothed <- summary(object)$s.table[, "p-value", drop = FALSE]
                     linear <- summary(object)$p.table
                     linear <- linear[, grepl("^Pr", colnames(linear)), drop = FALSE]
                     gams <- rbind(smoothed, linear)
                     gams <- gams[rownames(gams) != "(Intercept)",,drop = FALSE]
                     rownames(gams) <- gsub("^s\\(", "", rownames(gams))
                     rownames(gams) <- gsub("\\)$", "", rownames(gams))
                     colnames(gams)[1] <- "Overall"
                     gams <- as.data.frame(gams, stringsAsFactors = TRUE)
                     gams$Overall <- -log10(gams$Overall)
                     allPreds <- colnames(attr(object$terms,"factors"))
                     extras <- allPreds[!(allPreds %in% rownames(gams))]
                     if(any(extras)) {
                       tmp <- data.frame(Overall = rep(NA, length(extras)))
                       rownames(tmp) <- extras
                       gams <- rbind(gams, tmp)
                     }
                     gams
                   },
                   notes =
                     paste(
                       'Which terms enter the model in a nonlinear manner is determined',
                       'by the number of unique values for the predictor. For example,',
                       'if a predictor only has four unique values, most basis expansion',
                       'method will fail because there are not enough granularity in the',
                       'data. By default, a predictor must have at least 10 unique',
                       'values to be used in a nonlinear basis expansion.',
                       'Unlike other packages used by `train`, the `mgcv`',
                       'package is fully loaded when this model is used.'
                     ),
                   tags = c("Generalized Linear Model", "Generalized Additive Model"),
                   sort = function(x) x)


# Ranger ------------------------------------------------------------------

models$ranger <- list(label = "Random Forest",
                      library = "ranger",
                      check = function(pkg) {
                        requireNamespace("ranger")
                        current <- packageDescription("ranger")$Version
                        expected <- "0.8.0"
                        if(compareVersion(current, expected) < 0)
                          stop("This modeling workflow requires ranger version ",
                               expected, "or greater.", call. = FALSE)
                      },
                      loop = NULL,
                      type = c("Classification", "Regression"),
                      parameters = data.frame(parameter = c("mtry", "splitrule", "min.node.size"),
                                              class = c("numeric", "character", "numeric"),
                                              label = c("#Randomly Selected Predictors",
                                                        "Splitting Rule",
                                                        "Minimal Node Size")),
                      grid = function(x, y, len = NULL, search = "grid") {
                        if(search == "grid") {
                          srule <-
                            if (is.factor(y))
                              "gini"
                          else
                            "variance"
                          out <- expand.grid(mtry =
                                               caret::var_seq(p = ncol(x),
                                                              classification = is.factor(y),
                                                              len = len),
                                      min.node.size = ifelse( is.factor(y), 1, 5),
                                      splitrule = c(srule, "extratrees")[1:min(2,len)])
                 } else {
                   srules <- if (is.factor(y))
                     c("gini", "extratrees")
                   else
                     c("variance", "extratrees", "maxstat")
                   out <-
                     data.frame(
                       min.node.size= sample(1:(min(20,nrow(x))), size = len, replace = TRUE),
                       mtry = sample(1:ncol(x), size = len, replace = TRUE),
                       splitrule = sample(srules, size = len, replace = TRUE)
                     )
                 }
               },
               fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                 if((!is.data.frame(x))||dplyr::is.tbl(x)) x <- as.data.frame(x, stringsAsFactors = TRUE)
                 x$.outcome <- y
                 if(!is.null(wts)) {
                   out <- ranger::ranger(dependent.variable.name = ".outcome",
                                         data = x,
                                         mtry = min(param$mtry, ncol(x)),
                                         min.node.size = param$min.node.size,
                                         splitrule = as.character(param$splitrule),
                                         write.forest = TRUE,
                                         probability = classProbs,
                                         case.weights = wts,
                                         ...)
                 } else {
                   out <- ranger::ranger(dependent.variable.name = ".outcome",
                                         data = x,
                                         mtry = min(param$mtry, ncol(x)),
                                         min.node.size = param$min.node.size,
                                         splitrule = as.character(param$splitrule),
                                         write.forest = TRUE,
                                         probability = classProbs,
                                         ...)
                 }
                 ## in case the resampling method is "oob"
                 if(!last) out$y <- y
                 out
               },
               predict = function(modelFit, newdata, submodels = NULL) {
                 if((!is.data.frame(newdata))||dplyr::is.tbl(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                 out <- predict(modelFit, newdata)$predictions
                 if(!is.null(modelFit$obsLevels) & modelFit$treetype == "Probability estimation") {
                   out <- colnames(out)[apply(out, 1, which.max)]
                 }
                 out
               },
               prob = function(modelFit, newdata, submodels = NULL) {
                 if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
                 predict(modelFit, newdata)$predictions
               },
               predictors = function(x, ...) {
                 var_index <- sort(unique(unlist(lapply(x$forest$split.varIDs, function(x) x))))
                 var_index <-var_index[var_index > 0]
                 x$forest$independent.variable.names[var_index]
               },
               varImp = function(object, ...){
                 if(length(object$variable.importance) == 0)
                   stop("No importance values available")
                 imps <- ranger:::importance(object)
                 out <- data.frame(Overall = as.vector(imps))
                 rownames(out) <- names(imps)
                 out
               },
               levels = function(x) {
                 if(x$treetype == "Probability estimation") {
                   out <- colnames(x$predictions)
                 } else {
                   if(x$treetype == "Classification") {
                     out <- levels(x$predictions)
                   } else out <- NULL
                 }
                 out
               },
               oob = function(x) {
                 postResample(x$predictions, x$y)
               },
               tags = c("Random Forest", "Ensemble Model", "Bagging",
                        "Implicit Feature Selection", "Accepts Case Weights"),
               sort = function(x) x[order(x[,1]),])
