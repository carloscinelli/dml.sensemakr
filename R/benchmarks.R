
##' @export
benchmark <- function(model, benchmark_covariates, cf.folds = NULL,
                      combine.method = "median"){
  model.type <- model$info$model
  bench_fun <- switch(model.type,
                      npm = bench_npm,
                      plm = bench_plm)
  bench_fun(model = model, benchmark_covariates = benchmark_covariates,
            cf.folds = cf.folds, combine.method = combine.method)
}

bench_plm <- function(model, benchmark_covariates, cf.folds = NULL,
                      combine.method = "median") {
  x <- model$data$x
  which.not <- which(!benchmark_covariates %in% colnames(x))
  if (any(which.not)){
    stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }
  comb_fun <- get(combine.method)

  resY.D   <- sapply(model$fits,
                     function(x) lm(model$data$y - x$preds$yhat ~ model$data$d - x$preds$dhat)$res)
  resD   <- sapply(model$fits, function(x) model$data$d - x$preds$dhat)
  R2.Y <- (apply(resY.D, 2, function(x) max(1-var(x)/var(model$data$y),0)))
  R2.D <- (apply(resD, 2, function(x) max(1-var(x)/var(model$data$d),0)))

  benchmarks <- list()
  for (i in seq_along(benchmark_covariates)){
    covar <- benchmark_covariates[i]
    cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
    index.o <- which(colnames(x) == covar)
    xo <- x[,-index.o]
    model.call <- model$call
    model.call["x"] <- call("xo")
    model.wo <- eval(model.call)

    resY.D.wo   <- sapply(model.wo$fits,
                       function(x) lm(model.wo$data$y - x$preds$yhat ~ model.wo$data$d - x$preds$dhat)$res)
    resD.wo   <- sapply(model.wo$fits, function(x) model.wo$data$d - x$preds$dhat)

    R2.Ywo <- (apply(resY.D.wo, 2, function(x) max(1-var(x)/var(model.wo$data$y),0)))
    R2.Dwo <- (apply(resD.wo, 2, function(x) max(1-var(x)/var(model.wo$data$d),0)))

    ## Bias Decomposition
    Bias <- extract_estimate(model.wo$results$main, "theta.s") -
      extract_estimate(model$results$main, "theta.s")
    V.g <- apply(resY.D.wo, 2, var) - apply(resY.D,2, var) # var( g - g_s)
    V.a <- apply(resD, 2, function(x) var(x/mean(x^2)))-
      apply(resD.wo, 2, function(x) var(x/mean(x^2))) # Var (a-a_s)
    Cor <- ifelse(V.g > 0 &  V.a > 0, abs(Bias)/sqrt(V.g*V.a), NA)

    #Gain metrics:
    Gain.Y <- pmax(0, (R2.Y-R2.Ywo)/(1-R2.Y))
    Gain.D <- pmax(0, (R2.D-R2.Dwo)/(1-R2.D))

    bench <- data.frame(covariate = covar,
                        gain.Y = comb_fun(Gain.Y),
                        gain.D =  comb_fun(Gain.D),
                        rho = comb_fun(Cor),
                        bias = comb_fun(Bias))

    benchmarks[[i]] = bench
  }

  do.call("rbind", benchmarks)
}



bench_npm <- function(model, benchmark_covariates, cf.folds = NULL,
                      combine.method = "median"){

  x <- model$data$x
  comb_fun <- get(combine.method)
  which.not <- which(!benchmark_covariates %in% colnames(x))

  if (any(which.not)){
    stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }

  nu.sq <- extract_estimate(model$results$main, param = "nu2.s")
  resY  <- sapply(model$fits, function(x)model$data$y-x$preds$yhat)
  R2.Y  <- apply(resY, 2, function(x) max(1-var(x)/var(model$data$y),0))

  theta.short <- extract_estimate(model$results$main, "theta.s")
  benchmarks <- list()
  for (i in seq_along(benchmark_covariates)) {
    covar <- benchmark_covariates[i]
    cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
    index.o <- which(colnames(x) == covar)
    xo <- x[,-index.o]
    model.call <- model$call
    model.call["x"] <- call("xo")
    model.wo <- eval(model.call)

    nu.sq.wo <- extract_estimate(model.wo$results$main, param = "nu2.s")
    resY.wo  <- sapply(model.wo$fits, function(x)model.wo$data$y-x$preds$yhat)
    R2.Y.wo  <- apply(resY.wo, 2, function(x) max(1-var(x)/var(model.wo$data$y),0))

    ## (Debiased) Bias Decomposition
    theta.short.wo <- extract_estimate(model.wo$results$main, "theta.s")
    Bias <- theta.short.wo - theta.short
    V.g <- apply(resY.wo,2,var) - apply(resY,2,var)
    V.a <- nu.sq - nu.sq.wo
    Cor <- ifelse(V.g > 0 &  V.a > 0, abs(Bias)/sqrt(V.g*V.a), NA)

    #(1- R^2_{a~a_s}) =  (Ea^2 - Ea_s^2)/ E a^2

    Gain.Y = (R2.Y - R2.Y.wo)/(1 - R2.Y);   #
    Gain.D = pmax(0, (nu.sq - nu.sq.wo)/nu.sq.wo)

     bench <- data.frame(covariate = covar,
                        gain.Y = comb_fun(Gain.Y),
                        gain.D =  comb_fun(Gain.D),
                        rho = comb_fun(Cor),
                        bias = comb_fun(Bias))

    benchmarks[[i]] = bench
  }
  do.call("rbind", benchmarks)
}
