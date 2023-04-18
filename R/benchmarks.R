##' Benchmarks for the strength of latent variables using observed covariates
##' @description
##' Compute benchmarks for the strength of latent variables, under the assumption that the gains in explanatory power due to latent variables is proportional to the gains of observed covariates.
##' @param model an object of class \code{\link{dml}}.
##' @param benchmark_covariates a character vector with the names of the observed covariates that will be used for benchmarking.
##' @export
dml_benchmark <- function(model, benchmark_covariates, target = "ate"){
  model.type <- model$info$model
  bench_fun <- switch(model.type,
                      npm = bench_npm,
                      plm = bench_plm)
  bench <- bench_fun(model = model, benchmark_covariates = benchmark_covariates)
  class(bench) <- "dml_benchmark"
  return(bench)
}

##' @param x an object of class \code{\link{dml_benchmark}}.
##' @param digits minimal number of significant digits.
##' @rdname summary.dml_benchmark
##' @export
print.dml_benchmark <- function(x, digits = max(3L, getOption("digits") - 3L),
                                 combine.method = "mean", ...){
  print(summary(x), digits = digits, ...)
}

##' @param object an object of class \code{\link{dml_benchmark}}.
##' @rdname summary.dml_benchmark
##' @export
summary.dml_benchmark <- function(object, combine.method = "mean", na.rm = T, ...){
  comb_fun <- get(combine.method)
  out <- t(sapply(object, function(x) apply(x,2, comb_fun, na.rm = na.rm)))
}



bench_plm <- function(model, benchmark_covariates) {
  if (is.null(model$results$main$all)) stop("Benchmarks implemented for ATE only. ATT/ATU coming soon.")
  x <- model$data$x
  which.not <- which(!benchmark_covariates %in% colnames(x))
  if (any(which.not)){
    stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }

  resY.D   <- sapply(model$fits,
                     function(x) lm(model$data$y - x$preds$yhat ~ model$data$d - x$preds$dhat)$res)
  resD   <- sapply(model$fits, function(x) model$data$d - x$preds$dhat)
  R2.Y <- (apply(resY.D, 2, function(x) max(1-var(x)/var(model$data$y),0)))
  R2.D <- (apply(resD, 2, function(x) max(1-var(x)/var(model$data$d),0)))
  theta.short <- extract_estimate(model$results$main$all, "theta.s")
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
    theta.short.wo <- extract_estimate(model.wo$results$main$all, "theta.s")
    Bias <-  theta.short.wo -theta.short
    V.g <- apply(resY.D.wo, 2, var) - apply(resY.D,2, var) # var( g - g_s)
    V.a <- apply(resD, 2, function(x) var(x/mean(x^2)))-
      apply(resD.wo, 2, function(x) var(x/mean(x^2))) # Var (a-a_s)
    valid <- V.g > 0 & V.a > 0
    Cor <- 0
    Cor[valid] <- (abs(Bias[valid])/sqrt(V.g[valid]*V.a[valid]))
    Cor <- pmin(1, Cor)
    Cor <- Cor*sign(Bias)

    #Gain metrics:
    Gain.Y <- pmax(0, (R2.Y-R2.Ywo)/(1-R2.Y))
    Gain.D <- pmax(0, (R2.D-R2.Dwo)/(1-R2.D))

    bench <- data.frame(gain.Y = Gain.Y,
                        gain.D =  Gain.D,
                        rho = Cor,
                        theta.s  = theta.short,
                        theta.sj = theta.short.wo,
                        delta = Bias)

    benchmarks[[covar]] = bench
  }
  return(benchmarks)
}



bench_npm <- function(model, benchmark_covariates){

  if (is.null(model$results$main$all)) stop("Benchmarks implemented for ATE only. ATT/ATU coming soon.")
  x <- model$data$x
  which.not <- which(!benchmark_covariates %in% colnames(x))

  if (any(which.not)){
    stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }

  nu.sq <- extract_estimate(model$results$main$all, param = "nu2.s")
  resY  <- sapply(model$fits, function(x)model$data$y-x$preds$yhat)
  R2.Y  <- apply(resY, 2, function(x) max(1-var(x)/var(model$data$y),0))

  theta.short <- extract_estimate(model$results$main$all, "theta.s")
  benchmarks <- list()
  for (i in seq_along(benchmark_covariates)) {
    covar <- benchmark_covariates[i]
    cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
    index.o <- which(colnames(x) == covar)
    xo <- x[,-index.o]
    model.call <- model$call
    model.call["x"] <- call("xo")
    model.wo <- eval(model.call)

    nu.sq.wo <- extract_estimate(model.wo$results$main$all, param = "nu2.s")
    resY.wo  <- sapply(model.wo$fits, function(x) model.wo$data$y - x$preds$yhat)
    R2.Y.wo  <- apply(resY.wo, 2, function(x) max(1-var(x)/var(model.wo$data$y),0))

    ## (Debiased) Bias Decomposition
    theta.short.wo <- extract_estimate(model.wo$results$main$all, "theta.s")
    Bias <- theta.short.wo - theta.short
    V.g <- apply(resY.wo,2,var) - apply(resY,2,var)
    V.a <- nu.sq - nu.sq.wo
    valid <- V.g > 0 & V.a > 0
    Cor <- 0
    Cor[valid] <- (abs(Bias[valid])/sqrt(V.g[valid]*V.a[valid]))*sign(Bias[valid])

    #(1- R^2_{a~a_s}) =  (Ea^2 - Ea_s^2)/ E a^2

    Gain.Y = pmax(0, (R2.Y - R2.Y.wo)/(1 - R2.Y))   #
    Gain.D = pmax(0, (nu.sq - nu.sq.wo)/nu.sq.wo)

     bench <- data.frame(gain.Y = Gain.Y,
                         gain.D = Gain.D,
                         rho = Cor,
                         theta.s  = theta.short,
                         theta.sj = theta.short.wo,
                         delta = Bias)

    benchmarks[[covar]] = bench
  }
  return(benchmarks)
}
