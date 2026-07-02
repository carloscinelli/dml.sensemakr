##' Benchmarks for the strength of latent variables using observed covariates
##' @description
##' Compute benchmarks for the strength of latent variables, under the assumption that the gains in explanatory power due to latent variables is proportional to the gains of observed covariates.
##' @param model an object of class \code{\link{dml}}.
##' @param benchmark_covariates a character vector with the names of the observed covariates that will be used for benchmarking.
##' @param target character. The target parameter. Default is \code{"ate"}.
##' @returns An object of class \code{dml_benchmark} containing benchmark results.
##' @export
dml_benchmark <- function(model, benchmark_covariates, target = "ate"){
  model.type <- model$info$model
  # bench_fun <- switch(model.type,
  #                     npm = bench_npm,
  #                     plm = bench_plm)
  bench <- bench_fun(model = model, benchmark_covariates = benchmark_covariates)
  class(bench) <- "dml_benchmark"
  return(bench)
}

##' Print and summary methods for DML benchmarks
##' @description Print and summary methods for benchmark results.
##' @param x an object of class \code{\link{dml_benchmark}}.
##' @param digits minimal number of significant digits.
##' @rdname summary.dml_benchmark
##' @export
print.dml_benchmark <- function(x, digits = max(3L, getOption("digits") - 3L),
                                 combine.method = "mean", ...){
  print(summary(x), digits = digits, ...)
}

##' @param object an object of class \code{\link{dml_benchmark}}.
##' @param combine.method method to combine results. Default is \code{"mean"}.
##' @param na.rm logical. Should NA values be removed? Default is \code{TRUE}.
##' @param ... arguments passed to other methods.
##' @returns For \code{print}: the object, printed to console. For \code{summary}: the object with aggregated benchmarks.
##' @rdname summary.dml_benchmark
##' @export
summary.dml_benchmark <- function(object, combine.method = "mean", na.rm = TRUE, ...){
  comb_fun <- get(combine.method)
  out <- object
  out$benchmarks <- t(sapply(object$benchmarks, function(x) apply(x,2, comb_fun, na.rm = na.rm)))
}

# bench_plm <- function(model, benchmark_covariates) {
#   # if (is.null(model$results$main$all)) stop("Benchmarks implemented for ATE only. ATT/ATU coming soon.")
#   x <- model$data$x
#   which.not <- which(!benchmark_covariates %in% colnames(x))
#   if (any(which.not)){
#     stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
#   }
#
#   resY.D   <- sapply(model$fits,
#                      function(x) lm(model$data$y - x$preds$yhat ~ model$data$d - x$preds$dhat)$res)
#   resD   <- sapply(model$fits, function(x) model$data$d - x$preds$dhat)
#
#   R2.Y <- (apply(resY.D, 2, function(x) max(1-mean(x^2)/var(model$data$y),0)))
#   R2.D <- (apply(resD, 2, function(x) max(1-mean(x^2)/var(model$data$d),0)))
#
#   theta.short <- extract_estimate(model$results$main[[1]], "theta.s")
#   benchmarks <- list()
#   for (i in seq_along(benchmark_covariates)){
#     covar <- benchmark_covariates[i]
#     cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
#     index.o <- which(colnames(x) == covar)
#     xo <- x[,-index.o]
#     model.call <- model$call
#     model.call["x"] <- call("xo")
#     model.wo <- eval(model.call)
#
#     resY.D.wo   <- sapply(model.wo$fits,
#                        function(x) lm(model.wo$data$y - x$preds$yhat ~ model.wo$data$d - x$preds$dhat)$res)
#     resD.wo   <- sapply(model.wo$fits, function(x) model.wo$data$d - x$preds$dhat)
#
#     R2.Ywo <- (apply(resY.D.wo, 2, function(x) max(1-mean(x^2)/var(model.wo$data$y),0)))
#     R2.Dwo <- (apply(resD.wo, 2, function(x) max(1-mean(x^2)/var(model.wo$data$d),0)))
#
#     ## Bias Decomposition
#     theta.short.wo <- extract_estimate(model.wo$results$main[[1]], "theta.s")
#     Bias <-  theta.short.wo - theta.short
#     V.g <- apply(resY.D.wo, 2, function(x) mean(x^2)) -
#       apply(resY.D,2, function(x) mean(x^2)) # var( g - g_s)
#     V.a <- apply(resD, 2, function(x) mean((x/mean(x^2))^2))-
#       apply(resD.wo, 2, function(x) mean((x/mean(x^2))^2)) # Var (a-a_s)
#     valid <- V.g > 0 & V.a > 0
#     Cor <- 0
#     Cor[valid] <- (abs(Bias[valid])/sqrt(V.g[valid]*V.a[valid]))
#     Cor <- pmin(1, Cor)
#     Cor <- Cor*sign(Bias)
#
#     #Gain metrics:
#     Gain.Y <- pmax(0, (R2.Y-R2.Ywo)/(1-R2.Y))
#     Gain.D <- pmax(0, (R2.D-R2.Dwo)/(1-R2.D))
#
#     bench <- data.frame(gain.Y = Gain.Y,
#                         gain.D =  Gain.D,
#                         rho = Cor,
#                         theta.s  = theta.short,
#                         theta.sj = theta.short.wo,
#                         delta = Bias)
#
#     benchmarks[[covar]] = bench
#   }
#   return(benchmarks)
# }


##' Covariate strength table for conditional DML (DiD/ATT)
##' @description
##' For each benchmark covariate, fits a single-covariate conditional DML model
##' and an intercept-only (null) model, then computes four strength diagnostics:
##' imbalance (\code{chi_squared_root_divergence}), its SE, pre-trend
##' (\code{trend}), and its SE. These correspond to the columns of the
##' "Covariate Strength" table in the OVB-for-DiD manuscript.
##'
##' @param model a fitted \code{cond_dml} object (class \code{"dml"} with conditional
##'   parameters stored in \code{results$main$treat}).
##' @param benchmark_covariates character vector of covariate column names from
##'   \code{model$data$x} to benchmark one at a time.
##' @param med.fold logical. If \code{TRUE} (default), summarise across cross-fitting
##'   folds by picking the fold closest to the median of \code{theta.s}. If \code{FALSE},
##'   use fold 1.
##' @returns A \code{data.frame} with one row per covariate and columns:
##'   \code{variable}, \code{chi_squared_root_divergence},
##'   \code{chi_squared_root_se}, \code{trend}, \code{trend_se},
##'   \code{alignment}, \code{alignment_se}, \code{bias}, \code{bias_se}.
##' @export
cond_dml_strength <- function(model, benchmark_covariates, med.fold = TRUE) {

  x <- model$data$x
  which.not <- which(!benchmark_covariates %in% colnames(x))
  if (length(which.not) > 0) {
    stop("Covariates not found: ",
         paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }

  # helper: pick the fold index to summarise with
  pick_fold <- function(treat_results) {
    if (!med.fold) return(1L)
    theta.vec <- sapply(treat_results, function(r) r$estimates$theta.s)
    med <- stats::median(theta.vec)
    which.min(abs(theta.vec - med))
  }

  # helper: extract scalar estimates across cross-fitting folds, then pick fold
  get_est <- function(treat_results, param, k) {
    treat_results[[k]]$estimates[[param]]
  }
  get_psi <- function(treat_results, param, k) {
    treat_results[[k]]$psis[[param]]
  }

  # ----- 1. Fit the null / unconditional model (intercept only) ----------------
  x_null <- matrix(1, nrow = nrow(x), ncol = 1,
                   dimnames = list(NULL, "(Intercept)"))
  call_null <- model$call
  call_null["x"] <- call("x_null")
  cat("\n=== Fitting null (intercept-only) model ===\n\n")
  model_null <- eval(call_null)

  treat_null   <- model_null$results$main$treat
  k_null       <- pick_fold(treat_null)
  sigma2.uncond <- get_est(treat_null, "sigma2.s",  k_null)
  nu2.uncond    <- get_est(treat_null, "nu2.s",     k_null)
  theta.uncond  <- get_est(treat_null, "theta.s",   k_null)
  psi.theta.uncond  <- get_psi(treat_null, "psi.theta.s",  k_null)
  psi.sigma2.uncond <- get_psi(treat_null, "psi.sigma2.s", k_null)
  psi.nu2.uncond    <- get_psi(treat_null, "psi.nu2.s",    k_null)

  # ----- 2. Loop over each benchmark covariate ----------------------------------
  out_rows <- vector("list", length(benchmark_covariates))

  for (i in seq_along(benchmark_covariates)) {
    covar <- benchmark_covariates[i]
    cat("\n=== Fitting single-covariate model: ", covar, " ===\n\n")

    x_var      <- x[, covar, drop = FALSE]
    call_var   <- model$call
    call_var["x"] <- call("x_var")
    model_var  <- eval(call_var)

    treat_var  <- model_var$results$main$treat
    k_var      <- pick_fold(treat_var)

    nu2.var    <- get_est(treat_var, "nu2.s",    k_var)
    se.nu2.var <- get_est(treat_var, "se.nu2.s", k_var)
    sigma2.var <- get_est(treat_var, "sigma2.s", k_var)
    theta.var  <- get_est(treat_var, "theta.s",  k_var)

    psi.nu2.var    <- get_psi(treat_var, "psi.nu2.s",    k_var)
    psi.sigma2.var <- get_psi(treat_var, "psi.sigma2.s", k_var)
    psi.theta.var  <- get_psi(treat_var, "psi.theta.s",  k_var)

    # (1) Imbalance: sqrt(nu2 - 1)  [chi-squared divergence root]
    obs_imbalance    <- sqrt(max(0, nu2.var - 1))
    imbalance_se     <- if (obs_imbalance > 0) se.nu2.var / (2 * obs_imbalance) else NA_real_

    # (2) Pre-trend: sqrt(max(0, 1 - sigma2_var / sigma2_uncond))
    obs_trend_sq <- 1 - sigma2.var / sigma2.uncond
    obs_trend    <- sqrt(max(0, obs_trend_sq))

    # delta-method IF for sqrt(1 - sigma2_var / sigma2_uncond)
    # d/d(sigma2_uncond): sigma2_var / sigma2_uncond^2  * psi.sigma2_uncond
    # d/d(sigma2_var):   -1/sigma2_uncond               * psi.sigma2_var
    psi.trend.raw <- (sigma2.var / sigma2.uncond^2) * psi.sigma2.uncond -
      (1 / sigma2.uncond) * psi.sigma2.var
    psi.trend <- if (obs_trend > 0) psi.trend.raw * 0.5 / obs_trend else rep(0, length(psi.trend.raw))
    trend_se_i <- psi.sd(psi.trend)

    # (3) Alignment (correlation from bias decomposition)
    Bias  <- theta.uncond - theta.var
    V.g   <- sigma2.uncond - sigma2.var
    V.a   <- nu2.var - nu2.uncond
    valid <- V.g > 0 & V.a > 0

    Cor <- 0
    if (valid) {
      Cor <- pmin(1, abs(Bias) / sqrt(V.g * V.a)) * sign(Bias)
    }

    psi.rho <- if (!valid) {
      rep(0, nrow(x))
    } else {
      ((psi.theta.uncond - psi.theta.var) / sqrt(V.g * V.a)) -
        ((Bias * (psi.sigma2.uncond - psi.sigma2.var)) / (2 * (V.g^(3/2)) * sqrt(V.a))) -
        ((Bias * (psi.nu2.var - psi.nu2.uncond)) / (2 * (V.a^(3/2)) * sqrt(V.g)))
    }

    out_rows[[i]] <- data.frame(
      variable                 = covar,
      chi_squared_root_divergence = obs_imbalance,
      chi_squared_root_se      = imbalance_se,
      trend                    = obs_trend,
      trend_se                 = trend_se_i,
      alignment                = Cor,
      alignment_se             = psi.sd(psi.rho),
      bias                     = -Bias,
      bias_se                  = psi.sd(psi.theta.uncond - psi.theta.var),
      stringsAsFactors         = FALSE
    )
  }

  do.call(rbind, out_rows)
}

bench_fun <- function(model, benchmark_covariates){

  # if (is.null(model$results$main$all)) stop("Benchmarks implemented for ATE only. ATT/ATU coming soon.")
  x <- model$data$x
  which.not <- which(!benchmark_covariates %in% colnames(x))

  if (any(which.not)){
    stop("Covariates not found: ", paste(benchmark_covariates[which.not], collapse = ", "), ".")
  }

  nu.sq <- extract_estimate(model$results$main[[1]], param = "nu2.s")
  sigma.sq <- extract_estimate(model$results$main[[1]], param = "sigma2.s")

  # resY  <- sapply(model$fits, function(x)model$data$y-x$preds$yhat)
  # R2.Y  <- apply(resY, 2, function(x) max(1-var(x)/var(model$data$y),0))

  theta.short <- extract_estimate(model$results$main[[1]], "theta.s")

  # short IFs
  psi.theta.s  <- lapply(model$results$main[[1]], function(x) x$psis$psi.theta.s)
  psi.sigma2.s <- lapply(model$results$main[[1]], function(x) x$psis$psi.sigma2.s)
  psi.nu2.s    <- lapply(model$results$main[[1]], function(x) x$psis$psi.nu2.s)

  benchmarks <- list()
  benchmarks_psis <- list()
  for (i in seq_along(benchmark_covariates)) {
    covar <- benchmark_covariates[i]
    cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
    index.o <- which(colnames(x) == covar)
    xo <- x[,-index.o]
    model.call <- model$call
    model.call["x"] <- call("xo")
    model.wo <- eval(model.call)

    nu.sq.wo <- extract_estimate(model.wo$results$main[[1]], param = "nu2.s")
    sigma.sq.wo <- extract_estimate(model.wo$results$main[[1]], param = "sigma2.s")

    # resY.wo  <- sapply(model.wo$fits, function(x) model.wo$data$y - x$preds$yhat)
    # R2.Y.wo  <- apply(resY.wo, 2, function(x) max(1-var(x)/var(model.wo$data$y),0))

    ## (Debiased) Bias Decomposition
    theta.short.wo <- extract_estimate(model.wo$results$main[[1]], "theta.s")

    # benchmark IFs
    psi.theta.s.wo  <- lapply(model.wo$results$main[[1]], function(x) x$psis$psi.theta.s)
    psi.sigma2.s.wo <- lapply(model.wo$results$main[[1]], function(x) x$psis$psi.sigma2.s)
    psi.nu2.s.wo    <- lapply(model.wo$results$main[[1]], function(x) x$psis$psi.nu2.s)

    Bias <- theta.short.wo - theta.short

    # V.g <- apply(resY.wo,2,var) - apply(resY,2,var)
    V.g <- sigma.sq.wo - sigma.sq
    V.a <- nu.sq - nu.sq.wo

    valid <- V.g > 0 & V.a > 0
    Cor <- rep(0,length(valid))
    Cor[valid] <- (abs(Bias[valid])/sqrt(V.g[valid]*V.a[valid]))
    Cor <- pmin(1, Cor)
    Cor <- Cor*sign(Bias)

    #(1- R^2_{a~a_s}) =  (Ea^2 - Ea_s^2)/ E a^2

    # Gain.Y = pmax(0, (R2.Y - R2.Y.wo)/(1 - R2.Y))
    Gain.Y = pmax(0, (sigma.sq.wo - sigma.sq)/sigma.sq)
    Gain.D = pmax(0, (nu.sq - nu.sq.wo)/nu.sq.wo)


    bench <- data.frame(gain.Y = Gain.Y,
                        gain.D = Gain.D,
                        rho = Cor,
                        theta.s  = theta.short,
                        theta.sj = theta.short.wo,
                        delta = Bias)

    benchmarks[[covar]] = bench

    ## bounds IFs
    psi.GY <- Map(function(psi.wo, psi, s, s.wo) {
      (s * psi.wo - s.wo * psi) / (s^2)
    }, psi.sigma2.s.wo, psi.sigma2.s, sigma.sq, sigma.sq.wo)

    psi.GD <- Map(function(psi.wo, psi, nu, nu.wo) {
      (nu.wo * psi - nu * psi.wo) / (nu.wo^2)
    }, psi.nu2.s.wo, psi.nu2.s, nu.sq, nu.sq.wo)

    psi.rho <- lapply(seq_len(length(valid)), function(k) {
      if (!valid[k]) {
        rep(0, nrow(x))
      } else {
        ((psi.theta.s.wo[[k]] - psi.theta.s[[k]]) /
           (sqrt(V.g[k] * V.a[k]))) -
          ((Bias[k] * (psi.sigma2.s.wo[[k]] - psi.sigma2.s[[k]])) /
             (2 * (V.g[k]^(3/2)) * sqrt(V.a[k]))) -
          ((Bias[k] * (psi.nu2.s[[k]] - psi.nu2.s.wo[[k]])) /
             (2 * (V.a[k]^(3/2)) * sqrt(V.g[k])))
      }
    })

    benchmarks_psis[[covar]] = list(
      psi.theta.s  = psi.theta.s,
      psi.sigma2.s = psi.sigma2.s,
      psi.nu2.s    = psi.nu2.s,

      psi.theta.s.wo  = psi.theta.s.wo,
      psi.sigma2.s.wo = psi.sigma2.s.wo,
      psi.nu2.s.wo    = psi.nu2.s.wo,

      psi.GY  = psi.GY,
      psi.GD  = psi.GD,
      psi.rho = psi.rho
    )
  }
  return(list(
    benchmarks      = benchmarks,
    benchmarks_psis = benchmarks_psis))
}
