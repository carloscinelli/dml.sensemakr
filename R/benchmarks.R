##' Benchmarks for the strength of latent variables using observed covariates
##' @description
##' Compute benchmarks for the strength of latent variables, under the assumption that the gains in explanatory power due to latent variables is proportional to the gains of observed covariates.
##' @param model an object of class \code{\link{dml}}, fit with a single target
##'   (\code{"ate"}, \code{"att"}, or \code{"atu"}), conditional or unconditional.
##'   The benchmarking procedure is identical across these; only the results
##'   slot read from the model differs (\code{all}/\code{treat}/\code{untr}).
##' @param benchmark_covariates the observed covariates to benchmark. Either a
##'   character vector of column names in the model's \code{x} (each benchmarked
##'   on its own), or a named list where each element is a character vector of
##'   column names to drop \emph{together} in the leave-one-out refit (e.g. the
##'   dummy columns of a factor: \code{list(region = c("region3", "region4"))}).
##'   List element names become the row labels; unnamed elements are labelled by
##'   the column name (singletons) or the columns joined by \code{"+"}.
##' @returns An object of class \code{dml_benchmark} containing benchmark results.
##' @export
dml_benchmark <- function(model, benchmark_covariates){
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
                                 combine.method = "median", ...){
  print(summary(x, combine.method = combine.method), digits = digits, ...)
}

##' @param object an object of class \code{\link{dml_benchmark}}.
##' @param combine.method method to combine cross-fitting repetitions. Either
##'   \code{"median"} (default) or \code{"mean"}.
##' @param na.rm logical. Should NA values be removed? Default is \code{TRUE}.
##' @param ... arguments passed to other methods.
##' @returns For \code{print}: the object, printed to console. For \code{summary}:
##'   an object of class \code{summary_dml_benchmark} holding a table of the
##'   benchmark components -- the leave-one-out gains \code{gain.Y} and
##'   \code{gain.D}, the alignment \code{rho}, and the bias contribution
##'   \code{delta} -- each with a standard error derived from its influence
##'   function and combined across cross-fitting repetitions. \code{delta} is
##'   reported in the convention \code{delta = theta - theta_s}, so that
##'   \code{delta = -rho * M} for ATT and \code{delta = +rho * M} for ATU/ATE
##'   (with \code{M = sqrt(V.g * V.a)}), matching the OVB decomposition.
##' @rdname summary.dml_benchmark
##' @export
summary.dml_benchmark <- function(object, combine.method = c("median", "mean"),
                                  na.rm = TRUE, ...){
  combine.method <- match.arg(combine.method)
  combine <- if (combine.method == "mean") combine.mean else combine.median
  covars  <- names(object$benchmarks)

  rows <- lapply(covars, function(v) {
    est  <- object$benchmarks[[v]]        # per-rep point estimates (one row/rep)
    psis <- object$benchmarks_psis[[v]]   # per-rep influence functions
    reps <- seq_len(nrow(est))
    se_of <- function(psi.list) sapply(reps, function(i) psi.sd(psi.list[[i]]))
    # bias-contribution IF for delta = theta.s - theta.s,-j: psi(theta.s) - psi(theta.s,-j).
    # psi.sd is sign-invariant, so the stored (theta.s,-j - theta.s) order gives the same SE.
    se.delta <- sapply(reps, function(i)
      psi.sd(psis$psi.theta.s.wo[[i]] - psis$psi.theta.s[[i]]))
    cGY <- combine(est$gain.Y, se_of(psis$psi.GY))
    cGD <- combine(est$gain.D, se_of(psis$psi.GD))
    cRH <- combine(est$rho,    se_of(psis$psi.rho))
    cDL <- combine(est$delta,  se.delta)
    c(gain.Y = unname(cGY["estimate"]), se.gain.Y = unname(cGY["se"]),
      gain.D = unname(cGD["estimate"]), se.gain.D = unname(cGD["se"]),
      rho    = unname(cRH["estimate"]), se.rho    = unname(cRH["se"]),
      delta  = unname(cDL["estimate"]), se.delta  = unname(cDL["se"]))
  })

  tab <- do.call(rbind, rows)
  rownames(tab) <- covars
  out <- list(benchmarks = tab, combine.method = combine.method)
  class(out) <- "summary_dml_benchmark"
  out
}

##' @rdname summary.dml_benchmark
##' @export
print.summary_dml_benchmark <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  cat("Covariate benchmarks (leave-one-out):\n\n")
  print(round(x$benchmarks, digits))
  cat("\nCombined across cross-fitting repetitions using the",
      x$combine.method, "method.\n")
  invisible(x)
}

# bench_plm <- function(model, benchmark_covariates) {
#   # if (is.null(model$results$main[[slot]])) stop("Benchmarks implemented for ATE only. ATT/ATU coming soon.")
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
#   theta.short <- extract_estimate(model$results$main[[slot]], "theta.s")
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
#     theta.short.wo <- extract_estimate(model.wo$results$main[[slot]], "theta.s")
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


bench_fun <- function(model, benchmark_covariates){

  # The benchmarking procedure is identical for ATE/ATT/ATU and for conditional
  # or unconditional models; only the results slot read from the fitted model
  # differs (all = ATE, treat = ATT, untr = ATU). Select it from the model's
  # (single) target.
  slot <- unname(.target_to_slot[model$info$target])
  if (length(slot) != 1L || is.na(slot))
    stop("dml_benchmark() requires a model fit with a single target ",
         "('ate', 'att', or 'atu').")

  # Sign of the alignment rho in the OVB decomposition of delta = theta - theta_s:
  #   ATT:      delta = -rho * sqrt(V.g * V.a)      (Theorem 1, ATT)
  #   ATE/ATU:  delta = +rho * sqrt(V.g * V.a)      (Theorem 2 / Theorem 1, ATU)
  # The leave-one-out formula below (delta = -Bias, rho = sign(Bias)*|Bias|/sqrt)
  # returns the ATT-correct rho; flip it for the other targets so that rho is the
  # true error correlation for every target.
  align.sign <- if (slot == "treat") 1 else -1

  x <- model$data$x

  # Normalise benchmark_covariates into a named list of column-name groups. A
  # character vector benchmarks each column on its own; a named list lets a
  # group of columns (e.g. the dummies of a factor) be dropped together in the
  # leave-one-out refit. Element names become the row labels.
  covariate_groups <- if (is.list(benchmark_covariates)) benchmark_covariates
                      else as.list(benchmark_covariates)
  grp_names <- names(covariate_groups)
  if (is.null(grp_names)) grp_names <- rep("", length(covariate_groups))
  for (i in seq_along(covariate_groups)) {
    cols <- covariate_groups[[i]]
    if (!is.character(cols) || length(cols) < 1L)
      stop("Each entry of 'benchmark_covariates' must be a non-empty ",
           "character vector of column names.")
    if (is.na(grp_names[i]) || grp_names[i] == "")
      grp_names[i] <- if (length(cols) == 1L) cols else paste(cols, collapse = "+")
  }
  names(covariate_groups) <- grp_names

  all.cols  <- unlist(covariate_groups, use.names = FALSE)
  which.not <- which(!all.cols %in% colnames(x))
  if (length(which.not) > 0) {
    stop("Covariates not found: ", paste(all.cols[which.not], collapse = ", "), ".")
  }

  nu.sq <- extract_estimate(model$results$main[[slot]], param = "nu2.s")
  sigma.sq <- extract_estimate(model$results$main[[slot]], param = "sigma2.s")

  # resY  <- sapply(model$fits, function(x)model$data$y-x$preds$yhat)
  # R2.Y  <- apply(resY, 2, function(x) max(1-var(x)/var(model$data$y),0))

  theta.short <- extract_estimate(model$results$main[[slot]], "theta.s")

  # short IFs
  psi.theta.s  <- lapply(model$results$main[[slot]], function(x) x$psis$psi.theta.s)
  psi.sigma2.s <- lapply(model$results$main[[slot]], function(x) x$psis$psi.sigma2.s)
  psi.nu2.s    <- lapply(model$results$main[[slot]], function(x) x$psis$psi.nu2.s)

  benchmarks <- list()
  benchmarks_psis <- list()
  for (covar in names(covariate_groups)) {
    cols <- covariate_groups[[covar]]
    cat("\n=== Computing benchmarks using covariate:", covar, " ===\n\n")
    index.o <- which(colnames(x) %in% cols)   # drop all columns in the group
    xo <- x[, -index.o, drop = FALSE]
    model.call <- model$call
    model.call["x"] <- call("xo")
    model.wo <- eval(model.call)

    nu.sq.wo <- extract_estimate(model.wo$results$main[[slot]], param = "nu2.s")
    sigma.sq.wo <- extract_estimate(model.wo$results$main[[slot]], param = "sigma2.s")

    # resY.wo  <- sapply(model.wo$fits, function(x) model.wo$data$y - x$preds$yhat)
    # R2.Y.wo  <- apply(resY.wo, 2, function(x) max(1-var(x)/var(model.wo$data$y),0))

    ## (Debiased) Bias Decomposition
    theta.short.wo <- extract_estimate(model.wo$results$main[[slot]], "theta.s")

    # benchmark IFs
    psi.theta.s.wo  <- lapply(model.wo$results$main[[slot]], function(x) x$psis$psi.theta.s)
    psi.sigma2.s.wo <- lapply(model.wo$results$main[[slot]], function(x) x$psis$psi.sigma2.s)
    psi.nu2.s.wo    <- lapply(model.wo$results$main[[slot]], function(x) x$psis$psi.nu2.s)

    Bias <- theta.short.wo - theta.short

    # V.g <- apply(resY.wo,2,var) - apply(resY,2,var)
    V.g <- sigma.sq.wo - sigma.sq
    V.a <- nu.sq - nu.sq.wo

    valid <- V.g > 0 & V.a > 0
    Cor <- rep(0,length(valid))
    Cor[valid] <- (abs(Bias[valid])/sqrt(V.g[valid]*V.a[valid]))
    Cor <- pmin(1, Cor)
    Cor <- align.sign * Cor * sign(Bias)   # target-correct OVB alignment sign

    #(1- R^2_{a~a_s}) =  (Ea^2 - Ea_s^2)/ E a^2

    # Gain.Y = pmax(0, (R2.Y - R2.Y.wo)/(1 - R2.Y))
    Gain.Y = pmax(0, (sigma.sq.wo - sigma.sq)/sigma.sq)
    Gain.D = pmax(0, (nu.sq - nu.sq.wo)/nu.sq.wo)


    # delta is the OVB bias in the convention delta = theta - theta_s, i.e.
    #   delta = theta.short (full, has covariate) - theta.short.wo (leave-one-out) = -Bias.
    # With rho = align.sign * Bias / sqrt(V.g * V.a) (unclipped), this gives the
    # signed decomposition delta = -align.sign * rho * sqrt(V.g * V.a):
    #   ATT (align.sign = +1): delta = -rho * M   (Theorem 1, ATT)
    #   ATU/ATE (align.sign = -1): delta = +rho * M   (Theorem 1 ATU / Theorem 2)
    bench <- data.frame(gain.Y = Gain.Y,
                        gain.D = Gain.D,
                        rho = Cor,
                        theta.s  = theta.short,
                        theta.sj = theta.short.wo,
                        delta = -Bias)

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
    psi.rho <- lapply(psi.rho, function(p) align.sign * p)   # match Cor's sign

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
