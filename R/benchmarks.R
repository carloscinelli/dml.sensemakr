##' Benchmarks for the strength of latent variables using observed covariates
##' @description
##' Compute benchmarks for the strength of latent variables, under the assumption that the gains in explanatory power due to latent variables is proportional to the gains of observed covariates.
##' @param model an object of class \code{\link{dml}}.
##' @param benchmark_covariates the observed covariates to benchmark. Either a
##'   character vector of column names in the model's \code{x} (each benchmarked
##'   on its own), or a named list where each element is a character vector of
##'   column names to drop \emph{together} in the refit (e.g. the
##'   dummy columns of a factor: \code{list(region = c("region3", "region4"))}).
##'   List element names become the row labels; unnamed elements are labelled by
##'   the column name (singletons) or the columns joined by \code{"+"}.
##' @param dreg,yreg optional learner specifications (as in \code{\link{dml}}) for
##'   the \emph{refit}, overriding those stored in \code{model$call}.
##' @returns An object of class \code{dml_benchmark} containing benchmark results.
##' @export
dml_benchmark <- function(model, benchmark_covariates, dreg = NULL, yreg = NULL){
  bench <- bench_fun(model = model, benchmark_covariates = benchmark_covariates,
                     dreg = dreg, yreg = yreg)
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
##'   benchmark components -- the gains \code{gain.Y} and
##'   \code{gain.D}, the alignment \code{rho}, and the change in estimate
##'   \code{delta} -- each with a standard error derived from its influence
##'   function and combined across cross-fitting repetitions.
##' @rdname summary.dml_benchmark
##' @export
summary.dml_benchmark <- function(object, combine.method = c("median", "mean"),
                                  na.rm = TRUE, ...){
  combine.method <- match.arg(combine.method)
  combine.base <- if (combine.method == "mean") combine.mean else combine.median
  combine <- function(est, se) combine.base(est, se, na.rm = na.rm)
  covars  <- names(object$benchmarks)

  rows <- lapply(covars, function(v) {
    est  <- object$benchmarks[[v]]        # per-rep point estimates (one row/rep)
    psis <- object$benchmarks_psis[[v]]   # per-rep influence functions
    reps <- seq_len(nrow(est))
    se_of <- function(psi.list) sapply(reps, function(i) psi.sd(psi.list[[i]]))
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
  cat("Covariate benchmarks:\n\n")
  print(round(x$benchmarks, digits))
  cat("\nCombined across cross-fitting repetitions using the",
      x$combine.method, "method.\n")
  invisible(x)
}

##' Confidence bounds for covariate benchmarking (propagating benchmark uncertainty)
##'
##' @description
##' Constructs the bias-adjusted bounds on the target parameter implied by
##' benchmarking against observed covariates, together with one-sided confidence
##' bounds that \emph{propagate the estimation uncertainty of the benchmarked
##' gains}. Both the fixed-gain and the propagated bounds are
##' returned so the two can be compared.
##'
##'
##' @param model the \code{\link{dml}} fit used for the benchmark. Must be fit with a single target (\code{"ate"}/\code{"att"}/\code{"atu"}).
##' @param benchmark either a \code{dml_benchmark} object (from
##'   \code{\link{dml_benchmark}}, re-used \emph{without} refitting) or the
##'   \code{benchmark_covariates} to pass to \code{\link{dml_benchmark}}.
##' @param kY,kD relative-strength multipliers \eqn{k_{Y,j}}, \eqn{k_{D,j}} of the
##'   latent-vs-observed gains. Either a single number or a vector with one entry
##'   per benchmark covariate (in the order of \code{benchmark}). Default \code{1}
##'   (a latent confounder as strong as the observed covariate).
##' @param rho2 optional fixed squared alignment \eqn{\rho^2} to use in place of
##'   the benchmarked \eqn{\hat\rho_j}. \code{NULL} (default) uses covariate
##'   \eqn{j}'s estimated (signed) alignment and, in the propagated CI, its
##'   sampling uncertainty. A value in \eqn{[0, 1]}.
##' @param level confidence level for the one-sided bounds. Default \code{0.95}
##'   (critical value \eqn{\Phi^{-1}(level)}).
##' @param combine.method how to combine cross-fitting repetitions,
##'   \code{"median"} (default) or \code{"mean"}. Each repetition's bound and its
##'   influence-function SE are combined with the same rule as
##'   \code{\link{confidence_bounds}}, so the reported SE folds in the
##'   across-repetition spread of the estimate.
##' @param na.rm logical. Should repetitions with a missing benchmark gain be
##'   dropped before the repetitions are combined? Default is \code{TRUE}. With
##'   \code{FALSE}, a single failed repetition makes the combined gain
##'   \code{NA}.
##' @returns An object of class \code{dml_benchmark_bounds} (a data frame, one row
##'   per benchmark covariate) with the bias factor \code{BF}, the point bounds
##'   \code{theta.minus}/\code{theta.plus}, the fixed-gain confidence bounds
##'   \code{lwr.fixed}/\code{upr.fixed} (\eqn{=}\code{confidence_bounds}), the
##'   uncertainty-propagated confidence bounds \code{lwr}/\code{upr}, and the
##'   propagated bound SEs \code{se.minus}/\code{se.plus}. Covariates whose implied
##'   \eqn{k_D \hat G_{D,j} \ge 1} (\code{cf.d} reaches 1, so the bound diverges)
##'   return \code{-Inf}/\code{Inf} bounds with a warning.
##' @seealso \code{\link{dml_benchmark}}, \code{\link{confidence_bounds}}.
##' @references
##'   Chernozhukov, V., Cinelli, C., Newey, W., Sharma, A., and Syrgkanis, V.
##'   (2026). "Long Story Short: Omitted Variable Bias in Causal Machine Learning."
##'   \emph{Review of Economics and Statistics} (Appendix E).
##'
##'   Wang, J., Sant'Anna, P. H. C., Chernozhukov, V., and Cinelli, C. (2026).
##'   "Omitted Variable Bias in Difference-in-Differences Designs." Working Paper.
##'   (Benchmarking for the conditional ATT/ATU.)
##' @export
benchmark_bounds <- function(model, benchmark, kY = 1, kD = 1, rho2 = NULL,
                             level = 0.95, combine.method = c("median", "mean"),
                             na.rm = TRUE) {
  combine.method <- match.arg(combine.method)
  if (!is.null(rho2) && (length(rho2) != 1L || !is.finite(rho2) || rho2 < 0 || rho2 > 1))
    stop("`rho2` must be NULL or a single value in [0, 1].")
  rho2.fixed <- !is.null(rho2)
  cmb <- function(v) if (combine.method == "median") stats::median(v, na.rm = na.rm) else mean(v, na.rm = na.rm)

  bench <- if (inherits(benchmark, "dml_benchmark")) benchmark
           else dml_benchmark(model, benchmark)
  if (!inherits(bench, "dml_benchmark"))
    stop("`benchmark` must be a dml_benchmark object or benchmark_covariates.")

  slot <- unname(.target_to_slot[model$info$target])
  if (length(slot) != 1L || is.na(slot) || is.null(model$results$main[[slot]]))
    stop("benchmark_bounds() requires a `model` fit with a single target ",
         "('ate', 'att', or 'atu') matching the benchmark.")
  post <- model$results$main[[slot]]

  # ---- per-rep scale components; repetitions combined like confidence_bounds()
  sigma2.s.rep <- extract_estimate(post, "sigma2.s")
  nu2.s.rep    <- extract_estimate(post, "nu2.s")
  theta.s.rep  <- extract_estimate(post, "theta.s")
  R       <- length(theta.s.rep)                          # cross-fitting repetitions
  combine <- function(est, se) {
    if (combine.method == "mean") combine.mean(est, se, na.rm = na.rm)
    else combine.median(est, se, na.rm = na.rm)
  }
  level[level < 0.5] <- 0.5              # one-sided; same clamp as confidence_bounds()
  z       <- stats::qnorm(level)

  diverged <- character(0)
  unmeasured <- character(0)
  covs <- names(bench$benchmarks)
  if (!is.numeric(kY) || !(length(kY) %in% c(1L, length(covs))) || anyNA(kY) || any(kY < 0))
    stop("`kY` must be a single non-negative number, or one per benchmark covariate (",
         length(covs), " here).")
  if (!is.numeric(kD) || !(length(kD) %in% c(1L, length(covs))) || anyNA(kD) || any(kD < 0))
    stop("`kD` must be a single non-negative number, or one per benchmark covariate (",
         length(covs), " here).")
  kY.j <- rep_len(kY, length(covs))
  kD.j <- rep_len(kD, length(covs))
  rows <- lapply(seq_along(covs), function(j) {
    v    <- covs[j]
    kY   <- kY.j[j]
    kD   <- kD.j[j]
    est  <- bench$benchmarks[[v]]
    psis <- bench$benchmarks_psis[[v]]

    # combined benchmark gains -> the (fixed) bias factor, as in confidence_bounds()
    GY <- cmb(est$gain.Y); GD <- cmb(est$gain.D)
    rho.use <- if (rho2.fixed) sqrt(rho2) else cmb(est$rho)   # fixed |rho| = sqrt(rho2), or benchmarked (signed)
    kGY <- kY * GY; kGD <- kD * GD
    na.row <- data.frame(BF = NA_real_, theta.minus = NA_real_, theta.plus = NA_real_,
                         lwr.fixed = NA_real_, upr.fixed = NA_real_,
                         lwr = NA_real_, upr = NA_real_,
                         se.minus = NA_real_, se.plus = NA_real_, row.names = v)
    if (!is.finite(GY) || !is.finite(GD) || !is.finite(rho.use)) {
      # no gain could be measured at all -- not the same as a measured gain of
      # zero, so do not fall into the gain.zero branch below
      unmeasured <<- c(unmeasured, v)
      return(na.row)
    }
    if (is.finite(kGD) && kGD >= 1) {
      # the bound genuinely diverges: the interval is the whole real line.
      # Report that as -Inf/Inf, distinct from the NA of an unmeasured gain.
      diverged <<- c(diverged, v)
      return(data.frame(BF = Inf, theta.minus = -Inf, theta.plus = Inf,
                        lwr.fixed = -Inf, upr.fixed = Inf,
                        lwr = -Inf, upr = Inf,
                        se.minus = NA_real_, se.plus = NA_real_, row.names = v))
    }
    gain.zero <- GY <= 0 || GD <= 0 || kGY <= 0 || kGD <= 0
    if (gain.zero) { CY <- CD <- BF <- 0 } else {
      CY <- sqrt(kGY); CD <- sqrt(kGD / (1 - kGD)); BF <- abs(rho.use) * CY * CD
    }

    # per-rep bound endpoints and their influence-function SEs
    tm <- tp <- sm <- sp <- sm.fx <- sp.fx <- numeric(R)
    for (k in seq_len(R)) {
      s2 <- sigma2.s.rep[k]; n2 <- nu2.s.rep[k]; th <- theta.s.rep[k]
      S.k    <- sqrt(s2 * n2)
      psi.th <- psis$psi.theta.s[[k]]
      psi.S  <- (s2 * psis$psi.nu2.s[[k]] + n2 * psis$psi.sigma2.s[[k]]) / (2 * S.k)
      psi.BF <- if (gain.zero) numeric(length(psi.th)) else {
        # gain-uncertainty terms (always propagated); rho term only when rho is estimated
        gains <- abs(rho.use) * CD * (kY * psis$psi.GY[[k]]) / (2 * CY) +
                 abs(rho.use) * CY * (kD * psis$psi.GD[[k]]) / (2 * sqrt(kGD) * (1 - kGD)^(3/2))
        if (rho2.fixed) gains else gains + sign(rho.use) * CY * CD * psis$psi.rho[[k]]
      }
      tm[k] <- th - BF * S.k;                     tp[k] <- th + BF * S.k
      sm[k] <- psi.sd(psi.th - BF * psi.S - S.k * psi.BF)   # full-propagation SE
      sp[k] <- psi.sd(psi.th + BF * psi.S + S.k * psi.BF)
      sm.fx[k] <- psi.sd(psi.th - BF * psi.S)               # fixed-gain SE
      sp.fx[k] <- psi.sd(psi.th + BF * psi.S)
    }

    # combine reps: estimate + an SE that folds in the across-rep spread (CCDDHNR)
    cm <- combine(tm, sm);       cp <- combine(tp, sp)
    cm.fx <- combine(tm, sm.fx); cp.fx <- combine(tp, sp.fx)
    data.frame(
      BF = BF,
      theta.minus = unname(cm["estimate"]), theta.plus = unname(cp["estimate"]),
      lwr.fixed = unname(cm.fx["estimate"] - z * cm.fx["se"]),
      upr.fixed = unname(cp.fx["estimate"] + z * cp.fx["se"]),
      lwr = unname(cm["estimate"] - z * cm["se"]),
      upr = unname(cp["estimate"] + z * cp["se"]),
      se.minus = unname(cm["se"]), se.plus = unname(cp["se"]), row.names = v)
  })
  if (length(unmeasured))
    warning("The combined benchmark gain is missing for: ",
            paste(unmeasured, collapse = ", "),
            " -- NA is returned, which is not the same as a measured gain of ",
            "zero. With na.rm = TRUE this means every cross-fitting repetition ",
            "returned NA; with na.rm = FALSE a single one is enough.",
            call. = FALSE)
  if (length(diverged))
    warning("k_D * gain.D >= 1 for: ", paste(diverged, collapse = ", "),
            " -- the implied cf.d reaches 1, so latent variables would account ",
            "for all the variation in the Riesz representer and the bound ",
            "diverges (-Inf/Inf returned). Lower kD for a finite bound.")

  out <- do.call(rbind, rows)
  attr(out, "kY") <- kY; attr(out, "kD") <- kD; attr(out, "rho2") <- rho2
  attr(out, "level") <- level; attr(out, "combine.method") <- combine.method
  attr(out, "theta.s") <- cmb(theta.s.rep)
  attr(out, "S") <- sqrt(cmb(sigma2.s.rep) * cmb(nu2.s.rep))
  class(out) <- c("dml_benchmark_bounds", "data.frame")
  out
}

##' @param x an object of class \code{dml_benchmark_bounds}.
##' @param digits number of digits to print.
##' @param ... ignored.
##' @rdname benchmark_bounds
##' @export
print.dml_benchmark_bounds <- function(x, digits = 4, ...) {
  fmt <- function(v) formatC(v, digits = digits, format = "f")
  rho.txt <- if (is.null(attr(x, "rho2"))) "rho2 = benchmarked" else
             paste0("rho2 = ", attr(x, "rho2"), " (fixed)")
  cat("Covariate benchmark bounds  (kY = ", paste(attr(x, "kY"), collapse = ", "),
      ", kD = ", paste(attr(x, "kD"), collapse = ", "),
      ", ", rho.txt, ";  ", format(100 * attr(x, "level")), "% one-sided)\n", sep = "")
  cat("theta.s = ", fmt(attr(x, "theta.s")), ",  S = ", fmt(attr(x, "S")),
      "\n\n", sep = "")
  cols <- c("BF", "theta.minus", "theta.plus", "lwr.fixed", "upr.fixed", "lwr", "upr")
  print(round(as.data.frame(x)[, cols], digits))
  cat("\n [theta.minus, theta.plus] point estimates of the bounds\n")
  cat(" [lwr.fixed, upr.fixed]    confidence bounds treating benchmark as fixed\n")
  cat(" [lwr, upr]                confidence bounds accounting for benchmark uncertainty\n")
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


bench_fun <- function(model, benchmark_covariates, dreg = NULL, yreg = NULL){

  # The benchmarking procedure is identical for ATE/ATT/ATU and for conditional
  # or unconditional models; only the results slot read from the fitted model
  # differs (all = ATE, treat = ATT, untr = ATU). Select it from the model's
  # (single) target.
  slot <- unname(.target_to_slot[model$info$target])
  if (length(slot) != 1L || is.na(slot))
    stop("dml_benchmark() requires a model fit with a single target ",
         "('ate', 'att', or 'atu').")

  # Sign that maps the shift delta = theta.s - theta.s,-j to the
  # reported alignment rho. For the ATE, ATU and the unconditional ATT the
  # decomposition is delta = +rho * M, so rho = delta / M (align.sign = -1).
  # The conditional (single-arm / DiD-style) ATT imputes the subtracted
  # counterfactual, mirroring the conditional ATU: delta = -rho * M there, so
  # its reported rho keeps the opposite sign (align.sign = +1).
  align.sign <- if (slot == "treat" && isTRUE(model$info$conditional)) 1 else -1

  x <- model$data$x

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
    model.call[["x"]] <- quote(xo)
    if (!is.null(dreg)) model.call[["dreg"]] <- dreg   # pin refit treatment learner
    if (!is.null(yreg)) model.call[["yreg"]] <- yreg   # pin refit outcome learner
    # evaluate the stored call where the user originally made it (with the
    # reduced covariate matrix spliced in), so their variables -- including any
    # named `x`, `yreg`, or `dreg` -- resolve to their objects, not our locals
    eval.env <- new.env(parent = if (is.environment(model$call.env))
                                   model$call.env else globalenv())
    eval.env$xo <- xo
    model.wo <- eval(model.call, eval.env)

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
