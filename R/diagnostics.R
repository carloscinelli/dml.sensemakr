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

  get_est <- function(treat_results, param, k) treat_results[[k]]$estimates[[param]]
  get_psi <- function(treat_results, param, k) treat_results[[k]]$psis[[param]]

  # ----- 1. Fit the null / unconditional model (intercept only) ----------------
  x_null <- matrix(1, nrow = nrow(x), ncol = 1,
                   dimnames = list(NULL, "(Intercept)"))
  call_null        <- model$call
  call_null["x"]   <- call("x_null")
  cat("\n=== Fitting null (intercept-only) model ===\n\n")
  model_null <- eval(call_null)

  treat_null        <- model_null$results$main$treat
  k_null            <- pick_fold(treat_null)
  sigma2.uncond     <- get_est(treat_null, "sigma2.s",     k_null)
  nu2.uncond        <- get_est(treat_null, "nu2.s",        k_null)
  theta.uncond      <- get_est(treat_null, "theta.s",      k_null)
  psi.theta.uncond  <- get_psi(treat_null, "psi.theta.s",  k_null)
  psi.sigma2.uncond <- get_psi(treat_null, "psi.sigma2.s", k_null)
  psi.nu2.uncond    <- get_psi(treat_null, "psi.nu2.s",    k_null)

  # ----- 2. Loop over each benchmark covariate ----------------------------------
  out_rows <- vector("list", length(benchmark_covariates))

  for (i in seq_along(benchmark_covariates)) {
    covar <- benchmark_covariates[i]
    cat("\n=== Fitting single-covariate model:", covar, "===\n\n")

    x_var          <- x[, covar, drop = FALSE]
    call_var       <- model$call
    call_var["x"]  <- call("x_var")
    model_var      <- eval(call_var)

    treat_var      <- model_var$results$main$treat
    k_var          <- pick_fold(treat_var)

    nu2.var        <- get_est(treat_var, "nu2.s",    k_var)
    se.nu2.var     <- get_est(treat_var, "se.nu2.s", k_var)
    sigma2.var     <- get_est(treat_var, "sigma2.s", k_var)
    theta.var      <- get_est(treat_var, "theta.s",  k_var)

    psi.nu2.var    <- get_psi(treat_var, "psi.nu2.s",    k_var)
    psi.sigma2.var <- get_psi(treat_var, "psi.sigma2.s", k_var)
    psi.theta.var  <- get_psi(treat_var, "psi.theta.s",  k_var)

    # (1) Imbalance: sqrt(nu2 - 1)  [chi-squared divergence root]
    obs_imbalance <- sqrt(max(0, nu2.var - 1))
    imbalance_se  <- if (obs_imbalance > 0) se.nu2.var / (2 * obs_imbalance) else NA_real_

    # (2) Pre-trend: sqrt(max(0, 1 - sigma2_var / sigma2_uncond))
    obs_trend_sq <- 1 - sigma2.var / sigma2.uncond
    obs_trend    <- sqrt(max(0, obs_trend_sq))

    # delta-method IF for obs_trend
    psi.trend.raw <- (sigma2.var / sigma2.uncond^2) * psi.sigma2.uncond -
      (1 / sigma2.uncond) * psi.sigma2.var
    psi.trend  <- if (obs_trend > 0) psi.trend.raw * 0.5 / obs_trend else rep(0, length(psi.trend.raw))
    trend_se_i <- psi.sd(psi.trend)

    # (3) Alignment (correlation from bias decomposition)
    Bias  <- theta.uncond - theta.var
    V.g   <- sigma2.uncond - sigma2.var
    V.a   <- nu2.var - nu2.uncond
    valid <- V.g > 0 & V.a > 0

    Cor <- 0
    if (valid) Cor <- pmin(1, abs(Bias) / sqrt(V.g * V.a)) * sign(Bias)

    psi.rho <- if (!valid) {
      rep(0, nrow(x))
    } else {
      ((psi.theta.uncond - psi.theta.var) / sqrt(V.g * V.a)) -
        ((Bias * (psi.sigma2.uncond - psi.sigma2.var)) / (2 * (V.g^(3/2)) * sqrt(V.a))) -
        ((Bias * (psi.nu2.var - psi.nu2.uncond))       / (2 * (V.a^(3/2)) * sqrt(V.g)))
    }

    out_rows[[i]] <- data.frame(
      variable                    = covar,
      chi_squared_root_divergence = obs_imbalance,
      chi_squared_root_se         = imbalance_se,
      trend                       = obs_trend,
      trend_se                    = trend_se_i,
      alignment                   = Cor,
      alignment_se                = psi.sd(psi.rho),
      bias                        = -Bias,
      bias_se                     = psi.sd(psi.theta.uncond - psi.theta.var),
      stringsAsFactors            = FALSE
    )
  }

  do.call(rbind, out_rows)
}
