##' Covariate strength table for conditional DML (DiD/ATT)
##' @description
##' For each benchmark covariate, fits a single-covariate conditional DML model
##' and an intercept-only (null) model using the supplied data and learner
##' arguments, then computes four strength diagnostics per covariate:
##' imbalance (\code{chi_squared_root_divergence}), its SE, pre-trend
##' (\code{trend}), and its SE. These correspond to the columns of the
##' "Covariate Strength" table in the OVB-for-DiD manuscript.
##'
##' @param y numeric vector. Outcome (e.g. first-differenced outcome).
##' @param d numeric vector. Treatment indicator.
##' @param x numeric matrix. Full covariate matrix; columns must be named.
##' @param diagnostic_covariates character vector of column names in \code{x}
##'   to benchmark one at a time.
##' @param scaled logical. Passed to \code{cond_dml}. Default \code{TRUE}.
##' @param model character. Model type passed to \code{cond_dml}. Default \code{"npm"}.
##' @param cf.folds integer. Number of cross-fitting folds. Default \code{5}.
##' @param cf.reps integer. Number of cross-fitting repetitions. Default \code{1}.
##' @param cf.seed integer or \code{NULL}. Seed for cross-fitting. Default \code{NULL}.
##' @param ps.trim trimming rule passed to \code{cond_dml}. Default \code{0.01}.
##' @param dreg learner specification for the propensity score. Default \code{"ranger"}.
##' @param yreg0 learner specification for the outcome regression on controls.
##'   Default inherits \code{dreg}.
##' @param med.rep logical. If \code{TRUE} (default), summarise across cross-fitting
##'   repetitions by picking the rep closest to the median \code{theta.s}.
##'   Only relevant when \code{cf.reps > 1}.
##' @param verbose logical. Print progress messages. Default \code{TRUE}.
##' @param ... additional arguments passed to \code{cond_dml}.
##' @returns A \code{data.frame} with one row per benchmark covariate and columns:
##'   \code{variable}, \code{chi_squared_root_divergence},
##'   \code{chi_squared_root_se}, \code{trend}, \code{trend_se},
##'   \code{alignment}, \code{alignment_se}, \code{bias}, \code{bias_se}.
##' @export
cond_dml_strength <- function(y, d, x,
                              diagnostic_covariates,
                              scaled   = TRUE,
                              model    = "npm",
                              cf.folds = 5,
                              cf.reps  = 1,
                              cf.seed  = NULL,
                              ps.trim  = 0.01,
                              dreg     = "ranger",
                              yreg0    = dreg,
                              med.rep  = TRUE,
                              verbose  = TRUE,
                              ...) {

  which.not <- which(!diagnostic_covariates %in% colnames(x))
  if (length(which.not) > 0) {
    stop("Covariates not found in x: ",
         paste(diagnostic_covariates[which.not], collapse = ", "), ".")
  }

  fit_one <- function(x_i, label) {
    if (verbose) cat("\n=== Fitting model:", label, "===\n\n")
    cond_dml(y = y, d = d, x = x_i,
             scaled   = scaled,
             model    = model,
             target   = "att",
             cf.folds = cf.folds,
             cf.reps  = cf.reps,
             cf.seed  = cf.seed,
             ps.trim  = ps.trim,
             dreg     = dreg,
             yreg0    = yreg0,
             verbose  = verbose,
             ...)
  }

  pick_rep <- function(treat_results) {
    if (!med.rep || length(treat_results) == 1L) return(1L)
    theta.vec <- sapply(treat_results, function(r) r$estimates$theta.s)
    which.min(abs(theta.vec - stats::median(theta.vec)))
  }

  get_est <- function(treat_results, param, k) treat_results[[k]]$estimates[[param]]
  get_psi <- function(treat_results, param, k) treat_results[[k]]$psis[[param]]

  # ----- 1. Null model (intercept only) ----------------------------------------
  x_null     <- matrix(1, nrow = nrow(x), ncol = 1,
                       dimnames = list(NULL, "(Intercept)"))
  model_null <- fit_one(x_null, "null (intercept only)")

  treat_null        <- model_null$results$main$treat
  k_null            <- pick_rep(treat_null)
  sigma2.uncond     <- get_est(treat_null, "sigma2.s",     k_null)
  nu2.uncond        <- get_est(treat_null, "nu2.s",        k_null)
  theta.uncond      <- get_est(treat_null, "theta.s",      k_null)
  psi.theta.uncond  <- get_psi(treat_null, "psi.theta.s",  k_null)
  psi.sigma2.uncond <- get_psi(treat_null, "psi.sigma2.s", k_null)
  psi.nu2.uncond    <- get_psi(treat_null, "psi.nu2.s",    k_null)

  # ----- 2. One single-covariate model per benchmark variable ------------------
  out_rows <- vector("list", length(diagnostic_covariates))

  for (i in seq_along(diagnostic_covariates)) {
    covar     <- diagnostic_covariates[i]
    x_var     <- x[, covar, drop = FALSE]
    model_var <- fit_one(x_var, covar)

    treat_var      <- model_var$results$main$treat
    k_var          <- pick_rep(treat_var)

    nu2.var        <- get_est(treat_var, "nu2.s",    k_var)
    se.nu2.var     <- get_est(treat_var, "se.nu2.s", k_var)
    sigma2.var     <- get_est(treat_var, "sigma2.s", k_var)
    theta.var      <- get_est(treat_var, "theta.s",  k_var)
    psi.nu2.var    <- get_psi(treat_var, "psi.nu2.s",    k_var)
    psi.sigma2.var <- get_psi(treat_var, "psi.sigma2.s", k_var)
    psi.theta.var  <- get_psi(treat_var, "psi.theta.s",  k_var)

    # (1) Imbalance: sqrt(nu2 - 1)
    obs_imbalance <- sqrt(max(0, nu2.var - 1))
    imbalance_se  <- if (obs_imbalance > 0) se.nu2.var / (2 * obs_imbalance) else NA_real_

    # (2) Pre-trend: sqrt(max(0, 1 - sigma2_var / sigma2_uncond))
    obs_trend_sq  <- 1 - sigma2.var / sigma2.uncond
    obs_trend     <- sqrt(max(0, obs_trend_sq))
    psi.trend.raw <- (sigma2.var / sigma2.uncond^2) * psi.sigma2.uncond -
                     (1 / sigma2.uncond) * psi.sigma2.var
    psi.trend     <- if (obs_trend > 0) psi.trend.raw * 0.5 / obs_trend else rep(0, length(psi.trend.raw))
    trend_se_i    <- psi.sd(psi.trend)

    # (3) Alignment (correlation from bias decomposition)
    Bias  <- theta.uncond - theta.var
    V.g   <- sigma2.uncond - sigma2.var
    V.a   <- nu2.var - nu2.uncond
    valid <- V.g > 0 & V.a > 0

    Cor     <- 0
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
