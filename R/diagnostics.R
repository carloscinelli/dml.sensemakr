##' Covariate strength diagnostics for DML models
##' @description
##' For each benchmark covariate (and optionally for each learner in
##' \code{learners}), fits a single-covariate DML model and computes four
##' strength diagnostics: imbalance, trend, alignment, and bias.  These
##' correspond to the columns of the "Covariate Strength" table in the
##' OVB-for-DiD manuscript.
##'
##' @param y numeric vector. Outcome (e.g. first-differenced outcome).
##' @param d numeric vector. Treatment indicator.
##' @param x numeric matrix. Full covariate matrix; columns must be named.
##' @param diagnostic_covariates the covariates to benchmark one at a time.
##'   Either a character vector of column names in \code{x} (each benchmarked
##'   individually), or a named list where each element is a character vector of
##'   column names to benchmark \emph{together} as a single covariate (e.g. the
##'   dummy columns of a factor: \code{list(region = c("region2", "region3",
##'   "region4"))}).  The two forms may be mixed inside a list.  List element
##'   names become the row labels in the output table; unnamed elements are
##'   labelled by the column name (singletons) or the columns joined by
##'   \code{"+"}.
##' @param type character. \code{"cond"} (default) fits conditional models
##'   (DiD/panel setting) for the target given by \code{target}.
##'   \code{"uncond"} is reserved for future unconditional ATE diagnostics and
##'   is not yet implemented.
##' @param target character. Causal target for the conditional diagnostics:
##'   \code{"att"} (default) for the average effect on the treated (outcome
##'   regression fit on controls), or \code{"atu"} for the average effect on the
##'   untreated (outcome regression fit on the treated).
##' @param x_list optional named list of pre-built covariate matrices, keyed by
##'   covariate label.  When a label appears in \code{x_list}, that matrix is
##'   used as-is (no intercept is added automatically).  This is a low-level
##'   escape hatch for columns that are \emph{not} in \code{x} at all; to
##'   benchmark a group of columns that are already in \code{x} (such as a
##'   factor's dummies), prefer passing them as one entry of
##'   \code{diagnostic_covariates}.
##' @param scaled logical. Passed to \code{dml}. Default \code{FALSE}.
##'   When \code{FALSE}, \code{nu2.s} is the full second moment of the Riesz
##'   representer (\eqn{\chi^2 + 1}); when \code{TRUE}, it is the \eqn{\chi^2}
##'   divergence. The strength table is identical either way (the imbalance
##'   column is converted internally).
##' @param model character. \code{"npm"} (default) uses the nonparametric model.
##'   \code{"plm"} (partially linear model) is not yet implemented.
##' @param cf.folds integer. Number of cross-fitting folds. Default \code{5}.
##' @param cf.reps integer. Number of cross-fitting repetitions. Default \code{1}.
##' @param cf.seed integer or \code{NULL}. Seed for cross-fitting. Default \code{NULL}.
##' @param ps.trim trimming rule passed to \code{dml}. Default \code{0.01}.
##' @param dreg learner specification for the propensity score. Default \code{"ranger"}.
##'   Ignored when \code{learners} is supplied.
##' @param yreg0 learner specification for the outcome regression. Applied to
##'   the control units for \code{target = "att"} and to the treated units for
##'   \code{target = "atu"}. Default inherits \code{dreg}. Ignored when
##'   \code{learners} is supplied.
##' @param learners optional named list of learner pairs. Each element should be
##'   a named list with elements \code{dreg} and \code{yreg0}, e.g.
##'   \code{list(ranger = list(dreg = dreg_rf, yreg0 = yreg_rf), lm = list(dreg
##'   = list(method = "lm"), yreg0 = list(method = "lm")))}. When supplied,
##'   \code{dreg} and \code{yreg0} are ignored and the function returns results
##'   for every learner, mirroring the \code{models_all} structure from the
##'   manuscript.
##' @param null_model optional. A fitted \code{dml} object (from \code{\link{dml}}
##'   with \code{conditional = TRUE}, matching \code{target}, and an
##'   intercept-only \code{x}) whose conditional results (\code{treat} for ATT,
##'   \code{untr} for ATU) supply the null-model quantities.  Shared across all
##'   learners when \code{learners} is supplied.
##' @param med.rep logical. If \code{TRUE} (default), summarise across
##'   repetitions by picking the rep closest to the median \code{theta.s}.
##' @param verbose logical. Print progress messages. Default \code{TRUE}.
##' @param ... additional arguments passed to \code{\link{dml}}.
##' @returns When \code{learners} is \code{NULL}: a list with elements
##'   \code{table} (data frame) and \code{models} (named list of fitted
##'   \code{dml} objects).  When \code{learners} is supplied: a list with
##'   elements \code{tables} (named list of data frames, one per learner) and
##'   \code{models} (named list of named lists of fitted \code{dml} objects,
##'   mirroring \code{models_all[[learner]][[variable]]}).
##' @export
dml_diagnostic <- function(y, d, x,
                           diagnostic_covariates,
                           type       = c("cond", "uncond"),
                           target     = c("att", "atu"),
                           x_list     = NULL,
                           scaled     = FALSE,
                           model      = c("npm", "plm"),
                           cf.folds   = 5,
                           cf.reps    = 1,
                           cf.seed    = NULL,
                           ps.trim    = 0.01,
                           dreg       = "ranger",
                           yreg0      = dreg,
                           learners   = NULL,
                           null_model = NULL,
                           med.rep    = TRUE,
                           verbose    = TRUE,
                           ...) {

  type   <- match.arg(type)
  target <- match.arg(target)
  model  <- match.arg(model)

  if (type == "uncond")
    stop("Unconditional diagnostics (type = \"uncond\") are not yet implemented.")
  if (model == "plm")
    stop("PLM diagnostics (model = \"plm\") are not yet implemented.")

  # conditional results live in slot "treat" (ATT) or "untr" (ATU)
  cond_slot <- if (target == "att") "treat" else "untr"

  # Normalise diagnostic_covariates into a named list of column-name vectors:
  #   - a character vector  -> each element is its own singleton group
  #   - a list              -> each element is a group of columns to benchmark
  #     together; element names give the row label, falling back to the column
  #     name (singletons) or the columns joined by "+".
  covariate_groups <- if (is.list(diagnostic_covariates)) {
    diagnostic_covariates
  } else {
    as.list(diagnostic_covariates)
  }
  grp_names <- names(covariate_groups)
  if (is.null(grp_names)) grp_names <- rep("", length(covariate_groups))
  for (i in seq_along(covariate_groups)) {
    cols <- covariate_groups[[i]]
    if (!is.character(cols) || length(cols) < 1L)
      stop("Each entry of 'diagnostic_covariates' must be a non-empty ",
           "character vector of column names.")
    if (is.na(grp_names[i]) || grp_names[i] == "")
      grp_names[i] <- if (length(cols) == 1L) cols else paste(cols, collapse = "+")
  }
  names(covariate_groups) <- grp_names
  if (anyDuplicated(grp_names))
    stop("Duplicate covariate labels in 'diagnostic_covariates': ",
         paste(unique(grp_names[duplicated(grp_names)]), collapse = ", "), ".")
  covariate_labels <- grp_names

  # Every referenced column (for labels not supplied via x_list) must be in x
  labels_from_x <- setdiff(covariate_labels, names(x_list))
  cols_need_x   <- unique(unlist(covariate_groups[labels_from_x], use.names = FALSE))
  which.not     <- which(!cols_need_x %in% colnames(x))
  if (length(which.not) > 0) {
    stop("Columns not found in x: ",
         paste(cols_need_x[which.not], collapse = ", "), ".")
  }

  # Normalise learners: if not supplied, wrap dreg/yreg0 into the same structure
  if (is.null(learners)) {
    learners <- list(default = list(dreg = dreg, yreg0 = yreg0))
    single_learner <- TRUE
  } else {
    if (is.null(names(learners)) || any(names(learners) == ""))
      stop("Every element of 'learners' must be named.")
    for (nm in names(learners)) {
      if (!all(c("dreg", "yreg0") %in% names(learners[[nm]])))
        stop("learners[['", nm, "']] must have elements 'dreg' and 'yreg0'.")
    }
    single_learner <- FALSE
  }

  fit_one <- function(x_i, label, dreg_i, yreg0_i) {
    if (verbose) cat("\n=== Fitting model:", label, "===\n\n")
    dml(y = y, d = d, x = x_i,
        model       = model,
        target      = target,
        conditional = TRUE,
        scaled      = scaled,
        cf.folds    = cf.folds,
        cf.reps     = cf.reps,
        cf.seed     = cf.seed,
        ps.trim     = ps.trim,
        dreg        = dreg_i,
        yreg        = yreg0_i,
        verbose     = verbose,
        ...)
  }

  pick_rep <- function(treat_results) {
    if (!med.rep || length(treat_results) == 1L) return(1L)
    theta.vec <- sapply(treat_results, function(r) r$estimates$theta.s)
    which.min(abs(theta.vec - stats::median(theta.vec)))
  }

  get_est <- function(treat_results, param, k) treat_results[[k]]$estimates[[param]]
  get_psi <- function(treat_results, param, k) treat_results[[k]]$psis[[param]]

  # nu2.s is the full second moment E[(O_X/O)^2] when the model was fit with
  # scaled = FALSE, and the chi^2 divergence (that moment minus 1) when scaled =
  # TRUE. Convert either to the chi^2 divergence so the imbalance formulas below
  # are convention-agnostic. (Influence functions and SEs are unaffected by the
  # constant shift, so only the point estimates need converting.)
  to_chi2 <- function(nu2, is_scaled) if (isTRUE(is_scaled)) nu2 else nu2 - 1

  # ----- 1. Null model quantities (shared across all learners) -----------------
  n_obs <- nrow(x)

  if (!is.null(null_model)) {
    if (verbose) cat("\n=== Using supplied null_model for null-model quantities ===\n")
    treat_null        <- null_model$results$main[[cond_slot]]
    k_null            <- pick_rep(treat_null)
    sigma2.uncond     <- get_est(treat_null, "sigma2.s",  k_null)
    nu2.uncond        <- get_est(treat_null, "nu2.s",     k_null)
    theta.uncond      <- get_est(treat_null, "theta.s",   k_null)
    psi.theta.uncond  <- get_psi(treat_null, "psi.theta.s",  k_null)
    psi.sigma2.uncond <- get_psi(treat_null, "psi.sigma2.s", k_null)
    psi.nu2.uncond    <- get_psi(treat_null, "psi.nu2.s",    k_null)
  } else {
    if (verbose) cat("\n=== Computing null model quantities (analytical) ===\n")
    p_bar <- mean(d)
    if (target == "att") {
      # intercept-only conditional ATT: g0s = E[dY | D=0]
      g_null  <- mean(y[d == 0])
      l_null  <- d / p_bar               # treated target weight
      lc_null <- (1 - d) / (1 - p_bar)   # control weight for sigma2
      Ms_null <- (y - g_null) * l_null
    } else {
      # intercept-only conditional ATU: g1s = E[dY | D=1]
      g_null  <- mean(y[d == 1])
      l_null  <- (1 - d) / (1 - p_bar)   # untreated target weight
      lc_null <- d / p_bar               # treated weight for sigma2
      Ms_null <- (g_null - y) * l_null
    }
    sigma2.uncond     <- mean(lc_null * (y - g_null)^2) / mean(lc_null)
    psi.sigma2.uncond <- lc_null * ((y - g_null)^2 - sigma2.uncond) / mean(lc_null)
    nu2.uncond        <- 1
    psi.nu2.uncond    <- rep(0, n_obs)
    theta.uncond      <- mean(Ms_null) / mean(l_null)
    psi.theta.uncond  <- (Ms_null - theta.uncond * l_null) / mean(l_null)
  }

  # chi^2 divergence of the null model (0 for an intercept-only model). The
  # analytical fallback uses the full-second-moment convention (nu2.uncond = 1).
  scaled.uncond <- if (!is.null(null_model)) isTRUE(null_model$info$scaled) else FALSE
  chi2.uncond   <- to_chi2(nu2.uncond, scaled.uncond)

  # ----- 2. Helper: compute strength table from a list of fitted models --------
  compute_table <- function(fitted_models) {
    rows <- vector("list", length(covariate_labels))
    for (i in seq_along(covariate_labels)) {
      covar     <- covariate_labels[i]
      treat_var <- fitted_models[[covar]]$results$main[[cond_slot]]
      k_var     <- pick_rep(treat_var)

      nu2.var        <- get_est(treat_var, "nu2.s",    k_var)
      se.nu2.var     <- get_est(treat_var, "se.nu2.s", k_var)
      sigma2.var     <- get_est(treat_var, "sigma2.s", k_var)
      theta.var      <- get_est(treat_var, "theta.s",  k_var)
      psi.nu2.var    <- get_psi(treat_var, "psi.nu2.s",    k_var)
      psi.sigma2.var <- get_psi(treat_var, "psi.sigma2.s", k_var)
      psi.theta.var  <- get_psi(treat_var, "psi.theta.s",  k_var)

      scaled.var    <- isTRUE(fitted_models[[covar]]$info$scaled)
      chi2.var      <- to_chi2(nu2.var, scaled.var)
      obs_imbalance <- sqrt(max(0, chi2.var))
      imbalance_se  <- if (obs_imbalance > 0) se.nu2.var / (2 * obs_imbalance) else NA_real_

      obs_trend_sq  <- 1 - sigma2.var / sigma2.uncond
      obs_trend     <- sqrt(max(0, obs_trend_sq))
      psi.trend.raw <- (sigma2.var / sigma2.uncond^2) * psi.sigma2.uncond -
                       (1 / sigma2.uncond) * psi.sigma2.var
      psi.trend     <- if (obs_trend > 0) psi.trend.raw * 0.5 / obs_trend else rep(0, length(psi.trend.raw))
      trend_se_i    <- psi.sd(psi.trend)

      Bias  <- theta.uncond - theta.var
      V.g   <- sigma2.uncond - sigma2.var
      V.a   <- chi2.var - chi2.uncond
      valid <- V.g > 0 & V.a > 0

      Cor     <- 0
      if (valid) Cor <- pmin(1, abs(Bias) / sqrt(V.g * V.a)) * sign(Bias)

      psi.rho <- if (!valid) {
        rep(0, n_obs)
      } else {
        ((psi.theta.uncond - psi.theta.var) / sqrt(V.g * V.a)) -
          ((Bias * (psi.sigma2.uncond - psi.sigma2.var)) / (2 * (V.g^(3/2)) * sqrt(V.a))) -
          ((Bias * (psi.nu2.var - psi.nu2.uncond))       / (2 * (V.a^(3/2)) * sqrt(V.g)))
      }

      rows[[i]] <- data.frame(
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
    do.call(rbind, rows)
  }

  # ----- 3. Fit models and compute tables for each learner ---------------------
  all_tables <- vector("list", length(learners))
  all_models <- vector("list", length(learners))
  names(all_tables) <- names(all_models) <- names(learners)

  for (lrn in names(learners)) {
    if (verbose && !single_learner)
      cat("\n\n========== Learner:", lrn, "==========\n")

    dreg_l  <- learners[[lrn]]$dreg
    yreg0_l <- learners[[lrn]]$yreg0

    fitted <- vector("list", length(covariate_labels))
    names(fitted) <- covariate_labels

    for (covar in covariate_labels) {
      if (!is.null(x_list) && covar %in% names(x_list)) {
        x_var <- x_list[[covar]]
      } else {
        x_var <- cbind("(Intercept)" = 1,
                       x[, covariate_groups[[covar]], drop = FALSE])
      }
      fitted[[covar]] <- fit_one(x_var, paste0(lrn, "/", covar), dreg_l, yreg0_l)
    }

    all_models[[lrn]] <- fitted
    all_tables[[lrn]] <- compute_table(fitted)
  }

  # ----- 4. Return -------------------------------------------------------------
  if (single_learner) {
    list(
      table  = all_tables[["default"]],
      models = all_models[["default"]]
    )
  } else {
    list(
      tables = all_tables,
      models = all_models
    )
  }
}
