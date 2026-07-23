##' Covariate strength diagnostics for DML models
##' @description
##' For each benchmark covariate (and optionally for each learner in
##' \code{learners}), fits a single-covariate DML model and computes four
##' strength diagnostics: imbalance, trend, alignment (rho), and bias.  These
##' correspond to the columns of the "Covariate Strength" table in
##' Wang et al. (2026).
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
##'   for every learner, mirroring the \code{models_all} structure from
##'   Wang et al. (2026).
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
##' @references
##'   Wang, J., Sant'Anna, P. H. C., Chernozhukov, V., and Cinelli, C. (2026).
##'   "Omitted Variable Bias in Difference-in-Differences Designs." Working paper.
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

  # OVB alignment sign: delta = theta - theta_s decomposes as
  #   ATT : delta = -rho * (imbalance * trend * scaling)   -> sign(rho) = -sign(delta)
  #   ATU : delta = +rho * (imbalance * trend * scaling)   -> sign(rho) = +sign(delta)
  # compute_table's raw Cor uses sign(Bias) = -sign(delta) (Bias = theta.uncond - theta.var),
  # which is ATT-correct; flip it for ATU so the reported alignment carries the true rho sign.
  align.sign <- if (target == "att") 1 else -1

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
      if (valid) Cor <- align.sign * pmin(1, abs(Bias) / sqrt(V.g * V.a)) * sign(Bias)

      psi.rho <- if (!valid) {
        rep(0, n_obs)
      } else {
        align.sign * (
          ((psi.theta.uncond - psi.theta.var) / sqrt(V.g * V.a)) -
            ((Bias * (psi.sigma2.uncond - psi.sigma2.var)) / (2 * (V.g^(3/2)) * sqrt(V.a))) -
            ((Bias * (psi.nu2.var - psi.nu2.uncond))       / (2 * (V.a^(3/2)) * sqrt(V.g)))
        )
      }

      rows[[i]] <- data.frame(
        variable                    = covar,
        imbalance                   = obs_imbalance,
        se.imbalance                = imbalance_se,
        trend                       = obs_trend,
        se.trend                    = trend_se_i,
        rho                         = Cor,
        se.rho                      = psi.sd(psi.rho),
        bias                        = -Bias,
        se.bias                     = psi.sd(psi.theta.uncond - psi.theta.var),
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


# =============================================================================
# Covariate balance table
# =============================================================================
# Standardized mean differences and variance-ratio deviations between treated
# and control units for the observed covariates -- reproduces the covariate
# balance table (Table 2) of Wang et al. (2026). Self-contained: uses only
# model$data$x and model$data$d, independent of the diagnostics above.
check_balance_model <- function(model) {
  if (!inherits(model, "dml")) {
    stop("model must be an object of class 'dml' created with dml().")
  }
  if (is.null(model$data$x) || is.null(model$data$d)) {
    stop("model does not contain the covariates ('x') and/or treatment ('d') it was fitted with.")
  }
  invisible(TRUE)
}

check_balance_treatment <- function(d) {
  d.value <- unique(d)
  binary   <- all(d.value %in% c(0, 1))
  if (!binary) {
    stop("Treatment 'd' must be binary (0 = control, 1 = treated) to compute covariate balance.")
  }
  invisible(TRUE)
}

check_balance_covariates <- function(x, covariates) {

  if (is.null(colnames(x))) {
    stop("'x' must have column names identifying the covariates.")
  }

  if (is.null(covariates)) {
    return(colnames(x))
  }

  which.not <- which(!covariates %in% colnames(x))
  if (length(which.not) > 0) {
    stop("Covariates not found: ", paste(covariates[which.not], collapse = ", "), ".")
  }
  covariates
}

# per-covariate group statistics ---------------------------------------------
covariate_group_stats <- function(xj, d) {
  list(mean.control = mean(xj[d == 0]),
      var.control  = var(xj[d == 0]),
      mean.treated = mean(xj[d == 1]),
      var.treated  = var(xj[d == 1]))
}

# standardized mean difference, (mu1 - mu0)/sqrt((s1^2 + s0^2)/2) -----------
std_mean_diff <- function(mean.control, mean.treated, var.control, var.treated) {
  (mean.treated - mean.control) / sqrt((var.treated + var.control) / 2)
}

# deviation of the variance ratio from one, s1^2/s0^2 - 1 -------------------
var_ratio_dev <- function(var.control, var.treated) {
  var.treated / var.control - 1
}

# builds the balance table for a set of covariates ---------------------------
balance_fun <- function(x, d, covariates) {

  rows <- lapply(covariates, function(covar) {
    stats <- covariate_group_stats(x[, covar], d)
    smd <- std_mean_diff(mean.control = stats$mean.control,
                         mean.treated = stats$mean.treated,
                         var.control  = stats$var.control,
                         var.treated  = stats$var.treated)
    vrd <- var_ratio_dev(var.control = stats$var.control,
                        var.treated = stats$var.treated)
    c(mean.control  = stats$mean.control,
      var.control   = stats$var.control,
      mean.treated  = stats$mean.treated,
      var.treated   = stats$var.treated,
      std.mean.diff = smd,
      var.ratio.dev = vrd)
  })

  out <- do.call(rbind, rows)
  rownames(out) <- covariates
  out
}

##' Covariate balance statistics for debiased machine learning
##' @description
##' Computes a covariate balance table between treated and control units, following Table 2 of Wang et al. (2026). For each covariate, the table reports the group means and variances (control and treated), the standardized difference in means, and the deviation of the variance ratio from one.
##'
##' The covariates and treatment may be supplied either through a fitted
##' \code{\link{dml}} model (the \code{dml} method) or directly as a covariate
##' matrix \code{x} and a binary treatment vector \code{d} (the default method);
##' the latter does not require fitting a model, since the balance table depends
##' only on the observed covariates and treatment.
##' @param model an object of class \code{\link{dml}}; the covariates and
##'   treatment are taken from \code{model$data$x} and \code{model$data$d}. The
##'   treatment must be binary (0 = control, 1 = treated).
##' @param x a numeric matrix (or data frame) of covariates with column names.
##'   Used by the default method to compute balance directly from data.
##' @param d a binary treatment vector (0 = control, 1 = treated) with
##'   \code{length(d) == nrow(x)}.
##' @param covariates character vector with the names (and order) of the
##'   covariates to include in the balance table. Default (\code{NULL}) uses all
##'   columns of \code{x}.
##' @param ... arguments passed to other methods.
##' @returns An object of class \code{balance_stats} containing the covariate balance table.
##' @references
##'   Wang, J., Sant'Anna, P. H. C., Chernozhukov, V., and Cinelli, C. (2026).
##'   "Omitted Variable Bias in Difference-in-Differences Designs." Working paper.
##' @rdname balance_stats
##' @export
balance_stats <- function(...) {
  UseMethod("balance_stats")
}

##' @rdname balance_stats
##' @export
balance_stats.dml <- function(model, covariates = NULL, ...) {
  check_balance_model(model)
  balance_stats.default(x = model$data$x, d = model$data$d,
                        covariates = covariates, ...)
}

##' @rdname balance_stats
##' @export
balance_stats.default <- function(x, d, covariates = NULL, ...) {

  if (is.null(nrow(x)) || nrow(x) != length(d)) {
    stop("'x' must have one row per observation, with nrow(x) == length(d).")
  }

  check_balance_treatment(d)
  covariates <- check_balance_covariates(x, covariates)

  out <- list()
  out$info <- list(covariates = covariates,
                   n.control  = sum(d == 0),
                   n.treated  = sum(d == 1))

  out$table <- balance_fun(x = x, d = d, covariates = covariates)

  class(out) <- "balance_stats"
  return(out)
}

##' Print and summary methods for DML covariate balance
##' @description Print and summary methods for objects of class \code{balance_stats} created with \code{\link{balance_stats}}.
##' @param x an object of class \code{\link{balance_stats}} or \code{summary_balance_stats}.
##' @param digits minimal number of significant digits.
##' @rdname summary.balance_stats
##' @export
print.balance_stats <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  print(summary(x), digits = digits, ...)
}

##' @param object an object of class \code{\link{balance_stats}}.
##' @param ... arguments passed to other methods.
##' @returns For \code{print}: the object, printed to console. For \code{summary}: the object of class \code{summary_balance_stats} holding the balance table.
##' @rdname summary.balance_stats
##' @export
summary.balance_stats <- function(object, ...) {
  out <- object
  class(out) <- "summary_balance_stats"
  out
}

##' @rdname summary.balance_stats
##' @export
print.summary_balance_stats <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\n")
  cat("Covariate Balance:", x$info$n.control, "control units,", x$info$n.treated, "treated units\n")
  cat("\n")
  print(round(x$table, digits), ...)
  cat("\nNote: columns report control/treated means and variances, the standardized mean difference, and the variance ratio deviation from one.\n")
}

# Formatted rendering of the balance table (Table 2 layout), in LaTeX or
# HTML -----------------------------------------------------------------------
# The LaTeX renderer uses standard $...$ math, compiled by LaTeX itself. The
# HTML renderer deliberately does NOT reuse that notation: R Markdown's
# html_document only loads MathJax via a <script> injected at render time
# that fetches https://mathjax.rstudio.com/... at runtime, which fails
# silently when the file is opened offline or directly from disk (many
# browsers block scripts on file:// pages outright). Instead, the HTML
# symbols/formulas below are built from plain HTML entities and CSS, so the
# output renders identically with no JavaScript or network dependency. The
# LaTeX Std. Mean Diff./Var. Ratio Dev. columns use siunitx's `S` column
# type so that values align on the decimal point regardless of sign; the
# document's LaTeX preamble must include \usepackage{siunitx} in addition
# to \usepackage{booktabs}.

# LaTeX math notation for the header symbols/formulas
mu0.sym      <- "$\\mu_0$"
sigma0.sym   <- "$\\sigma_0^2$"
mu1.sym      <- "$\\mu_1$"
sigma1.sym   <- "$\\sigma_1^2$"
smd.formula  <- "$\\frac{\\mu_1-\\mu_0}{\\sqrt{(\\sigma_1^2+\\sigma_0^2)/2}}$"
vrd.formula  <- "$\\frac{\\sigma_1^2}{\\sigma_0^2}-1$"

# a self-contained CSS "stacked fraction" (numerator over a rule over
# denominator), used to build the HTML formulas below without any
# JavaScript/MathJax dependency
html_fraction <- function(numerator, denominator) {
  paste0("<span style=\"display:inline-block; vertical-align:middle; text-align:center;\">",
        "<span style=\"display:block; border-bottom:1px solid black; padding:0 2px;\">",
        numerator, "</span>",
        "<span style=\"display:block; padding:0 2px;\">", denominator, "</span>",
        "</span>")
}

# HTML notation for the header symbols/formulas (plain entities + CSS, no
# MathJax needed)
mu0.html    <- "&mu;<sub>0</sub>"
sigma0.html <- "&sigma;<sub>0</sub><sup>2</sup>"
mu1.html    <- "&mu;<sub>1</sub>"
sigma1.html <- "&sigma;<sub>1</sub><sup>2</sup>"
smd.formula.html <- html_fraction(
  paste0(mu1.html, " &minus; ", mu0.html),
  paste0("&radic;((", sigma1.html, " + ", sigma0.html, ")/2)")
)
vrd.formula.html <- paste0(html_fraction(sigma1.html, sigma0.html), " &minus; 1")

# escapes the LaTeX special characters that may show up in covariate names,
# captions, or labels
escape_latex <- function(text) {
  gsub("([&%$#_{}])", "\\\\\\1", text)
}

# escapes the HTML special characters that may show up in covariate names,
# captions, or labels (the latter may end up inside an id="..." attribute,
# hence also escaping quotes)
escape_html <- function(text) {
  text <- gsub("&", "&amp;",  text, fixed = TRUE)
  text <- gsub("<", "&lt;",   text, fixed = TRUE)
  text <- gsub(">", "&gt;",   text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text
}

# shared numeric formatting for finite values: fixed decimals for ordinary
# magnitudes, thousands separator for large ones (used by both renderers)
format_finite_num <- function(value, digits, big.threshold = 1000) {
  if (abs(value) >= big.threshold) {
    return(formatC(round(value), format = "d", big.mark = ","))
  }
  formatC(value, digits = digits, format = "f")
}

# the placeholder strings used for values that are not plain numbers -- an
# `S` column cannot parse these directly, so cells containing them must be
# wrapped in \multicolumn{1}{c}{...} (see latex_balance_rows())
latex_na    <- "--"
latex_inf   <- "$\\infty$"
latex_ninf  <- "$-\\infty$"

# formats a single numeric value for LaTeX: NA/NaN and +-Inf become
# plain-text placeholders, everything else uses format_finite_num()
format_latex_value <- function(value, digits, big.threshold = 1000) {
  if (is.na(value)) return(latex_na)
  if (is.infinite(value)) return(if (value > 0) latex_inf else latex_ninf)
  format_finite_num(value, digits, big.threshold)
}

# formats a numeric vector/matrix of values for LaTeX (see format_latex_value)
format_latex_num <- function(values, digits, big.threshold = 1000) {
  vapply(values, format_latex_value, character(1),
        digits = digits, big.threshold = big.threshold)
}

# formats a single numeric value for HTML: NA/NaN and +-Inf become HTML
# entities, everything else uses format_finite_num()
format_html_value <- function(value, digits, big.threshold = 1000) {
  if (is.na(value)) return("--")
  if (is.infinite(value)) return(if (value > 0) "&infin;" else "&minus;&infin;")
  format_finite_num(value, digits, big.threshold)
}

# formats a numeric vector/matrix of values for HTML (see format_html_value)
format_html_num <- function(values, digits, big.threshold = 1000) {
  vapply(values, format_html_value, character(1),
        digits = digits, big.threshold = big.threshold)
}

# wraps non-numeric placeholders (NA/Inf) so they render correctly inside a
# siunitx `S` column; plain numbers are left untouched
wrap_if_placeholder <- function(values) {
  placeholder <- values %in% c(latex_na, latex_inf, latex_ninf)
  ifelse(placeholder, paste0("\\multicolumn{1}{c}{", values, "}"), values)
}

# the siunitx `S[table-format=...]` column spec needed to fit every finite
# value in a column, given a fixed number of decimals
siunitx_format <- function(values, digits) {
  finite.values <- values[is.finite(values)]
  int.digits <- if (length(finite.values) == 0) 1L else
    max(1L, nchar(formatC(floor(abs(finite.values)), format = "d")))
  paste0("S[table-format=-", int.digits, ".", digits, "]")
}

# LaTeX header rows: group labels, per-column rules, then the (mu, sigma^2) /
# formula labels. Std. Mean Diff./Var. Ratio Dev. cells are wrapped in
# \multicolumn{1}{c}{...} because they sit in `S` columns, which only
# accept plain numbers in the body rows below.
latex_balance_header <- function() {
  c("\\toprule",
   paste(" & \\multicolumn{2}{c}{Untreated} & \\multicolumn{2}{c}{Treated}",
         "& \\multicolumn{1}{c}{Std. Mean Diff.}",
         "& \\multicolumn{1}{c}{Var. Ratio Dev.} \\\\"),
   "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-6} \\cmidrule(lr){7-7}",
   paste0(" & \\multicolumn{1}{c}{", mu0.sym, "} & \\multicolumn{1}{c}{", sigma0.sym, "}",
         "& \\multicolumn{1}{c}{", mu1.sym, "} & \\multicolumn{1}{c}{", sigma1.sym, "}",
         "& \\multicolumn{1}{c}{", smd.formula, "}",
         "& \\multicolumn{1}{c}{", vrd.formula, "} \\\\"),
   "\\midrule")
}

# HTML header rows: mirrors latex_balance_header(), using <th colspan> for
# the group labels and plain HTML entities/CSS (see above) for the
# symbols/formulas -- no MathJax dependency
html_balance_header <- function() {
  c("<thead>",
   "<tr>",
   "<th></th>",
   "<th colspan=\"2\" style=\"text-align:center; border-bottom:1px solid black;\">Untreated</th>",
   "<th colspan=\"2\" style=\"text-align:center; border-bottom:1px solid black;\">Treated</th>",
   "<th style=\"text-align:center; border-bottom:1px solid black;\">Std. Mean Diff.</th>",
   "<th style=\"text-align:center; border-bottom:1px solid black;\">Var. Ratio Dev.</th>",
   "</tr>",
   "<tr style=\"border-bottom:2px solid black;\">",
   "<th></th>",
   paste0("<th style=\"text-align:center;\">", mu0.html, "</th>"),
   paste0("<th style=\"text-align:center;\">", sigma0.html, "</th>"),
   paste0("<th style=\"text-align:center;\">", mu1.html, "</th>"),
   paste0("<th style=\"text-align:center;\">", sigma1.html, "</th>"),
   paste0("<th style=\"text-align:center;\">", smd.formula.html, "</th>"),
   paste0("<th style=\"text-align:center;\">", vrd.formula.html, "</th>"),
   "</tr>",
   "</thead>")
}

# resolves group_breaks into integer row indices marking the end of each
# group. Accepts either raw (1-indexed) row indices, for backward
# compatibility, or a list of covariate-name vectors defining each group in
# order -- the latter is robust to reordering or adding covariates, since it
# does not depend on hardcoded row positions. When named groups are given,
# they must account for every covariate in the table, in table order, so
# mistakes (typos, missing/reordered covariates) are caught immediately
# instead of silently drawing rules in the wrong place.
resolve_group_breaks <- function(group_breaks, covariate.names) {

  if (is.null(group_breaks) || is.numeric(group_breaks)) {
    return(group_breaks)
  }

  if (!is.list(group_breaks)) {
    stop("group_breaks must be NULL, an integer vector of row indices, ",
        "or a list of covariate-name vectors.")
  }

  flat <- unlist(group_breaks, use.names = FALSE)
  if (!identical(flat, covariate.names)) {
    stop("group_breaks must list every covariate in 'x' exactly once, ",
        "in the same order as the balance table.")
  }

  cumsum(vapply(group_breaks, length, integer(1)))
}

# one formatted LaTeX row per covariate, with an optional rule after the
# given (1-indexed) rows to separate covariate groups (e.g., region dummies
# vs. economic variables, as in Table 2)
latex_balance_rows <- function(table, digits, group_breaks = NULL) {
  covariate.names <- escape_latex(rownames(table))
  values <- format_latex_num(table, digits = digits)
  dim(values) <- dim(table)   # formatC() does not preserve matrix shape
  colnames(values) <- colnames(table)

  s.cols <- c("std.mean.diff", "var.ratio.dev")
  values[, s.cols] <- wrap_if_placeholder(values[, s.cols, drop = FALSE])

  rows <- apply(values, 1, paste, collapse = " & ")
  rows <- paste0(covariate.names, " & ", rows, " \\\\")

  if (!is.null(group_breaks)) {
    # a break after the last row would duplicate \bottomrule, so drop it
    group_breaks <- group_breaks[group_breaks < nrow(table)]
    rows[group_breaks] <- paste0(rows[group_breaks], "\n\\midrule")
  }

  rows
}

# one formatted HTML row (<tr>...</tr>) per covariate. Rather than adding a
# rule after the last row of a group (which, at the very last row, would sit
# oddly against the table's own border), a top border is added to the *first*
# row of each new group -- this also sidesteps the "duplicate rule" issue
# latex_balance_rows() has to explicitly guard against.
html_balance_rows <- function(table, digits, group_breaks = NULL) {
  covariate.names <- escape_html(rownames(table))
  values <- format_html_num(table, digits = digits)
  dim(values) <- dim(table)
  colnames(values) <- colnames(table)

  align <- c(mean.control = "right", var.control = "right",
            mean.treated = "right", var.treated = "right",
            std.mean.diff = "center", var.ratio.dev = "center")

  group.start <- if (is.null(group_breaks)) integer(0) else
    group_breaks[group_breaks < nrow(table)] + 1

  rows <- vapply(seq_len(nrow(table)), function(i) {
    border <- if (i %in% group.start) " style=\"border-top:1px solid black;\"" else ""
    cells <- paste0("<td style=\"text-align:", align, ";\">", values[i, ], "</td>",
                    collapse = "")
    paste0("<tr", border, "><td>", covariate.names[i], "</td>", cells, "</tr>")
  }, character(1))

  c("<tbody>", rows, "</tbody>")
}

# assembles the full LaTeX \begin{table}...\end{table} source
render_latex_balance <- function(x, digits = 3, caption = NULL, label = NULL,
                                 group_breaks = NULL) {

  if (is.null(caption)) {
    caption <- paste0("This table reports covariate balance between ",
                      x$info$n.treated, " treated and ", x$info$n.control,
                      " control units. From left to right, the columns report ",
                      "group means, variances, standardized mean differences, ",
                      "and deviations of the variance ratio from one.")
  }
  caption <- escape_latex(caption)
  if (!is.null(label)) label <- escape_latex(label)

  smd.spec <- siunitx_format(x$table[, "std.mean.diff"], digits)
  vrd.spec <- siunitx_format(x$table[, "var.ratio.dev"], digits)

  c("\\begin{table}[ht]",
   "\\centering",
   paste0("\\begin{tabular}{lrrrr", smd.spec, vrd.spec, "}"),
   latex_balance_header(),
   latex_balance_rows(x$table, digits = digits, group_breaks = group_breaks),
   "\\bottomrule",
   "\\end{tabular}",
   paste0("\\caption{", caption, "}"),
   if (!is.null(label)) paste0("\\label{", label, "}"),
   "\\end{table}")
}

# assembles the full HTML <table>...</table> source
render_html_balance <- function(x, digits = 3, caption = NULL, label = NULL,
                                group_breaks = NULL) {

  if (is.null(caption)) {
    caption <- paste0("This table reports covariate balance between ",
                      x$info$n.treated, " treated and ", x$info$n.control,
                      " control units. From left to right, the columns report ",
                      "group means, variances, standardized mean differences, ",
                      "and deviations of the variance ratio from one.")
  }
  caption <- escape_html(caption)

  # `label` becomes an id="..." attribute, HTML's equivalent of a LaTeX
  # \label -- it lets other content link to the table (e.g. <a href="#...">)
  id.attr <- if (!is.null(label)) paste0(" id=\"", escape_html(label), "\"") else ""

  c("<style>table.balance-table td, table.balance-table th { padding: 4px 10px; }</style>",
   paste0("<table class=\"balance-table\"", id.attr,
         " style=\"border-collapse: collapse; margin: 0 auto;\">"),
   html_balance_header(),
   html_balance_rows(x$table, digits = digits, group_breaks = group_breaks),
   "</table>",
   paste0("<p style=\"text-align:center;\">", caption, "</p>"))
}

##' Formatted table for covariate balance
##' @description
##' Formats an object of class \code{\link{balance_stats}} as a LaTeX or HTML table, replicating the layout of Table 2 in Wang et al. (2026). Intended to be used in an R Markdown/knitr chunk with \code{results = "asis"}. For \code{format = "latex"}, the document's LaTeX preamble must include \code{\\usepackage{booktabs}} (for the table rules) and \code{\\usepackage{siunitx}} (for decimal-point alignment of the Std. Mean Diff./Var. Ratio Dev. columns). For \code{format = "html"}, the output is plain, self-contained HTML/CSS with no MathJax or other JavaScript dependency, so it renders correctly even when the file is opened offline or directly from disk.
##' @param x an object of class \code{\link{balance_stats}}.
##' @param ... arguments passed to other methods.
##' @returns A character vector with the table source, printed to the console (or knitted document) and returned invisibly.
##' @references
##'   Wang, J., Sant'Anna, P. H. C., Chernozhukov, V., and Cinelli, C. (2026).
##'   "Omitted Variable Bias in Difference-in-Differences Designs." Working paper.
##' @export
balance_table <- function(x, ...) {
  UseMethod("balance_table")
}

##' @param format output format, either \code{"latex"} or \code{"html"}. Default is \code{"latex"}.
##' @param digits number of decimal places to display. Default is \code{3}.
##' @param caption table caption. Default (\code{NULL}) generates a caption analogous to Table 2 of Wang et al. (2026).
##' @param label a reference identifier for the table: for \code{format = "latex"}, this becomes a \code{\\label{}} for cross-referencing with \code{\\ref{}}; for \code{format = "html"}, this becomes an \code{id="..."} attribute on the \code{<table>}, so other content can link to it (e.g. \code{<a href="#label">}). Default is \code{NULL} (no label/id).
##' @param group_breaks either an integer vector with the (1-indexed) covariate rows after which a rule should be drawn, or a list of character vectors giving the covariate names in each group (which must together account for every covariate in \code{x}, in table order). Used to visually separate groups of covariates (e.g., region dummies vs. economic variables). Default (\code{NULL}) draws no such rules.
##' @rdname balance_table
##' @export
balance_table.balance_stats <- function(x, format = c("latex", "html"),
                                                digits = 3, caption = NULL, label = NULL,
                                                group_breaks = NULL, ...) {

  format <- match.arg(format)
  group_breaks <- resolve_group_breaks(group_breaks, rownames(x$table))

  out <- if (format == "latex") {
    render_latex_balance(x, digits = digits, caption = caption, label = label,
                         group_breaks = group_breaks)
  } else {
    render_html_balance(x, digits = digits, caption = caption, label = label,
                        group_breaks = group_breaks)
  }

  cat(out, sep = "\n")
  invisible(out)
}
