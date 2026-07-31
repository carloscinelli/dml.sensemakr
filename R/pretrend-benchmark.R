##' Pre-trend benchmarking for difference-in-differences OVB sensitivity
##'
##' @description
##' Calibrates omitted-variable-bias sensitivity for a difference-in-differences
##' ATT (or ATE/ATU) against the bias implied by an observed pre-treatment trend,
##' following the "benchmarking against pre-trend" analysis of the OVB-in-DiD
##' paper.
##'
##' @param model post-treatment \code{\link{dml}} fit (the analysis model).
##' @param pre_model pre-treatment placebo \code{\link{dml}} fit, computed on the
##'   same units and target as \code{model} but using the pre-treatment period.
##' @param parameter target quantity. Currently only \code{"att"} (the default)
##'   is supported: the pre-trend extrapolation theory is derived for the
##'   conditional ATT, and the \code{"ate"}/\code{"atu"} analogues are not yet
##'   established, so they raise an error rather than return unvalidated bounds.
##'   Must be present in both models.
##' @param k relative-strength multiplier for the post-vs-pre bias (the \eqn{k}
##'   of the extrapolation). Default \code{1} (transport the observed pre-trend
##'   as-is), reproducing \code{main2.Rmd}.
##' @param level confidence level for the CI-based contour level and band.
##'   Default \code{0.95}.
##' @param combine.method how to combine cross-fitting repetitions,
##'   \code{"median"} (default) or \code{"mean"}.
##' @param rho2 adversity \eqn{\rho_0^2} used when re-drawing the post-treatment
##'   sensitivity surface for the overlay. Default \code{1} (worst case).
##' @returns An object of class \code{dml_pretrend}: a list with the post- and
##'   pre-treatment estimates and scales, the scale ratio \code{scale.ratio}
##'   (\eqn{S_0/S_{0,pre}}), \code{bias.factor.pre}, and, for each extrapolation
##'   method, the transported point bounds \eqn{[\theta_-, \theta_+]}
##'   (\code{bounds.magnitude}/\code{bounds.factors}) and the one-sided
##'   confidence bounds \eqn{[\ell, u]}
##'   (\code{confbound.magnitude}/\code{confbound.factors}), built from the
##'   Appendix-C.2 influence functions of \eqn{\theta_\pm} with the one-sided
##'   critical value \eqn{\Phi^{-1}(1-a)}. It also carries the influence-function
##'   standard errors \code{se.tp.*}/\code{se.tm.*} and the post-treatment
##'   \code{surface} components used to redraw the sensitivity surface.
##' @seealso \code{\link{add_pretrend_contour}} to draw the locus on a contour
##'   plot; \code{\link{dml_benchmark}} for the (separate) covariate benchmarking.
##' @references
##'   Wang, J., Sant'Anna, P. H. C., Chernozhukov, V., and Cinelli, C. (2026).
##'   "Omitted Variable Bias in Difference-in-Differences Designs." Working paper.
##' @export
pretrend_benchmark <- function(model, pre_model,
                               parameter = c("att", "ate", "atu"),
                               k = 1, level = 0.95,
                               combine.method = c("median", "mean"),
                               rho2 = 1) {
  parameter      <- match.arg(parameter)
  combine.method <- match.arg(combine.method)

  # Pre-trend benchmarking is currently validated only for the conditional ATT.
    if (!identical(parameter, "att"))
    stop("pretrend_benchmark() currently supports only parameter = 'att'; '",
         parameter, "' is not allowed yet. The pre-trend extrapolation is ",
         "derived only for the conditional ATT; the ATE/ATU analogues are not yet ",
         "established, so they are disabled to avoid reporting unvalidated bounds.",
         call. = FALSE)

  slot <- unname(.target_to_slot[parameter])
  if (is.na(slot) || is.null(model$results$main[[slot]]))
    stop("`model` has no '", parameter, "' results (slot '", slot, "').")
  if (is.null(pre_model$results$main[[slot]]))
    stop("`pre_model` has no '", parameter, "' results (slot '", slot, "').")

  post <- model$results$main[[slot]]
  pre  <- pre_model$results$main[[slot]]

  need <- function(res, p, what) {
    v <- extract_estimate(res, p)
    if (is.null(v) || all(is.na(v)))
      stop("`", what, "` is missing the '", p, "' estimate needed for the ",
           "pre-trend surface. Refit with the current dml().")
    v
  }
  cmb <- function(v) if (combine.method == "median") stats::median(v) else mean(v)

  # ---- post-treatment sensitivity-surface components (one value per rep) ------
  theta.s      <- need(post, "theta.s", "model")
  S2           <- tryCatch(extract_estimate(post, "S2"), error = function(e) NULL)
  if (is.null(S2) || all(is.na(S2)))
    S2 <- need(post, "sigma2.s", "model") * need(post, "nu2.s", "model")
  se.theta.s   <- need(post, "se.theta.s", "model")
  se.S2        <- need(post, "se.S2", "model")
  cov.theta.S2 <- need(post, "cov.theta.S2", "model")

  theta.s.post    <- cmb(theta.s)
  se.theta.s.post <- cmb(se.theta.s)
  S0.post         <- sqrt(cmb(S2))

  # ---- pre-treatment (placebo) components -------------------------------------
  theta.s.pre    <- cmb(need(pre, "theta.s", "pre_model"))
  se.theta.s.pre <- cmb(need(pre, "se.theta.s", "pre_model"))
  sigma2.s.pre   <- cmb(need(pre, "sigma2.s", "pre_model"))
  nu2.s.pre      <- cmb(need(pre, "nu2.s", "pre_model"))
  S0.pre         <- sqrt(sigma2.s.pre * nu2.s.pre)

  # pre-treatment bias-factor product |rho0 C0Y C0D|_pre = |theta_s,pre| / S0,pre
  bias.factor.pre <- abs(theta.s.pre) / S0.pre

  # ---- C.2 influence functions and one-sided confidence bounds [l, u] ---------
  sigma2.s.post <- cmb(need(post, "sigma2.s", "model"))
  nu2.s.post    <- cmb(need(post, "nu2.s", "model"))

  ifs <- function(res, what) res[[1]]$psis[[what]]     # rep-1 influence functions
  psi.theta.s   <- ifs(post, "psi.theta.s");  psi.theta.s.pre  <- ifs(pre, "psi.theta.s")
  psi.sigma2.s  <- ifs(post, "psi.sigma2.s"); psi.sigma2.s.pre <- ifs(pre, "psi.sigma2.s")
  psi.nu2.s     <- ifs(post, "psi.nu2.s");    psi.nu2.s.pre    <- ifs(pre, "psi.nu2.s")
  if (is.null(psi.theta.s) || is.null(psi.theta.s.pre) ||
      is.null(psi.sigma2.s) || is.null(psi.nu2.s) ||
      is.null(psi.sigma2.s.pre) || is.null(psi.nu2.s.pre))
    stop("Influence functions (theta.s, sigma2.s, nu2.s) are missing from one of ",
         "the models; refit with the current dml().")
  if (length(psi.theta.s) != length(psi.theta.s.pre))
    stop("`model` and `pre_model` have different sample sizes; their influence ",
         "functions cannot be combined. Fit both on the same units.")
  if (length(post) > 1L || length(pre) > 1L)
    warning("cf.reps > 1: the confidence bounds use the first repetition's ",
            "influence functions only (matching the manuscript).")

  r   <- S0.post / S0.pre
  sgn <- sign(theta.s.pre)

  psi.S0     <- (sigma2.s.post * psi.nu2.s     + nu2.s.post * psi.sigma2.s)     / (2 * S0.post)
  psi.S0.pre <- (sigma2.s.pre  * psi.nu2.s.pre + nu2.s.pre  * psi.sigma2.s.pre) / (2 * S0.pre)

  #   magnitude: phi_theta_s +/- k*sign(theta_s,pre)*phi_theta_s,pre
  #   factors  : phi_theta_s +/- k|theta_s,pre|*r*( phi_theta_s,pre/theta_s,pre
  #                                                 + phi_S0/S0 - phi_S0,pre/S0,pre )
  psi.tp.mag <- psi.theta.s + k * sgn * psi.theta.s.pre
  psi.tm.mag <- psi.theta.s - k * sgn * psi.theta.s.pre
  fac.inner  <- psi.theta.s.pre / theta.s.pre + psi.S0 / S0.post - psi.S0.pre / S0.pre
  psi.tp.fac <- psi.theta.s + k * abs(theta.s.pre) * r * fac.inner
  psi.tm.fac <- psi.theta.s - k * abs(theta.s.pre) * r * fac.inner

  se.tp.magnitude <- psi.sd(psi.tp.mag); se.tm.magnitude <- psi.sd(psi.tm.mag)
  se.tp.factors   <- psi.sd(psi.tp.fac); se.tm.factors   <- psi.sd(psi.tm.fac)

  # ---- transported point bounds theta_+/- (the paper's two strategies) --------
  b.mag <- k *     abs(theta.s.pre)   # (1) magnitude: theta.s +/- k|theta_s,pre|
  b.fac <- k * r * abs(theta.s.pre)   # (2) factors  : theta.s +/- k(S0/S0pre)|theta_s,pre|
  bounds.magnitude <- c(lwr = theta.s.post - b.mag, upr = theta.s.post + b.mag)
  bounds.factors   <- c(lwr = theta.s.post - b.fac, upr = theta.s.post + b.fac)

  # ---- one-sided C.2 confidence bounds [l, u] --------------------------------
  z1 <- stats::qnorm(level)
  confbound.magnitude <- c(
    lwr = unname(bounds.magnitude["lwr"]) - z1 * se.tm.magnitude,
    upr = unname(bounds.magnitude["upr"]) + z1 * se.tp.magnitude)
  confbound.factors <- c(
    lwr = unname(bounds.factors["lwr"]) - z1 * se.tm.factors,
    upr = unname(bounds.factors["upr"]) + z1 * se.tp.factors)

  out <- list(
    parameter = parameter, k = k, level = level,
    combine.method = combine.method, rho2 = rho2,
    theta.s.post = theta.s.post, se.theta.s.post = se.theta.s.post,
    S0.post = S0.post,
    theta.s.pre = theta.s.pre, se.theta.s.pre = se.theta.s.pre,
    S0.pre = S0.pre, scale.ratio = r, bias.factor.pre = bias.factor.pre,
    bounds.magnitude = bounds.magnitude, bounds.factors = bounds.factors,
    confbound.magnitude = confbound.magnitude, confbound.factors = confbound.factors,
    se.tp.magnitude = se.tp.magnitude, se.tm.magnitude = se.tm.magnitude,
    se.tp.factors = se.tp.factors, se.tm.factors = se.tm.factors,
    surface = list(theta.s = theta.s, S2 = S2, se.theta.s = se.theta.s,
                   se.S2 = se.S2, cov.theta.S2 = cov.theta.S2)
  )
  class(out) <- "dml_pretrend"
  out
}

##' @param x an object of class \code{dml_pretrend}.
##' @param digits number of significant digits to print.
##' @param ... ignored.
##' @rdname pretrend_benchmark
##' @export
print.dml_pretrend <- function(x, digits = 4, ...) {
  fmt <- function(v) formatC(v, digits = digits, format = "f")
  cat("Pre-trend benchmark  (parameter = ", x$parameter,
      ", k = ", x$k, ")\n\n", sep = "")
  cat("  post-treatment  theta.s      = ", fmt(x$theta.s.post),
      "  (se ", fmt(x$se.theta.s.post), ")\n", sep = "")
  cat("  pre-treatment   theta.s,pre  = ", fmt(x$theta.s.pre),
      "  (se ", fmt(x$se.theta.s.pre), ")\n", sep = "")
  cat("  scale  S0(post) = ", fmt(x$S0.post), "   S0(pre) = ", fmt(x$S0.pre),
      "   S0/S0pre = ", fmt(x$scale.ratio), "\n", sep = "")
  cat("  pre-trend bias-factor product |rho0 C0Y C0D|_pre = ",
      fmt(x$bias.factor.pre), "\n\n", sep = "")
  cat("  transported bounds  (point estimate  [theta_-, theta_+]):\n")
  cat("   [magnitude]  theta.s +/- k|theta.s,pre|           : [",
      fmt(x$bounds.magnitude["lwr"]), ", ", fmt(x$bounds.magnitude["upr"]), "]\n", sep = "")
  cat("   [factors]    theta.s +/- k(S0/S0,pre)|theta.s,pre|: [",
      fmt(x$bounds.factors["lwr"]), ", ", fmt(x$bounds.factors["upr"]), "]\n", sep = "")
  cat("\n  one-sided ", format(100 * x$level), "% confidence bounds  [l, u]:\n", sep = "")
  cat("   [magnitude]  [", fmt(x$confbound.magnitude["lwr"]), ", ",
      fmt(x$confbound.magnitude["upr"]), "]\n", sep = "")
  cat("   [factors]    [", fmt(x$confbound.factors["lwr"]), ", ",
      fmt(x$confbound.factors["upr"]), "]\n", sep = "")
  invisible(x)
}

##' Overlay a pre-trend locus on a DiD sensitivity contour plot
##'
##' @description
##' Adds a SINGLE pre-trend confidence-bound contour -- for the extrapolation
##' method you select -- to a contour plot produced by
##' \code{\link{ovb_contour_plot}}. Call \code{ovb_contour_plot(model, ...)}
##' first, then this function with matching \code{which.bound}, \code{lim.x},
##' \code{lim.y} and \code{grid.number}. It redraws the post-treatment bound
##' surface on the same grid and adds a dashed contour at the pre-trend
##' one-sided confidence bound: the upper bound \eqn{u} when
##' \code{which.bound = "upr"}, the lower bound \eqn{\ell} when
##' \code{which.bound = "lwr"} (see \code{\link{pretrend_benchmark}}). Only the
##' selected method and direction are drawn.
##'
##' @param x an object of class \code{dml_pretrend} from
##'   \code{\link{pretrend_benchmark}}.
##' @param method which extrapolation to use: \code{"magnitude"}
##'   (default; transport \eqn{k|\theta_{s,pre}|}) or \code{"factors"} (transport
##'   the bias factors, re-scaled by \eqn{S_0/S_{0,pre}}). Only this one is drawn.
##' @param which.bound draw the upper confidence bound \eqn{u} (\code{"upr"},
##'   default) or the lower bound \eqn{\ell} (\code{"lwr"}); must match the
##'   \code{ovb_contour_plot} call.
##' @param lim.x,lim.y,grid.number axis limits and grid resolution; must match
##'   the \code{ovb_contour_plot} call so the grids align.
##' @param col color for the pre-trend confidence-bound contour.
##' @param lwd,lty line width and type for the added contour.
##' @param labcex contour label size; \code{draw.labels = FALSE} suppresses labels.
##' @param draw.labels logical; label the contour with its value. Default
##'   \code{TRUE}.
##' @param ... further arguments passed to \code{\link[graphics]{contour}}.
##' @returns \code{x}, invisibly.
##' @seealso \code{\link{pretrend_benchmark}}
##' @export
add_pretrend_contour <- function(x,
                                 method = c("magnitude", "factors"),
                                 which.bound = c("upr", "lwr"),
                                 lim.x = 0.15, lim.y = lim.x, grid.number = 70,
                                 col = "lightblue",
                                 lwd = 3, lty = 2, labcex = 1.1,
                                 draw.labels = TRUE, ...) {
  if (!inherits(x, "dml_pretrend"))
    stop("`x` must be a 'dml_pretrend' object from pretrend_benchmark().")
  if (!identical(x$parameter, "att"))
    stop("add_pretrend_contour() currently supports only parameter = 'att' ",
         "objects; this object has parameter = '", x$parameter, "'. Pre-trend ",
         "benchmarking is validated only for the conditional ATT.", call. = FALSE)
  method      <- match.arg(method)
  which.bound <- match.arg(which.bound)
  cb  <- if (method == "magnitude") x$confbound.magnitude else x$confbound.factors
  lvl <- unname(cb[which.bound])       # u for which.bound = "upr", l for "lwr"
  s <- x$surface

  x_grid <- seq(0, lim.x, by = lim.x / grid.number)
  y_grid <- seq(0, lim.y, by = lim.y / grid.number)
  vb <- Vectorize(confidence_bounds.numeric, vectorize.args = c("cf.y", "cf.d"))
  f <- function(cf.d, cf.y)
    vb(theta.s = s$theta.s, S2 = s$S2, se.theta.s = s$se.theta.s,
       se.S2 = s$se.S2, cov.theta.S2 = s$cov.theta.S2,
       cf.y = cf.y, cf.d = cf.d, rho2 = x$rho2,
       combine.method = x$combine.method, level = x$level)[which.bound, ]
  z_grid <- outer(X = x_grid, Y = y_grid, FUN = f)

  lab <- function(v) if (draw.labels) round(v, 4) else FALSE
  graphics::contour(x_grid, y_grid, z_grid, levels = lvl,
                    add = TRUE, col = col, lwd = lwd, lty = lty,
                    labels = lab(lvl), labcex = labcex, ...)
  invisible(x)
}
