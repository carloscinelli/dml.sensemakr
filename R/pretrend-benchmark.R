# =============================================================================
# Pre-trend benchmarking for the DiD OVB sensitivity analysis.
#
# This file is self-contained and additive: it does NOT modify the covariate
# benchmarking (dml_benchmark / bench_fun) or the contour machinery
# (ovb_contour_plot / confidence_bounds). It only *uses* the exported/internal
# helpers confidence_bounds.numeric(), extract_estimate(), psi.sd() and
# .target_to_slot to reproduce main2.Rmd's pre-trend calibration and overlay.
#
# Theory (ovb4did.pdf, "benchmarking against pre-trend bias"): the pre-treatment
# period is a placebo (theta_pre = 0), so the pre-treatment bias reduces to the
# observed short estimand theta_s,pre. Its factors satisfy
#   |rho0,pre| * C0Y,pre * C0D,pre * S0,pre = |theta_s,pre|.
# Two strategies transport it to the post period:
#   (1) magnitude:  theta ± k |theta_s,pre|
#   (2) factors:    theta ± k (S0 / S0,pre) |theta_s,pre|
# where S0^2 = sigma2_s * nu2_s and k collects the relative change in the
# bias components across periods.
# =============================================================================

##' Pre-trend benchmarking for difference-in-differences OVB sensitivity
##'
##' @description
##' Calibrates omitted-variable-bias sensitivity for a difference-in-differences
##' ATT (or ATE/ATU) against the bias implied by an observed pre-treatment trend,
##' following the "benchmarking against pre-trend" analysis of the OVB-in-DiD
##' paper. Given a post-treatment \code{\link{dml}} fit and a pre-treatment
##' placebo \code{\link{dml}} fit (same estimator, pre-period), it returns the
##' pre-trend short estimand \eqn{\theta_{s,pre}}, the pre-treatment scale
##' \eqn{S_{0,pre}}, the transported bias-factor product
##' \eqn{|\rho_0 C_{0Y} C_{0D}|_{pre} = |\theta_{s,pre}| / S_{0,pre}}, the two
##' transported bounds, and the levels needed to overlay the pre-trend locus on a
##' contour plot via \code{\link{add_pretrend_contour}}.
##'
##' @param model post-treatment \code{\link{dml}} fit (the analysis model).
##' @param pre_model pre-treatment placebo \code{\link{dml}} fit, computed on the
##'   same units and target as \code{model} but using the pre-treatment period.
##' @param parameter target quantity: \code{"att"} (default), \code{"ate"} or
##'   \code{"atu"}. Must be present in both models.
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
##'   (\eqn{S_0/S_{0,pre}}), \code{bias.factor.pre}, and the two transported
##'   bounds \code{bounds.magnitude} and \code{bounds.factors}. For each
##'   extrapolation method it also carries the difference SE
##'   (\code{se.diff.magnitude}/\code{se.diff.factors}), the overlay contour
##'   level (\code{level.magnitude}/\code{level.factors}) and its confidence band
##'   (\code{band.magnitude}/\code{band.factors}), plus the post-treatment
##'   \code{surface} components used to redraw it.
##' @seealso \code{\link{add_pretrend_contour}} to draw the locus on a contour
##'   plot; \code{\link{dml_benchmark}} for the (separate) covariate benchmarking.
##' @export
pretrend_benchmark <- function(model, pre_model,
                               parameter = c("att", "ate", "atu"),
                               k = 1, level = 0.95,
                               combine.method = c("median", "mean"),
                               rho2 = 1) {
  parameter      <- match.arg(parameter)
  combine.method <- match.arg(combine.method)
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

  # ---- SE of the (post - transported pre) difference (rep-1 IFs) --------------
  # magnitude transports k*theta_s,pre; factors transports k*r*theta_s,pre with
  # r = S0.post/S0.pre (the factors SE/band treat the scale ratio r as fixed).
  psi.post <- post[[1]]$psis$psi.theta.s
  psi.pre  <- pre[[1]]$psis$psi.theta.s
  if (is.null(psi.post) || is.null(psi.pre))
    stop("Influence functions for theta.s are missing from one of the models.")
  if (length(psi.post) != length(psi.pre))
    stop("`model` and `pre_model` have different sample sizes; their influence ",
         "functions cannot be differenced. Fit both on the same units.")
  if (length(post) > 1L || length(pre) > 1L)
    warning("cf.reps > 1: the post-vs-pre difference SE uses the first ",
            "repetition only (matching the manuscript).")
  r <- S0.post / S0.pre                          # scale ratio, post vs pre
  se.diff.magnitude <- psi.sd(psi.post - k *     psi.pre)
  se.diff.factors   <- psi.sd(psi.post - k * r * psi.pre)

  # ---- transported bounds (the paper's two strategies) ------------------------
  b.mag <- k *     abs(theta.s.pre)   # (1) magnitude: theta.s +/- k|theta_s,pre|
  b.fac <- k * r * abs(theta.s.pre)   # (2) factors  : theta.s +/- k(S0/S0pre)|theta_s,pre|
  bounds.magnitude <- c(lwr = theta.s.post - b.mag, upr = theta.s.post + b.mag)
  bounds.factors   <- c(lwr = theta.s.post - b.fac, upr = theta.s.post + b.fac)

  # ---- contour-overlay levels: locus where the post bias equals the pre-trend -
  # On the post upr surface z = theta.s + bias.bound + z*se, the contour at
  #   level = CI.upr - k*theta_s,pre     -> bias.bound = k|theta_s,pre|              (magnitude)
  #   level = CI.upr - k*r*theta_s,pre   -> bias.bound = k(S0/S0pre)|theta_s,pre|    (factors)
  # add_pretrend_contour(method=) draws exactly one of these.
  fac2   <- stats::qnorm(c((1 - level) / 2, 1 - (1 - level) / 2))
  ci.upr <- theta.s.post + fac2[2] * se.theta.s.post   # = confint(model)[,"upr"]
  level.magnitude <- ci.upr - k *     theta.s.pre
  level.factors   <- ci.upr - k * r * theta.s.pre
  band.magnitude  <- level.magnitude + se.diff.magnitude * fac2
  band.factors    <- level.factors   + se.diff.factors   * fac2

  out <- list(
    parameter = parameter, k = k, level = level,
    combine.method = combine.method, rho2 = rho2,
    theta.s.post = theta.s.post, se.theta.s.post = se.theta.s.post,
    S0.post = S0.post,
    theta.s.pre = theta.s.pre, se.theta.s.pre = se.theta.s.pre,
    S0.pre = S0.pre, scale.ratio = r, bias.factor.pre = bias.factor.pre,
    se.diff.magnitude = se.diff.magnitude, se.diff.factors = se.diff.factors,
    bounds.magnitude = bounds.magnitude, bounds.factors = bounds.factors,
    surface = list(theta.s = theta.s, S2 = S2, se.theta.s = se.theta.s,
                   se.S2 = se.S2, cov.theta.S2 = cov.theta.S2),
    level.magnitude = level.magnitude, level.factors = level.factors,
    band.magnitude = band.magnitude, band.factors = band.factors
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
  cat("  transported bounds:\n")
  cat("   [magnitude]  theta.s +/- k|theta.s,pre|           : [",
      fmt(x$bounds.magnitude["lwr"]), ", ", fmt(x$bounds.magnitude["upr"]), "]\n", sep = "")
  cat("   [factors]    theta.s +/- k(S0/S0,pre)|theta.s,pre|: [",
      fmt(x$bounds.factors["lwr"]), ", ", fmt(x$bounds.factors["upr"]), "]\n", sep = "")
  cat("\n  contour-overlay level (add_pretrend_contour method =):\n")
  cat("   magnitude = ", fmt(x$level.magnitude),
      "   factors = ", fmt(x$level.factors), "\n", sep = "")
  invisible(x)
}

##' Overlay a pre-trend locus on a DiD sensitivity contour plot
##'
##' @description
##' Adds a SINGLE pre-trend benchmark contour -- the extrapolation method you
##' select -- to a contour plot produced by \code{\link{ovb_contour_plot}}. Call
##' \code{ovb_contour_plot(model, ...)} first, then this function with matching
##' \code{which.bound}, \code{lim.x}, \code{lim.y} and \code{grid.number}. It
##' redraws the post-treatment bound surface on the same grid and adds a dashed
##' contour at the level where the adverse bias equals the transported pre-trend
##' -- \eqn{k\,|\theta_{s,pre}|} for \code{method = "magnitude"}, or
##' \eqn{k\,(S_0/S_{0,pre})\,|\theta_{s,pre}|} for \code{method = "factors"} --
##' optionally with a confidence band. Only the selected method is plotted; the
##' magnitude overlay reproduces the blue / light-blue lines in \code{main2.Rmd}.
##'
##' @param x an object of class \code{dml_pretrend} from
##'   \code{\link{pretrend_benchmark}}.
##' @param method which extrapolation locus to draw: \code{"magnitude"}
##'   (default; transport \eqn{k|\theta_{s,pre}|}) or \code{"factors"} (transport
##'   the bias factors, re-scaled by \eqn{S_0/S_{0,pre}}). Only this one is drawn.
##' @param which.bound overlay on the \code{"upr"} (default) or \code{"lwr"}
##'   bound surface; must match the \code{ovb_contour_plot} call.
##' @param lim.x,lim.y,grid.number axis limits and grid resolution; must match
##'   the \code{ovb_contour_plot} call so the grids align.
##' @param band logical; also draw the confidence-band contour. Default
##'   \code{TRUE}.
##' @param col,col.band colors for the pre-trend contour and its band.
##' @param lwd,lty line width and type for the added contours.
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
                                 band = TRUE,
                                 col = "blue", col.band = "lightblue",
                                 lwd = 3, lty = 2, labcex = 1.1,
                                 draw.labels = TRUE, ...) {
  if (!inherits(x, "dml_pretrend"))
    stop("`x` must be a 'dml_pretrend' object from pretrend_benchmark().")
  method      <- match.arg(method)
  which.bound <- match.arg(which.bound)
  lvl  <- if (method == "magnitude") x$level.magnitude   else x$level.factors
  bnd1 <- if (method == "magnitude") x$band.magnitude[1] else x$band.factors[1]
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
  if (isTRUE(band)) {
    graphics::contour(x_grid, y_grid, z_grid, levels = bnd1,
                      add = TRUE, col = col.band, lwd = lwd, lty = lty,
                      labels = lab(bnd1), labcex = labcex, ...)
  }
  invisible(x)
}
