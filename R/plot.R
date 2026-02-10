##' Coefficient plots for DML and bounds
##'
##' @inheritParams print.dml
##' @param x an object of class \code{\link{dml}} or \code{\link{dml_bounds}}.
##' @returns For \code{plot.dml}: a \code{ggplot} object. For \code{plot.dml.bounds}: a \code{ggplot} object.
##' @import ggplot2
##' @export
plot.dml <- function(x, combine.method = "median", level = 0.95, ...) {
  estimate <- coef(x, combine.method = combine.method)
  conf <- confint(x, combine.method = combine.method, level = level)
  coef_plot(labels = names(estimate),
            estimate = estimate,
            legends  = c("Estimate", "Conf. Interval"),
            lwr1 = conf[,1], upr1 = conf[,2], ...)
}

##' @export
##' @param type type of plot for confidence bounds. Options are \code{confidence_bounds}, and \code{all}.
##' @rdname plot.dml
plot.dml.bounds <- function(x,
                            type = c("confidence_bounds","all"),
                            combine.method = "median", level = 0.95,
                            ...) {
  type <- match.arg(type)
  if (type == "confidence_bounds") {
    plot.bounds2(x = x, combine.method = combine.method, level = level,...)
  } else {
    plot.bounds1(x = x, combine.method = combine.method, level = level,...)
  }
}

#' @keywords internal
plot.bounds2 <- function(x, combine.method = "median", level = 0.95, ...){
  coef_plot(estimate = coef(x)["theta.s", ],
             labels =  names(coef(x)["theta.s", ]),
             lwr1 = coef(x)["theta.m", ],
             upr1 = coef(x)["theta.p", ],
             lwr2 = confidence_bounds(x)[,"lwr"],
             upr2 = confidence_bounds(x)[,"upr"], ...)
}

#' @keywords internal
plot.bounds1 <- function(x, combine.method = "median", level = 0.95,...){
  estimate <- coef(x, combine.method = combine.method)
  conf <- confint(x, combine.method = combine.method, level = level)
  labels <- c("Short Estimate", "|Bias| Bounds","Lower Bound", "Upper Bound")
  p <- vector(mode = "list", length = length(conf))
  for(i in 1:ncol(estimate)){
    p[[i]] <- coef_plot(labels = factor(labels, levels = labels),
                        estimate = estimate[,i],
                        title = names(conf)[i],
                        legends  = c("Estimate", "Conf. Interval"),
                        lwr1 = conf[[i]][,1],
                        upr1 = conf[[i]][,2], ...)
    print(p[[i]])
  }
}
# ##' @export
# coef_plot <- function(estimate, lwr, upr,
#                       labels = NULL,
#                       xlab = "Parameter",
#                       ylab = "Value",
#                       colors = c("blue", "black")){
#   if(is.null(labels)){
#     labels <- paste0("coef.", 1:length(estimate))
#   }
#   df <- data.frame(
#     coefficient = labels,
#     estimate = estimate,
#     lwr = lwr,
#     upr = upr)
#   ggplot(df, aes(x = coefficient, y = estimate))+
#     xlab(xlab) +
#     ylab(ylab)+
#     geom_errorbar(aes(ymin = lwr, ymax = upr, color = "Conf. Interval")) +
#     geom_point(aes(color = "Estimate"), shape = 15, size = 4) +
#     scale_color_manual(name = "Legend", values = colors) +
#     guides(colour = guide_legend(override.aes = list(linetype = c("solid", "blank"),
#                                                      shape = c(NA, 15))))+
#     theme_bw()
# }


##' Coefficient plot
##'
##' @description Creates a coefficient plot with estimates and confidence intervals using ggplot2.
##'
##' @param estimate numeric vector of point estimates.
##' @param lwr1 numeric vector of lower bounds for the first interval.
##' @param upr1 numeric vector of upper bounds for the first interval.
##' @param labels optional character vector of labels for each coefficient.
##' @param lwr2 optional numeric vector of lower bounds for the second interval.
##' @param upr2 optional numeric vector of upper bounds for the second interval.
##' @param text logical. Should text labels be added? Default is \code{TRUE}.
##' @param text.size numeric. Size of text labels. Default is \code{3}.
##' @param round integer. Number of digits to round displayed values. Default is \code{2}.
##' @param coord.flip logical. Should the plot coordinates be flipped? Default is \code{FALSE}.
##' @param title optional character string for the plot title.
##' @param legends character vector of legend labels. Default is \code{c("Estimate", "Bounds", "Conf. Bounds")}.
##' @param h0 numeric. Value for the null hypothesis line. Default is \code{0}. Set to \code{NULL} to suppress.
##' @param h0.color character. Color of the null hypothesis line. Default is \code{"darkorange"}.
##' @param bar.type character. Type of interval display: \code{"error_bar"} or \code{"linerange"}.
##' @param err.width numeric. Width of error bars. Default is \code{0.1}.
##' @param xlab character. X-axis label. Default is \code{"Parameter"}.
##' @param ylab character. Y-axis label. Default is \code{"Value"}.
##' @param lwd numeric vector of line widths.
##' @param colors character vector of colors.
##' @returns A \code{ggplot} object.
##' @export
coef_plot <- function(estimate,
                      lwr1, upr1,
                      labels = NULL,
                      lwr2 = NULL,
                      upr2 = NULL,
                      text = TRUE,
                      text.size = 3,
                      round = 2,
                      coord.flip = FALSE,
                      title = NULL,
                      legends = c("Estimate", "Bounds", "Conf. Bounds"),
                      h0 = 0,
                      h0.color = "darkorange",
                      bar.type = c("error_bar", "linerange"),
                      err.width = 0.1,
                      xlab = "Parameter",
                      ylab = "Value",
                      lwd   = c(.625, 1.25),
                      colors = c("black", "blue", "red")){
  if(is.null(labels)){
    labels <- paste0("coef.", 1:length(estimate))
  }

  bar.type <- match.arg(bar.type)

  df <- data.frame(coefficient = labels,
                   estimate = estimate,
                   lwr1 = lwr1,
                   upr1 = upr1)

  if(!is.null(lwr2) & !is.null(upr2)){
    df$lwr2 <- lwr2
    df$upr2 <- upr2
  }

  if(coord.flip){
    vjust <- c(-1, 2,2,-1,-1)
    hjust <- c(0, 0, 0,0, 0)
  } else {
    vjust <- c( .5, 2, -1, 2, -1)
    hjust <- c(-.5, .5,  .5, .5,  .5)
  }
  p <- ggplot(df, aes(x = coefficient, y = estimate))+ xlab(xlab) + ylab(ylab)

  if(!is.null(h0)){
    p <- p + geom_hline(yintercept = h0, colour = h0.color, lty = 2)
  }

  if(bar.type == "error_bar"){
    if(!is.null(lwr2) & !is.null(upr2)){
      p <- p + geom_errorbar(aes(ymin = lwr2, ymax = upr2, color = legends[3]), lwd = lwd[1], width =err.width)
    }
    p <-  p + geom_errorbar(aes(ymin = lwr1, ymax = upr1, color = legends[2]), lwd = lwd[2], width = err.width)
  } else{
    if(!is.null(lwr2) & !is.null(upr2)){
      p <- p + geom_linerange(aes(ymin = lwr2, ymax = upr2, color = legends[3]), lwd = lwd[1])
    }
    p <- p + geom_linerange(aes(ymin = lwr1, ymax = upr1, color = legends[2]), lwd = lwd[2])
  }

  p <- p + geom_point(aes(color = legends[1]), shape = 15, size = 4)

  if(text){
    p <- p + geom_text(aes(x = coefficient,
                       y = estimate,
                       label = round(estimate,round),
                       vjust = vjust[1], hjust = hjust[1]), size = text.size) +
      geom_text(aes(x = coefficient,
                    y = lwr1,
                    label = round(lwr1,round),
                    vjust = vjust[2], hjust = hjust[2]), size = text.size) +
      geom_text(aes(x = coefficient,
                    y = upr1,
                    label = round(upr1,round),
                    vjust = vjust[3], hjust = hjust[3]), size = text.size)
    if(!is.null(lwr2) & !is.null(upr2)){
      p <- p + geom_text(aes(x = coefficient,
                             y = lwr2,
                             label = round(lwr2,round),
                             vjust = vjust[4], hjust = hjust[4]), size = text.size) +
        geom_text(aes(x = coefficient,
                      y = upr2,
                      label = round(upr2,round),
                      vjust = vjust[5], hjust = hjust[5]), size = text.size)
    }
  }
  len <- 2
  if(!is.null(lwr2) & !is.null(upr2)){
    len <- 3
  }
  p <- p  + scale_color_manual(name = "Legend",
                              values = colors,
                              breaks = legends[1:len]) +
    scale_shape_manual(values = c(15,NA, NA)[1:len],
                       breaks = legends[1:len]) +
    scale_linetype_manual(values = c("blank","solid", "solid")[1:len],
                          breaks = legends[1:len]) +
    theme_bw()  +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 10))
  if(!is.null(title)){
   p <- p + ggtitle(title)
  }

  if(coord.flip){
    p <- p + coord_flip()
  }
  p
}


##' Contour plots of omitted variable bias for Causal Machine Learning
##'
##'@description Contour plots of omitted variable bias for sensitivity analysis. The main input is a \code{\link{dml}} model.
##'
##' The vertical axis shows the partial R2 of the omitted variables with the outcome, i.e, the maximum proportion of the residual variation of the outcome that could be explained by latent variables.
##'
##' The horizontal axis shows the proportion of variation in the long Riesz Representer which is not explained by the short Riesz Representer (RR). This indicates how much variation in the RR is created by latent variables. In the partial linear model, this quantity parallels the sensitivity parameter for the outcome, and simply equals the partial R2 of latent variables with the treatment, i.e, the maximum proportion of the residual variation of the treatment that could be explained by latent variables. In the non-parametric model with a binary treatment, the interpretation is analogous, but instead of gains in variance, it stands for the gains in precision (1/variance).
##'
##' The contour levels represent the lower (upper) limit of the confidence bound for the target of interest, considering omitted variables with the postulated strength.
##'
##'  The dotted red line shows the chosen critical threshold (for instance, zero).
##'
##'  Almost all parameters can be customized by the user.
##'
##' @returns No return value, called for side effects (creates a contour plot).
##' @export
ovb_contour_plot <- function(...){
  UseMethod("ovb_contour_plot")
}


##' @rdname ovb_contour_plot
##' @param model an object of class \code{\link{dml}}.
##' @param parameter the target parameter to plot. Options are \code{"ate"}, \code{"att"}, and \code{"atu"}.
##' @param which.bound show contours for the lower limit (\code{lwr}) or upper limit (\code{upr}) of confidence bounds?
##' @inheritParams dml_bounds
##' @inheritParams summary.dml
##' @param bound.label text label for the manual bound provided via \code{cf.y} and \code{cf.d}.
##' @param group contour plots for main analysis or group analysis? Default is \code{FALSE} (main analysis).
##' @param group.number if \code{group = TRUE}, provide the number (level) of the group.
##' @param threshold critical threshold of interest. Default is \code{0}.
##' @param lim.x  sets limit for x-axis. If `NULL`, limits are computed automatically.
##' @param lim.y  sets limit for y-axis. If `NULL`, limits are computed automatically.
##' @param cex.label.text size of the label text.
##' @param xlab label of x axis. If `NULL`, default label is used.
##' @param ylab label of y axis. If `NULL`, default label is used.
##' @param list.par  arguments to be passed to \code{\link{par}}. It needs to be a named list.
##' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
##' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
##' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
##' @param asp the y/x aspect ratio. Default is 1.
##' @param show.unadjusted should the unadjusted estimates be shown? Default is `TRUE`.
##' @param label.unadjusted label for the unadjusted estimate. Default is \code{"Unadjusted"}.
##' @param nlevels number of levels for the contour plot.
##' @param grid.number approximate number of grid points on each axis.
##' @param col.contour color of contour lines.
##' @param col.thr.line color of threshold contour line.
##' @param label.text should label texts be plotted? Default is \code{TRUE}.
##' @param cex.label.text  The magnification to be used for label text relative to the current setting of cex.
##' @param label.bump.x bump on the x coordinate of label text.
##' @param label.bump.y bump on the y coordinate of label text.
##' @param round number of digits to show in contours and bound values
##' @export
##' @exportS3Method sensemakr::ovb_contour_plot dml
ovb_contour_plot.dml <- function(model,
                                 parameter = c("ate", "att", "atu"),
                                 which.bound = c("lwr", "upr"),
                                 level = 0.95,
                                 combine.method = "median",
                                 rho2 = 1,
                                 cf.y = NULL,
                                 cf.d = cf.y,
                                 bound.label = "Scenario",
                                 benchmarks = NULL,
                                 kd = 1,
                                 ky = kd,
                                 group = FALSE,
                                 group.number = 1,
                                 threshold = 0,
                                 lim.x = 0.15,
                                 lim.y = lim.x,
                                 asp   = lim.x/lim.y,
                                 nlevels = 10,
                                 grid.number = 70,
                                 col.contour = "grey40",
                                 col.thr.line = "red",
                                 xlab = NULL,
                                 ylab = NULL,
                                 cex.lab = 0.8,
                                 cex.axis = 0.8,
                                 cex.main = 1,
                                 show.unadjusted = TRUE,
                                 label.unadjusted = "Observed",
                                 label.text = TRUE,
                                 round = 0,
                                 cex.label.text = 0.7,
                                 label.bump.x = NULL,
                                 label.bump.y = NULL,
                                 list.par = NULL, ...){

  which.bound <- match.arg(which.bound)

  check_r2(cf.y = cf.y, cf.d = cf.d)

  if (length(cf.y) != length(cf.d)) {
    stop("Lengths of cf.y and cf.d must match.")
  }

  if (lim.x > 1) {
    lim.x <- 1 - 1e-12
    warning("Contour limit larger than 1 was set to 1.")
  }

  if (lim.y > 1) {
    lim.y <- 1 - 1e-12
    warning("Contour limit larger than 1 was set to 1.")
  }

  if (lim.x < 0) {
    lim.x <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
  }

  if (lim.y < 0) {
    lim.y <- 0.4
    warning("Contour limit less than 0 was set to 0.4.")
  }

  if (is.null(label.bump.x)){
    label.bump.x <- lim.x*(1/15)
  }

  if (is.null(label.bump.y)){
    label.bump.y <- lim.y*(1/15)
  }

  parameter <- match.arg(parameter)
  parameter <- switch (parameter,
    ate = "all",
    att = "treat",
    atu = "untr"
  )
  results <- model$results$main[[parameter]]

  if(group) {
    results <- model$results$groups[[group.number]]
  }

  theta.s <- extract_estimate(results, "theta.s")
  S2 <- extract_estimate(results, "S2")
  se.theta.s <- extract_estimate(results, "se.theta.s")
  se.S2 <- extract_estimate(results, "se.S2")
  cov.theta.S2 <- extract_estimate(results, "cov.theta.S2")
  x_grid <- seq(0, lim.x, by = lim.x/grid.number)
  y_grid <- seq(0, lim.y,  by = lim.y/grid.number)
  vec_bounds <- Vectorize(confidence_bounds.numeric, vectorize.args = c("cf.y", "cf.d"))

  f <- function(x, y) {
    vec_bounds(theta.s = theta.s,S2 = S2,
               se.theta.s = se.theta.s,
               se.S2 = se.S2,
               cov.theta.S2 = cov.theta.S2,
               combine.method = combine.method,
               level = level,
               rho2 = rho2,
               cf.d = x,
               cf.y = y)[which.bound, ]
  }
  z_grid <- outer(X = x_grid, Y = y_grid, FUN = f)

  contour_plot(grid_values.x = x_grid,
               grid_values.y = y_grid,
               z_axis        = z_grid,
               threshold     = threshold,
               nlevels       = nlevels,
               grid.number   = grid.number,
               col.contour   = col.contour,
               col.thr.line  = col.thr.line,
               xlab          = xlab,
               ylab          = ylab,
               round         = round,
               cex.lab       = cex.lab,
               cex.axis      = cex.axis,
               cex.main      = cex.main,
               asp           = asp,
               list.par      = list.par)

  points(0, 0, pch = 17, col = "black", cex = 1)

  idx <- ifelse(group, group.number + 1, paste0("ate.", parameter))
  plot_estimate <- confint(model,
                           combine.method = combine.method,
                           level = level)[idx, ifelse(which.bound == "lwr", 1, 2)]
  text(0 + label.bump.x, 0 + label.bump.y,
       paste0(label.unadjusted,
              "\n(", round(plot_estimate, round), ")"),
       cex = cex.label.text)

  if (!is.null(cf.y)){
    bound_value <- f(x = cf.d, y = cf.y)

    sensemakr::add_bound_to_contour(r2dz.x = cf.d,
                                    r2yz.dx = cf.y,
                                    bound_value = bound_value,
                                    bound_label = bound.label,
                                    label.text = label.text,
                                    label.bump.x = label.bump.x,
                                    label.bump.y = label.bump.y,
                                    cex.label.text = cex.label.text,
                                    round = round)
  }

  # auto-overlay benchmark bounds (cross product of covariates x multipliers)
  if (!is.null(benchmarks)) {
    bench_summary <- summary(benchmarks)
    for (i in seq_len(nrow(bench_summary))) {
      covar_name <- rownames(bench_summary)[i]
      gain.Y <- bench_summary[i, "gain.Y"]
      gain.D <- bench_summary[i, "gain.D"]

      for (j in seq_along(kd)) {
        scaled.cf.d <- gain.D * kd[j]
        scaled.cf.y <- gain.Y * ky[j]
        bound_label_j <- label_maker(covar_name, kd[j], ky[j])
        bound_value_j <- f(x = scaled.cf.d, y = scaled.cf.y)

        sensemakr::add_bound_to_contour(r2dz.x = scaled.cf.d,
                                        r2yz.dx = scaled.cf.y,
                                        bound_value = bound_value_j,
                                        bound_label = bound_label_j,
                                        label.text = label.text,
                                        label.bump.x = label.bump.x,
                                        label.bump.y = label.bump.y,
                                        cex.label.text = cex.label.text,
                                        round = round)
      }
    }
  }

}


##' @export
add_bound_to_contour <- function(model, ...) {
  if (inherits(model, "dml")) {
    add_bound_to_contour.dml(model, ...)
  } else if (inherits(model, "dml_benchmark")) {
    add_bound_to_contour.dml_benchmark(model, ...)
  } else {
    sensemakr::add_bound_to_contour(model, ...)
  }
}

##' Add bound to an existing contour plot
##'
##' @description
##' This method allows you to easily add sensitivity bounds to an existing contour plot created with \code{\link{ovb_contour_plot}}.
##' It computes the bound value automatically from the \code{\link{dml}} model, so you only need to provide \code{cf.y} and \code{cf.d}.
##'
##' @param model an object of class \code{\link{dml}}.
##' @param cf.y partial R2 of omitted variables with the outcome.
##' @param cf.d strength of confounding in the Riesz representer.
##' @param rho2 degree of adversity. Default is 1.
##' @param which.bound which bound was used for the contour plot: \code{"lwr"} or \code{"upr"}.
##' @param bound_label text label for the bound.
##' @param level confidence level. Default is 0.95.
##' @param combine.method method for combining cross-fitting estimates.
##' @param ... additional arguments passed to \code{sensemakr::add_bound_to_contour}.
##' @exportS3Method sensemakr::add_bound_to_contour dml
add_bound_to_contour.dml <- function(model,
                                     cf.y,
                                     cf.d = cf.y,
                                     rho2 = 1,
                                     which.bound = c("lwr", "upr"),
                                     bound_label = "Scenario",
                                     level = 0.95,
                                     combine.method = "median",
                                     label.bump.x = 0.01,
                                     label.bump.y = 0.01,
                                     ...){
  which.bound <- match.arg(which.bound)
  results <- model$results$main[[1]]

  theta.s <- extract_estimate(results, "theta.s")
  S2 <- extract_estimate(results, "S2")
  se.theta.s <- extract_estimate(results, "se.theta.s")
  se.S2 <- extract_estimate(results, "se.S2")
  cov.theta.S2 <- extract_estimate(results, "cov.theta.S2")

  bound_value <- confidence_bounds.numeric(
    theta.s = theta.s, S2 = S2,
    se.theta.s = se.theta.s, se.S2 = se.S2,
    cov.theta.S2 = cov.theta.S2,
    combine.method = combine.method,
    level = level, rho2 = rho2,
    cf.d = cf.d, cf.y = cf.y
  )[which.bound]

  sensemakr::add_bound_to_contour(r2dz.x = cf.d,
                                  r2yz.dx = cf.y,
                                  bound_value = bound_value,
                                  bound_label = bound_label,
                                  label.bump.x = label.bump.x,
                                  label.bump.y = label.bump.y,
                                  ...)
}


##' Add benchmark bounds to an existing contour plot
##'
##' @description
##' This method allows you to add benchmark bounds from a \code{\link{dml_benchmark}} object to an existing contour plot.
##' It computes bound values automatically using the provided \code{model}. Multipliers \code{kd} and \code{ky} are applied
##' to all benchmark covariates (cross product), matching sensemakr OLS behavior.
##'
##' @param benchmarks an object of class \code{\link{dml_benchmark}}.
##' @param model an object of class \code{\link{dml}} used to compute bound values.
##' @param kd numeric vector of multipliers for the treatment sensitivity parameter. Default is 1.
##' @param ky numeric vector of multipliers for the outcome sensitivity parameter. Default is \code{kd}.
##' @param rho2 degree of adversity. Default is 1.
##' @param which.bound which bound was used for the contour plot: \code{"lwr"} or \code{"upr"}.
##' @param level confidence level. Default is 0.95.
##' @param combine.method method for combining cross-fitting estimates.
##' @param label.bump.x bump on the x coordinate of label text.
##' @param label.bump.y bump on the y coordinate of label text.
##' @param cex.label.text magnification for label text.
##' @param round number of digits to show in bound values.
##' @param label.text should label texts be plotted? Default is \code{TRUE}.
##' @param ... additional arguments passed to \code{sensemakr::add_bound_to_contour}.
##' @exportS3Method sensemakr::add_bound_to_contour dml_benchmark
add_bound_to_contour.dml_benchmark <- function(benchmarks,
                                               kd = 1,
                                               ky = kd,
                                               rho2 = 1,
                                               which.bound = c("lwr", "upr"),
                                               level = 0.95,
                                               combine.method = "median",
                                               label.bump.x = 0.01,
                                               label.bump.y = 0.01,
                                               cex.label.text = 0.7,
                                               round = 0,
                                               label.text = TRUE,
                                               ...){
  which.bound <- match.arg(which.bound)
  model <- benchmarks$model
  results <- model$results$main[[1]]

  theta.s <- extract_estimate(results, "theta.s")
  S2 <- extract_estimate(results, "S2")
  se.theta.s <- extract_estimate(results, "se.theta.s")
  se.S2 <- extract_estimate(results, "se.S2")
  cov.theta.S2 <- extract_estimate(results, "cov.theta.S2")

  vec_bounds <- Vectorize(confidence_bounds.numeric, vectorize.args = c("cf.y", "cf.d"))
  f <- function(x, y) {
    vec_bounds(theta.s = theta.s, S2 = S2,
               se.theta.s = se.theta.s, se.S2 = se.S2,
               cov.theta.S2 = cov.theta.S2,
               combine.method = combine.method,
               level = level, rho2 = rho2,
               cf.d = x, cf.y = y)[which.bound, ]
  }

  bench_summary <- summary(benchmarks)
  for (i in seq_len(nrow(bench_summary))) {
    covar_name <- rownames(bench_summary)[i]
    gain.Y <- bench_summary[i, "gain.Y"]
    gain.D <- bench_summary[i, "gain.D"]

    for (j in seq_along(kd)) {
      scaled.cf.d <- gain.D * kd[j]
      scaled.cf.y <- gain.Y * ky[j]
      bound_label_j <- label_maker(covar_name, kd[j], ky[j])
      bound_value_j <- f(x = scaled.cf.d, y = scaled.cf.y)

      sensemakr::add_bound_to_contour(r2dz.x = scaled.cf.d,
                                      r2yz.dx = scaled.cf.y,
                                      bound_value = bound_value_j,
                                      bound_label = bound_label_j,
                                      label.text = label.text,
                                      label.bump.x = label.bump.x,
                                      label.bump.y = label.bump.y,
                                      cex.label.text = cex.label.text,
                                      round = round,
                                      ...)
    }
  }
}


contour_plot <- function(grid_values.x,
                         grid_values.y,
                         z_axis,
                         threshold = 0,
                         nlevels = 10,
                         grid.number = 70,
                         col.contour = "grey40",
                         col.thr.line = "red",
                         xlab = NULL,
                         ylab = NULL,
                         round = 0,
                         cex.lab = 0.8,
                         cex.axis = 0.8,
                         cex.main = 1,
                         asp = 1,
                         list.par = NULL){

  if(is.null(xlab)) xlab <- expression(paste("1-",R[alpha%~%alpha[s]]^2))
  if(is.null(ylab)) ylab <- expression(paste(R[y-g[s]%~%g-g[s]]^2))
  # if(is.null(ylab)) ylab <- expression(paste(eta[Y%~%A~"|"~DX]^2))

  # Guard against degenerate z values (NaN/Inf/no variation)
  z_finite <- z_axis[is.finite(z_axis)]
  if (length(z_finite) == 0 || diff(range(z_finite)) == 0) {
    warning("Contour plot could not be drawn: z values are degenerate.")
    plot(range(grid_values.x), range(grid_values.y), type = "n",
         xlab = xlab, ylab = ylab, cex.lab = cex.lab,
         cex.axis = cex.axis, cex.main = cex.main, asp = asp)
    return(invisible(NULL))
  }

  default_levels <- pretty(range(z_axis), nlevels)
  too_close      <- abs(default_levels - threshold) < min(diff(default_levels)) * 0.25
  line_color     <- ifelse(too_close, "transparent", col.contour)
  line_type      <- ifelse(too_close, 1, 1)
  line_width     <- ifelse(too_close, 1, 1)

  # Plot contour plot:
  if (is.null(list.par)) {
    oldpar <- par(mar = c(5, 5, 4, 1) + .1, pty = "s")
    on.exit(par(oldpar))
  } else {
    if (!is.list(list.par)) stop("list.par needs to be a named list")
    oldpar <- do.call("par", list.par)
    on.exit(par(oldpar))
  }

  contour(grid_values.x, grid_values.y, z_axis,
          nlevels = nlevels,
          xlab = xlab,
          ylab = ylab,
          cex.lab = cex.lab,
          cex.axis = cex.axis,
          cex.main = cex.main,
          asp = asp,
          col = line_color,
          lty = line_type,
          lwd = line_width)

  contour(grid_values.x, grid_values.y, z_axis,
          level = threshold,
          label = round(threshold, digits = round),
          add = TRUE,
          col = col.thr.line,
          lwd = 2,
          lty = 2,
          cex.lab = cex.lab,
          cex.axis = cex.axis,
          cex.main = cex.main,
          asp = asp)
}

# Internal helper to validate R2 sensitivity parameters
check_r2 <- function(cf.y, cf.d) {
  if (!is.null(cf.y) && any(!is.numeric(cf.y) | cf.y < 0 | cf.y > 1))
    stop("cf.y must be numeric values in [0, 1]")
  if (!is.null(cf.d) && any(!is.numeric(cf.d) | cf.d < 0 | cf.d > 1))
    stop("cf.d must be numeric values in [0, 1]")
}
