##' @import ggplot2
##' @export
plot.dml <- function(x, combine.method = "median", level = 0.95,...) {
  estimate <- coef(x, combine.method = combine.method)
  conf <- confint(x, combine.method = combine.method, level = level)
  df <- data.frame(coefficient = names(estimate))
  df$estimate <- estimate
  df$lwr <- conf[,1]
  df$upr <- conf[,2]
  xlab <- "Parameter"
  ylab <- "Value"

  ggplot(df, aes(x = coefficient, y = estimate))+
    xlab(xlab) +
    ylab(ylab)+
    geom_errorbar(aes(ymin = lwr, ymax = upr, color = "Conf. Interval")) +
    geom_point(aes(color = "Estimate"), shape = 15, size = 4) +
    scale_color_manual(name = "Legend", values = c("blue", "black")) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "blank"),
                                                     shape = c(NA, 15))))+
    theme_bw()
}
