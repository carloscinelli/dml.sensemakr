#!/usr/bin/env Rscript
# Quick verification: seed=2 in dml_benchmark() matches set.seed(2) before dml_benchmark()

devtools::load_all("/home/user/dml.sensemakr")

outdir <- "/home/user/dml.sensemakr/tmp_plots"

sink(file.path(outdir, "comparison_tables.txt"))

data("pension")
y <- pension$net_tfa
d <- pension$e401
x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

# PLM
set.seed(1)
dml.401k.plm <- dml(y, d, x, model = "plm", cf.folds = 5, cf.reps = 5)

# NPM
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25, .5, .75, 1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE)
set.seed(1)
dml.401k.npm <- dml(y, d, x, groups = g1, model = "npm", cf.folds = 5, cf.reps = 5)


cat("================================================================\n")
cat("  VERIFY: seed parameter produces exact replication\n")
cat("================================================================\n\n")

# PLM: original approach (set.seed before call)
cat("===== PLM BENCHMARKS =====\n")
set.seed(2)
bench.plm.orig <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.orig <- summary(bench.plm.orig)

# PLM: new approach (seed parameter)
bench.plm.new <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"), seed = 2)
out.new <- summary(bench.plm.new)

cat("Original (set.seed(2) before call):\n")
print(out.orig)
cat("\nNew (seed = 2 parameter):\n")
print(out.new)
cat("\nDIFF:\n")
print(out.new - out.orig)
cat("Max abs diff:", max(abs(out.new - out.orig)), "\n")

# PLM: via sensemakr with benchmark_seed
sens.bench <- sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03,
                        benchmark_covariates = c("inc", "pira", "twoearn"),
                        benchmark_seed = 2)
out.sens <- summary(sens.bench$bench.bounds)
cat("\nVia sensemakr(benchmark_seed = 2):\n")
print(out.sens)
cat("\nDIFF vs original:\n")
print(out.sens - out.orig)
cat("Max abs diff:", max(abs(out.sens - out.orig)), "\n")

# PLM: income-only benchmark with seed
bench.inc.orig <- {set.seed(2); dml_benchmark(dml.401k.plm, benchmark_covariates = "inc")}
bench.inc.new <- dml_benchmark(dml.401k.plm, benchmark_covariates = "inc", seed = 2)
cat("\n\n===== PLM INCOME-ONLY BENCHMARK =====\n")
cat("Original:\n"); print(summary(bench.inc.orig))
cat("New:\n"); print(summary(bench.inc.new))
cat("DIFF:\n"); print(summary(bench.inc.new) - summary(bench.inc.orig))
cat("Max abs diff:", max(abs(summary(bench.inc.new) - summary(bench.inc.orig))), "\n")

# Cross-check: income row in full benchmark vs income-only
cat("\n===== INCOME ROW: full vs income-only (both seed=2) =====\n")
cat("Full benchmark inc row:\n"); print(out.new["inc", ])
cat("Income-only:\n"); print(summary(bench.inc.new)["inc", ])
cat("DIFF:\n"); print(summary(bench.inc.new)["inc", ] - out.new["inc", ])

# NPM
cat("\n\n===== NPM BENCHMARKS =====\n")
set.seed(2)
bench.npm.orig <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"))
bench.npm.new <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"), seed = 2)
cat("Original:\n"); print(summary(bench.npm.orig))
cat("New:\n"); print(summary(bench.npm.new))
cat("DIFF:\n"); print(summary(bench.npm.new) - summary(bench.npm.orig))
cat("Max abs diff:", max(abs(summary(bench.npm.new) - summary(bench.npm.orig))), "\n")


# Print all contour plot values for verification
cat("\n\n================================================================\n")
cat("  ALL CONTOUR BOUND VALUES (seed=2 benchmarks)\n")
cat("================================================================\n\n")

out <- summary(bench.plm.new)
cat("PLM Benchmark coordinates:\n")
for (cov in c("pira", "twoearn", "inc")) {
  cat(sprintf("  %s: gain.Y = %.10f, gain.D = %.10f\n", cov, out[cov, "gain.Y"], out[cov, "gain.D"]))
}

cat("\nPLM bound values (rho2=1):\n")
cat("  Max Match:   "); print(confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03))
cat("  1x pira:     "); print(confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"]))
cat("  1x twoearn:  "); print(confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"]))
cat("  1x inc:      "); print(confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"]))
cat("  1/4x inc:    "); print(confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25))

rho2 <- (1/2)^2
cat("\nPLM bound values (rho2=0.25):\n")
cat("  Max Match:   "); print(confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2))
cat("  1x pira:     "); print(confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"], rho2 = rho2))
cat("  1x twoearn:  "); print(confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"], rho2 = rho2))
cat("  1x inc:      "); print(confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"], rho2 = rho2))
cat("  1/4x inc:    "); print(confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25, rho2 = rho2))

sink()
cat("Saved: comparison_tables.txt\n")

# =============================================
# PLOTS: Original manual vs New automated
# Now both use seed=2, so benchmark numbers match.
# Label bumps differ (original has per-covariate bumps).
# =============================================

# Original manual style (with per-covariate label bumps)
png(file.path(outdir, "orig_plm_contour_lwr.png"), width = 600, height = 600, res = 100)
sens.401k <- sensemakr(dml.401k.plm, cf.y = NULL)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = .04, cf.d = .03),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"]),
                     label.bump.x = 0.02, label.bump.y = -0.002)
sensemakr::add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out["inc","gain.D"]*.25, r2yz.dx = out["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = -0.005)
title(main = "ORIGINAL: PLM lwr (rho2=1)")
dev.off()
cat("Saved: orig_plm_contour_lwr.png\n")

# NEW automated style (uniform label bumps)
png(file.path(outdir, "new_plm_contour_lwr.png"), width = 600, height = 600, res = 100)
sens.new <- sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03,
                      bound_label = "Max Match",
                      benchmark_covariates = c("inc", "pira", "twoearn"),
                      benchmark_seed = 2)
plot(sens.new, which.bound = "lwr", col.contour = "blue", lim.x = .16)
bench.inc <- dml_benchmark(dml.401k.plm, benchmark_covariates = "inc", seed = 2)
add_bound_to_contour(bench.inc, kd = 1/4, which.bound = "lwr")
title(main = "NEW: PLM lwr (rho2=1)")
dev.off()
cat("Saved: new_plm_contour_lwr.png\n")

# GATE plots
group.names <- paste0("gate.q", 1:4)
df <- data.frame(groups = 1:4, estimate = coef(dml.401k.npm)[group.names])
cis <- confint(dml.401k.npm)[group.names, ]
cis <- setNames(as.data.frame(cis), c("lwr.ci", "upr.ci"))
df <- cbind(df, cis)

library(ggplot2)
p1 <- ggplot(df, aes(x = groups, y = estimate)) + geom_line() +
  geom_ribbon(aes(ymin = lwr.ci, ymax = upr.ci), alpha = 0.1, col = "blue", fill = "blue") +
  theme_bw() + xlab("Income Groups by Quartiles") + ylab("ATE")
ggsave(file.path(outdir, "gate_plot.png"), p1, width = 4.5, height = 4.5, dpi = 150)

bds  <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = 0)
bds  <- setNames(as.data.frame(bds), c("lwr.bound", "upr.bound"))
cbds <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = .95)
cbds <- setNames(as.data.frame(cbds), c("lwr.cbound", "upr.cbound"))
df2  <- cbind(df, bds[-1, ], cbds[-1, ])

p2 <- ggplot(df2, aes(x = groups, y = estimate)) + geom_line() +
  geom_ribbon(aes(ymin = lwr.bound, ymax = upr.bound),   alpha = 0.1, col = "red", fill = "red") +
  geom_ribbon(aes(ymin = lwr.cbound, ymax = upr.cbound), alpha = 0.1, col = "blue", fill = "blue") +
  theme_bw() + xlab("Income Groups by Quartiles") + ylab("ATE")
ggsave(file.path(outdir, "gate_bounds_plot.png"), p2, width = 4.5, height = 4.5, dpi = 150)

cat("\n===== DONE =====\n")
