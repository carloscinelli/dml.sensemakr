#!/usr/bin/env Rscript
# ==================================================================
# Compare: ORIGINAL manual replication code (original_replication.R)
#      vs  NEW automated code (restat-replication.Rmd)
#
# Checks every numeric value and generates side-by-side plots.
# ==================================================================

library(dml.sensemakr)
library(ggplot2)

outdir <- "/home/user/dml.sensemakr/tmp_plots"

sink(file.path(outdir, "comparison_tables.txt"))

cat("================================================================\n")
cat("  COMPARISON: Original Manual Code vs New Automated Code\n")
cat("================================================================\n\n")

# ==========================
# DATA (identical in both)
# ==========================
data("pension")
y <- pension$net_tfa
d <- pension$e401
x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

cat("Naive estimate:", mean(y[d==1]) - mean(y[d==0]), "\n\n")

# ==========================
# PLM MODEL (both use set.seed(1))
# ==========================
cat("===== FITTING PLM (set.seed(1)) =====\n")
set.seed(1)
dml.401k.plm <- dml(y, d, x, model = "plm", cf.folds = 5, cf.reps = 5)

cat("\nPLM Summary:\n")
summary(dml.401k.plm)
cat("\nPLM confint:\n")
print(confint(dml.401k.plm))
cat("\nPLM robustness_value:\n")
print(robustness_value(dml.401k.plm))
cat("\nPLM confidence_bounds (level=0):\n")
print(confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03, level = 0))
cat("\nPLM confidence_bounds (level=0.95):\n")
print(confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03))
cat("\nPLM dml_bounds:\n")
print(dml_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03))
cat("\nPLM sensemakr summary:\n")
summary(sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03))

# ==========================
# NPM MODEL (both use set.seed(1))
# ==========================
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25, .5, .75, 1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE)

cat("\n\n===== FITTING NPM (set.seed(1)) =====\n")
set.seed(1)
dml.401k.npm <- dml(y, d, x, groups = g1, model = "npm", cf.folds = 5, cf.reps = 5)

cat("\nNPM Summary:\n")
summary(dml.401k.npm)
cat("\nNPM confint:\n")
print(confint(dml.401k.npm))
cat("\nNPM robustness_value:\n")
print(robustness_value(dml.401k.npm))
cat("\nNPM sensemakr summary:\n")
summary(sensemakr(dml.401k.npm, cf.y = 0.04, cf.d = 0.03))
cat("\nNPM dml_bounds:\n")
print(dml_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03))
cat("\nNPM confidence_bounds (level=0):\n")
print(confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = 0))
cat("\nNPM confidence_bounds (level=0.95):\n")
print(confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03))


# ==========================
# BENCHMARKS: original uses set.seed(2)
# ==========================
cat("\n\n================================================================\n")
cat("  BENCHMARK COMPARISON\n")
cat("================================================================\n\n")

# Original code: set.seed(2) before benchmarks
cat("===== PLM BENCHMARKS (set.seed(2), as in original) =====\n")
set.seed(2)
bench.plm.orig <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.plm.orig <- summary(bench.plm.orig)
cat("Original benchmark summary:\n")
print(out.plm.orig)

# New vignette: no set.seed before benchmarks (uses sensemakr with benchmark_covariates)
cat("\n===== PLM BENCHMARKS (no seed, as in new vignette via sensemakr) =====\n")
bench.plm.new <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.plm.new <- summary(bench.plm.new)
cat("New benchmark summary:\n")
print(out.plm.new)

cat("\n===== PLM BENCHMARK DIFF (new - original) =====\n")
print(out.plm.new - out.plm.orig)
cat("Max absolute diff:", max(abs(out.plm.new - out.plm.orig)), "\n")

# NPM benchmarks
cat("\n===== NPM BENCHMARKS (set.seed(2), as in original) =====\n")
set.seed(2)
bench.npm.orig <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.npm.orig <- summary(bench.npm.orig)
cat("Original benchmark summary:\n")
print(out.npm.orig)

cat("\n===== NPM BENCHMARKS (no seed, as in new vignette) =====\n")
bench.npm.new <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.npm.new <- summary(bench.npm.new)
cat("New benchmark summary:\n")
print(out.npm.new)

cat("\n===== NPM BENCHMARK DIFF (new - original) =====\n")
print(out.npm.new - out.npm.orig)
cat("Max absolute diff:", max(abs(out.npm.new - out.npm.orig)), "\n")


# ==========================
# CONTOUR PLOT NUMERIC VALUES
# Compare: original manual bound_value vs new automated
# ==========================
cat("\n\n================================================================\n")
cat("  CONTOUR PLOT NUMERIC VALUES: Manual vs Automated\n")
cat("================================================================\n\n")

# Use original benchmarks (set.seed(2))
set.seed(2)
bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
out <- summary(bench.plm)

cat("===== PLM: Benchmark coordinates (gain.Y, gain.D) =====\n")
cat("These are the r2yz.dx / r2dz.x values used in original code:\n\n")
for (cov in c("pira", "twoearn", "inc")) {
  cat(sprintf("  %s: gain.Y = %.6f, gain.D = %.6f\n", cov, out[cov, "gain.Y"], out[cov, "gain.D"]))
}

cat("\n===== PLM: Manual bound_value (original code) vs automated =====\n\n")

# Original code passes confidence_bounds() result directly as bound_value
# bound_value in original = confidence_bounds(model, cf.y=..., cf.d=...)
# which returns a matrix with lwr/upr columns (and rows for each target)

cat("-- Max Match (cf.y=0.04, cf.d=0.03, rho2=1) --\n")
bv_manual <- confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03)
cat("  Manual bound_value:\n"); print(bv_manual)

cat("\n-- 1x pira --\n")
bv_pira <- confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"])
cat("  Manual bound_value:\n"); print(bv_pira)

cat("\n-- 1x twoearn --\n")
bv_twoearn <- confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"])
cat("  Manual bound_value:\n"); print(bv_twoearn)

cat("\n-- 1x inc --\n")
bv_inc <- confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"])
cat("  Manual bound_value:\n"); print(bv_inc)

cat("\n-- 1/4 x inc --\n")
bv_inc25 <- confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25)
cat("  Manual bound_value:\n"); print(bv_inc25)

cat("\n-- rho2 = (1/2)^2 versions --\n")
rho2 <- (1/2)^2
bv_mm_rho <- confidence_bounds(dml.401k.plm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2)
bv_pira_rho <- confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"], rho2 = rho2)
bv_twoearn_rho <- confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"], rho2 = rho2)
bv_inc_rho <- confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"], rho2 = rho2)
bv_inc25_rho <- confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25, rho2 = rho2)
cat("  Max Match (rho2=0.25):\n"); print(bv_mm_rho)
cat("  1x pira (rho2=0.25):\n"); print(bv_pira_rho)
cat("  1x twoearn (rho2=0.25):\n"); print(bv_twoearn_rho)
cat("  1x inc (rho2=0.25):\n"); print(bv_inc_rho)
cat("  1/4x inc (rho2=0.25):\n"); print(bv_inc25_rho)


# NPM benchmark values
cat("\n\n===== NPM: Benchmark coordinates (gain.Y, gain.D) =====\n")
set.seed(2)
bench.npm <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.npm <- summary(bench.npm)
for (cov in c("pira", "twoearn", "inc")) {
  cat(sprintf("  %s: gain.Y = %.6f, gain.D = %.6f\n", cov, out.npm[cov, "gain.Y"], out.npm[cov, "gain.D"]))
}

cat("\n===== NPM: Manual bound_value =====\n")
bv_npm_mm <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03)
bv_npm_pira <- confidence_bounds(dml.401k.npm, cf.y = out.npm["pira","gain.Y"], cf.d = out.npm["pira","gain.D"])
bv_npm_twoearn <- confidence_bounds(dml.401k.npm, cf.y = out.npm["twoearn","gain.Y"], cf.d = out.npm["twoearn","gain.D"])
bv_npm_inc <- confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"], cf.d = out.npm["inc","gain.D"])
bv_npm_inc25 <- confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25)
cat("  Max Match:\n"); print(bv_npm_mm)
cat("  1x pira:\n"); print(bv_npm_pira)
cat("  1x twoearn:\n"); print(bv_npm_twoearn)
cat("  1x inc:\n"); print(bv_npm_inc)
cat("  1/4x inc:\n"); print(bv_npm_inc25)

rho2 <- (1/2)^2
bv_npm_mm_rho <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2)
bv_npm_inc_rho <- confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"], cf.d = out.npm["inc","gain.D"], rho2 = rho2)
bv_npm_inc25_rho <- confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25, rho2 = rho2)
cat("  Max Match (rho2=0.25):\n"); print(bv_npm_mm_rho)
cat("  1x inc (rho2=0.25):\n"); print(bv_npm_inc_rho)
cat("  1/4x inc (rho2=0.25):\n"); print(bv_npm_inc25_rho)


# ==========================
# GATE DATA (identical code in both)
# ==========================
cat("\n\n===== GATE DATA =====\n")
group.names <- paste0("gate.q", 1:4)
cat("Coefficients:\n")
print(coef(dml.401k.npm)[group.names])
cat("\nConfidence intervals:\n")
print(confint(dml.401k.npm)[group.names, ])

bds  <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = 0)
cbds <- confidence_bounds(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, level = .95)
cat("\nGATE bounds (level=0):\n"); print(bds)
cat("\nGATE confidence bounds (level=0.95):\n"); print(cbds)

sink()
cat("Saved: comparison_tables.txt\n")


# ==========================
# PLOTS
# ==========================

# Use original benchmarks (set.seed(2))
set.seed(2)
bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"))
out <- summary(bench.plm)

# --- PLM: Original manual contour (rho2=1, lower bound) ---
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

# --- NEW automated PLM contour (rho2=1, lower bound) ---
png(file.path(outdir, "new_plm_contour_lwr.png"), width = 600, height = 600, res = 100)
sens.new <- sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03,
                      bound_label = "Max Match",
                      benchmark_covariates = c("inc", "pira", "twoearn"))
plot(sens.new, which.bound = "lwr", col.contour = "blue", lim.x = .16)
# Also add 1/4 x Income manually
bench.inc <- dml_benchmark(dml.401k.plm, benchmark_covariates = "inc")
add_bound_to_contour(bench.inc, kd = 1/4, which.bound = "lwr")
title(main = "NEW: PLM lwr (rho2=1)")
dev.off()
cat("Saved: new_plm_contour_lwr.png\n")


# --- PLM rho2 = (1/2)^2 ---
rho2 <- (1/2)^2
png(file.path(outdir, "orig_plm_contour_lwr_rho2.png"), width = 600, height = 600, res = 100)
sens.401k <- sensemakr(dml.401k.plm, cf.y = NULL, rho2 = rho2)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = .04, cf.d = .03, rho2 = rho2),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out["pira","gain.D"], r2yz.dx = out["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["pira","gain.Y"], cf.d = out["pira","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out["twoearn","gain.D"], r2yz.dx = out["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["twoearn","gain.Y"], cf.d = out["twoearn","gain.D"], rho2 = rho2),
                     label.bump.x = 0.02, label.bump.y = -0.002)
sensemakr::add_bound_to_contour(r2dz.x = out["inc","gain.D"], r2yz.dx = out["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"], cf.d = out["inc","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out["inc","gain.D"]*.25, r2yz.dx = out["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out["inc","gain.Y"]*.25, cf.d = out["inc","gain.D"]*.25, rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = -0.005)
title(main = "ORIGINAL: PLM lwr (rho2=0.25)")
dev.off()
cat("Saved: orig_plm_contour_lwr_rho2.png\n")

# NEW rho2
png(file.path(outdir, "new_plm_contour_lwr_rho2.png"), width = 600, height = 600, res = 100)
sens.new.rho <- sensemakr(dml.401k.plm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2,
                           bound_label = "Max Match",
                           benchmark_covariates = c("inc", "pira", "twoearn"))
plot(sens.new.rho, which.bound = "lwr", col.contour = "blue", lim.x = .16)
bench.inc <- dml_benchmark(dml.401k.plm, benchmark_covariates = "inc")
add_bound_to_contour(bench.inc, kd = 1/4, which.bound = "lwr", rho2 = rho2)
title(main = "NEW: PLM lwr (rho2=0.25)")
dev.off()
cat("Saved: new_plm_contour_lwr_rho2.png\n")


# --- NPM contour ---
set.seed(2)
bench.npm <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"))
out.npm <- summary(bench.npm)

png(file.path(outdir, "orig_npm_contour_lwr.png"), width = 600, height = 600, res = 100)
sens.401k <- sensemakr(dml.401k.npm, cf.y = NULL)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = .04, cf.d = .03),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["pira","gain.D"], r2yz.dx = out.npm["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["pira","gain.Y"], cf.d = out.npm["pira","gain.D"]),
                     label.bump.x = 0.005, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["twoearn","gain.D"], r2yz.dx = out.npm["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["twoearn","gain.Y"], cf.d = out.npm["twoearn","gain.D"]),
                     label.bump.x = 0.02, label.bump.y = 0.005)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"], r2yz.dx = out.npm["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"], cf.d = out.npm["inc","gain.D"]),
                     label.bump.x = -0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"]*.25, r2yz.dx = out.npm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = 0.00)
title(main = "ORIGINAL: NPM lwr (rho2=1)")
dev.off()
cat("Saved: orig_npm_contour_lwr.png\n")

png(file.path(outdir, "new_npm_contour_lwr.png"), width = 600, height = 600, res = 100)
sens.npm.new <- sensemakr(dml.401k.npm, cf.y = 0.04, cf.d = 0.03,
                           bound_label = "Max Match",
                           benchmark_covariates = c("inc", "pira", "twoearn"))
plot(sens.npm.new, which.bound = "lwr", col.contour = "blue", lim.x = .16)
bench.inc.npm <- dml_benchmark(dml.401k.npm, benchmark_covariates = "inc")
add_bound_to_contour(bench.inc.npm, kd = 1/4, which.bound = "lwr")
title(main = "NEW: NPM lwr (rho2=1)")
dev.off()
cat("Saved: new_npm_contour_lwr.png\n")

# NPM rho2
rho2 <- (1/2)^2
png(file.path(outdir, "orig_npm_contour_lwr_rho2.png"), width = 600, height = 600, res = 100)
sens.401k <- sensemakr(dml.401k.npm, cf.y = NULL, rho2 = rho2)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = .04, cf.d = .03, rho2 = rho2),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["pira","gain.D"], r2yz.dx = out.npm["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["pira","gain.Y"], cf.d = out.npm["pira","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["twoearn","gain.D"], r2yz.dx = out.npm["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["twoearn","gain.Y"], cf.d = out.npm["twoearn","gain.D"], rho2 = rho2),
                     label.bump.x = 0.02, label.bump.y = 0.005)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"], r2yz.dx = out.npm["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"], cf.d = out.npm["inc","gain.D"], rho2 = rho2),
                     label.bump.x = -0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"]*.25, r2yz.dx = out.npm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25, rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = 0.00)
title(main = "ORIGINAL: NPM lwr (rho2=0.25)")
dev.off()
cat("Saved: orig_npm_contour_lwr_rho2.png\n")

png(file.path(outdir, "new_npm_contour_lwr_rho2.png"), width = 600, height = 600, res = 100)
sens.npm.rho <- sensemakr(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2,
                           bound_label = "Max Match",
                           benchmark_covariates = c("inc", "pira", "twoearn"))
plot(sens.npm.rho, which.bound = "lwr", col.contour = "blue", lim.x = .16)
add_bound_to_contour(bench.inc.npm, kd = 1/4, which.bound = "lwr", rho2 = rho2)
title(main = "NEW: NPM lwr (rho2=0.25)")
dev.off()
cat("Saved: new_npm_contour_lwr_rho2.png\n")


# --- GATE plots (identical code in both) ---
group.names <- paste0("gate.q", 1:4)
df <- data.frame(groups = 1:4, estimate = coef(dml.401k.npm)[group.names])
cis <- confint(dml.401k.npm)[group.names, ]
cis <- setNames(as.data.frame(cis), c("lwr.ci", "upr.ci"))
df <- cbind(df, cis)

p1 <- ggplot(df, aes(x = groups, y = estimate)) + geom_line() +
  geom_ribbon(aes(ymin = lwr.ci, ymax = upr.ci), alpha = 0.1, col = "blue", fill = "blue") +
  theme_bw() + xlab("Income Groups by Quartiles") + ylab("ATE")
ggsave(file.path(outdir, "gate_plot.png"), p1, width = 4.5, height = 4.5, dpi = 150)
cat("Saved: gate_plot.png\n")

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
cat("Saved: gate_bounds_plot.png\n")


cat("\n===== ALL DONE =====\n")
cat("Files in tmp_plots/:\n")
cat("  comparison_tables.txt       - all numeric values\n")
cat("  orig_plm_contour_lwr.png    - original PLM contour (rho2=1)\n")
cat("  new_plm_contour_lwr.png     - new automated PLM contour (rho2=1)\n")
cat("  orig_plm_contour_lwr_rho2.png - original PLM contour (rho2=0.25)\n")
cat("  new_plm_contour_lwr_rho2.png  - new automated PLM contour (rho2=0.25)\n")
cat("  orig_npm_contour_lwr.png    - original NPM contour (rho2=1)\n")
cat("  new_npm_contour_lwr.png     - new automated NPM contour (rho2=1)\n")
cat("  orig_npm_contour_lwr_rho2.png - original NPM contour (rho2=0.25)\n")
cat("  new_npm_contour_lwr_rho2.png  - new automated NPM contour (rho2=0.25)\n")
cat("  gate_plot.png               - GATE plot\n")
cat("  gate_bounds_plot.png        - GATE bounds plot\n")
