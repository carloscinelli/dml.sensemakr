library(sensemakr)
devtools::load_all("/home/user/dml.sensemakr")

## loads data
data("pension")
y <- pension$net_tfa
d <- pension$e401
x <- model.matrix(~ -1 + age + inc + educ + fsize + marr + twoearn + pira + hown, data = pension)

# Fit models
set.seed(1)
dml.401k.plm <- dml(y, d, x, model = "plm", cf.folds = 5, cf.reps = 5)

g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25, .5, .75, 1), na.rm = TRUE),
          labels = c("q1", "q2", "q3", "q4"), include.lowest = TRUE)
set.seed(1)
dml.401k.npm <- dml(y, d, x, groups = g1, model = "npm", cf.folds = 5, cf.reps = 5)

# Benchmarks
bench.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = c("inc", "pira", "twoearn"), seed = 2)
bench.npm <- dml_benchmark(dml.401k.npm, benchmark_covariates = c("inc", "pira", "twoearn"), seed = 2)
bench.inc.plm <- dml_benchmark(dml.401k.plm, benchmark_covariates = "inc", seed = 2)
bench.inc.npm <- dml_benchmark(dml.401k.npm, benchmark_covariates = "inc", seed = 2)

out.plm <- summary(bench.plm)
out.npm <- summary(bench.npm)

######################################################
# PLM rho2=1: ORIGINAL vs NEW
######################################################
png("/home/user/dml.sensemakr/tmp_plots/orig_plm_contour_lwr.png", width = 600, height = 600)
sens.401k <- sensemakr(dml.401k.plm, cf.y = NULL)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
title(main = "ORIGINAL: PLM lwr (rho2=1)")
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = .04, cf.d = .03),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["pira","gain.D"], r2yz.dx = out.plm["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["pira","gain.Y"], cf.d = out.plm["pira","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["twoearn","gain.D"], r2yz.dx = out.plm["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["twoearn","gain.Y"], cf.d = out.plm["twoearn","gain.D"]),
                     label.bump.x = 0.02, label.bump.y = -0.002)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["inc","gain.D"], r2yz.dx = out.plm["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["inc","gain.Y"], cf.d = out.plm["inc","gain.D"]),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["inc","gain.D"]*.25,
                     r2yz.dx = out.plm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["inc","gain.Y"]*.25, cf.d = out.plm["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = -0.005)
dev.off()

png("/home/user/dml.sensemakr/tmp_plots/new_plm_contour_lwr.png", width = 600, height = 600)
sens.401k <- sensemakr(dml.401k.plm, cf.y = NULL)
plot(sens.401k, which.bound = "lwr", lim.x = 0.16, col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003)
title(main = "NEW: PLM lwr (rho2=1)")
add_bound_to_contour(dml.401k.plm, cf.y = 0.04, cf.d = 0.03,
                     bound_label = "Max Match",
                     label.bump.x = 0.015, label.bump.y = 0.003,
                     point.pch = 25, point.bg = "red")
add_bound_to_contour(bench.plm, which.bound = "lwr",
                     bound_label = c("1 x Income", "1 x Part. in IRA", "1 x Two Earners"),
                     label.bump.x = c(0.01, 0.01, 0.02),
                     label.bump.y = c(0.009, 0.009, -0.002))
add_bound_to_contour(bench.inc.plm, kd = 1/4, which.bound = "lwr",
                     bound_label = "1/4 x Income",
                     label.bump.x = 0.015, label.bump.y = -0.005)
dev.off()

######################################################
# PLM rho2=(1/2)^2: ORIGINAL vs NEW
######################################################
rho2 <- (1/2)^2

png("/home/user/dml.sensemakr/tmp_plots/orig_plm_contour_lwr_rho2.png", width = 600, height = 600)
sens.401k <- sensemakr(dml.401k.plm, cf.y = NULL, rho2 = rho2)
plot(sens.401k, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
title(main = "ORIGINAL: PLM lwr (rho2=0.25)")
sensemakr::add_bound_to_contour(r2dz.x = .03, r2yz.dx = .04,
                     bound_label = "Max Match",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = .04, cf.d = .03, rho2 = rho2),
                     point.pch = 25, point.bg = "red",
                     label.bump.x = 0.015, label.bump.y = 0.003)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["pira","gain.D"], r2yz.dx = out.plm["pira","gain.Y"],
                     bound_label = "1 x Part. in IRA",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["pira","gain.Y"], cf.d = out.plm["pira","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["twoearn","gain.D"], r2yz.dx = out.plm["twoearn","gain.Y"],
                     bound_label = "1 x Two Earners",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["twoearn","gain.Y"], cf.d = out.plm["twoearn","gain.D"], rho2 = rho2),
                     label.bump.x = 0.02, label.bump.y = -0.002)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["inc","gain.D"], r2yz.dx = out.plm["inc","gain.Y"],
                     bound_label = "1 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["inc","gain.Y"], cf.d = out.plm["inc","gain.D"], rho2 = rho2),
                     label.bump.x = 0.01, label.bump.y = 0.009)
sensemakr::add_bound_to_contour(r2dz.x = out.plm["inc","gain.D"]*.25,
                     r2yz.dx = out.plm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.plm, cf.y = out.plm["inc","gain.Y"]*.25, cf.d = out.plm["inc","gain.D"]*.25, rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = -0.005)
dev.off()

png("/home/user/dml.sensemakr/tmp_plots/new_plm_contour_lwr_rho2.png", width = 600, height = 600)
sens.401k.rho2 <- sensemakr(dml.401k.plm, cf.y = NULL, rho2 = rho2)
plot(sens.401k.rho2, which.bound = "lwr", lim.x = 0.16, col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003)
title(main = "NEW: PLM lwr (rho2=0.25)")
add_bound_to_contour(dml.401k.plm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2,
                     bound_label = "Max Match",
                     label.bump.x = 0.015, label.bump.y = 0.003,
                     point.pch = 25, point.bg = "red")
add_bound_to_contour(bench.plm, which.bound = "lwr", rho2 = rho2,
                     bound_label = c("1 x Income", "1 x Part. in IRA", "1 x Two Earners"),
                     label.bump.x = c(0.01, 0.01, 0.02),
                     label.bump.y = c(0.009, 0.009, -0.002))
add_bound_to_contour(bench.inc.plm, kd = 1/4, which.bound = "lwr", rho2 = rho2,
                     bound_label = "1/4 x Income",
                     label.bump.x = 0.015, label.bump.y = -0.005)
dev.off()

######################################################
# NPM rho2=1: ORIGINAL vs NEW
######################################################
png("/home/user/dml.sensemakr/tmp_plots/orig_npm_contour_lwr.png", width = 600, height = 600)
sens.npm <- sensemakr(dml.401k.npm, cf.y = NULL)
plot(sens.npm, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
title(main = "ORIGINAL: NPM lwr (rho2=1)")
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
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"]*.25,
                     r2yz.dx = out.npm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25),
                     label.bump.x = 0.015, label.bump.y = 0.00)
dev.off()

png("/home/user/dml.sensemakr/tmp_plots/new_npm_contour_lwr.png", width = 600, height = 600)
sens.npm <- sensemakr(dml.401k.npm, cf.y = NULL)
plot(sens.npm, which.bound = "lwr", lim.x = 0.16, col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003)
title(main = "NEW: NPM lwr (rho2=1)")
add_bound_to_contour(dml.401k.npm, cf.y = 0.04, cf.d = 0.03,
                     bound_label = "Max Match",
                     label.bump.x = 0.015, label.bump.y = 0.003,
                     point.pch = 25, point.bg = "red")
add_bound_to_contour(bench.npm, which.bound = "lwr",
                     bound_label = c("1 x Income", "1 x Part. in IRA", "1 x Two Earners"),
                     label.bump.x = c(-0.01, 0.005, 0.02),
                     label.bump.y = c(0.009, 0.009, 0.005))
add_bound_to_contour(bench.inc.npm, kd = 1/4, which.bound = "lwr",
                     bound_label = "1/4 x Income",
                     label.bump.x = 0.015, label.bump.y = 0.00)
dev.off()

######################################################
# NPM rho2=(1/2)^2: ORIGINAL vs NEW
######################################################
rho2 <- (1/2)^2

png("/home/user/dml.sensemakr/tmp_plots/orig_npm_contour_lwr_rho2.png", width = 600, height = 600)
sens.npm <- sensemakr(dml.401k.npm, cf.y = NULL, rho2 = rho2)
plot(sens.npm, which.bound = "lwr", col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003, lim.x = .16)
title(main = "ORIGINAL: NPM lwr (rho2=0.25)")
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
sensemakr::add_bound_to_contour(r2dz.x = out.npm["inc","gain.D"]*.25,
                     r2yz.dx = out.npm["inc","gain.Y"]*.25,
                     bound_label = "1/4 x Income",
                     bound_value = confidence_bounds(dml.401k.npm, cf.y = out.npm["inc","gain.Y"]*.25, cf.d = out.npm["inc","gain.D"]*.25, rho2 = rho2),
                     label.bump.x = 0.015, label.bump.y = 0.00)
dev.off()

png("/home/user/dml.sensemakr/tmp_plots/new_npm_contour_lwr_rho2.png", width = 600, height = 600)
sens.npm.rho2 <- sensemakr(dml.401k.npm, cf.y = NULL, rho2 = rho2)
plot(sens.npm.rho2, which.bound = "lwr", lim.x = 0.16, col.contour = "blue",
     label.bump.x = 0.015, label.bump.y = 0.003)
title(main = "NEW: NPM lwr (rho2=0.25)")
add_bound_to_contour(dml.401k.npm, cf.y = 0.04, cf.d = 0.03, rho2 = rho2,
                     bound_label = "Max Match",
                     label.bump.x = 0.015, label.bump.y = 0.003,
                     point.pch = 25, point.bg = "red")
add_bound_to_contour(bench.npm, which.bound = "lwr", rho2 = rho2,
                     bound_label = c("1 x Income", "1 x Part. in IRA", "1 x Two Earners"),
                     label.bump.x = c(-0.01, 0.01, 0.02),
                     label.bump.y = c(0.009, 0.009, 0.005))
add_bound_to_contour(bench.inc.npm, kd = 1/4, which.bound = "lwr", rho2 = rho2,
                     bound_label = "1/4 x Income",
                     label.bump.x = 0.015, label.bump.y = 0.00)
dev.off()

cat("All 8 plots generated successfully!\n")
