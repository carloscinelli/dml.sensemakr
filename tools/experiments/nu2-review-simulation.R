# Focused review of the nu2 floors introduced in 27f58b4351.
# Run from the repository root with Rscript tools/experiments/nu2-review-simulation.R.
# Uses the actual source at the reviewed revision, without caret or refitting Y.
# Set NU2_SOURCE_REV=worktree to run the working tree; NU2_MC_REPS controls MC size.
# Results are Monte Carlo diagnostics, not new package estimators or tests.

revision <- Sys.getenv("NU2_SOURCE_REV", "27f58b4351eb9e13179de7b670517c2f4020d6a1")
B <- as.integer(Sys.getenv("NU2_MC_REPS", "2000"))
set.seed(20261023)
code <- new.env(parent = globalenv())
for (file in c("R/short-parameters.R", "R/bias-bounds.R", "R/dml.R",
               "R/print-summary-dml.R")) {
  txt <- if (revision == "worktree") readLines(file, warn = FALSE) else
    system2("git", c("show", paste0(revision, ":", file)), stdout = TRUE)
  # These two unrelated re-exports need sensemakr; this experiment never uses them.
  txt <- txt[!grepl("^[A-Za-z_]+ <- sensemakr::", txt)]
  eval(parse(text = txt), envir = code)
}
outdir <- "tools/experiments"
z <- qnorm(.975)
bf <- code$bias.factor(.04, .03)

# Two folds; saturated cell-mean propensity regressions and out-of-fold shares.
crossfit_means <- function(d, cell) {
  n <- length(d)
  fold <- rep(1:2, each = n / 2)
  dhat <- phat <- numeric(n)
  for (k in 1:2) {
    train <- fold != k
    phat[!train] <- mean(d[train])
    means <- tapply(d[train], cell[train], mean)
    dhat[!train] <- unname(means[as.character(cell[!train])])
  }
  list(dhat = dhat, phat = phat)
}

collect <- function(fit, truth, floor, theta = 1) {
  e <- fit$estimates
  b <- code$bounds(fit)$estimates
  c(nu = e$nu2.s, se.nu = e$se.nu2.s,
    bind = as.numeric(abs(e$nu2.s - floor) < 1e-12),
    cover.nu = as.numeric(abs(e$nu2.s - truth) <= z * e$se.nu2.s),
    lower = b$theta.m, se.lower = b$se.theta.m,
    cover.lower = as.numeric(abs(b$theta.m - (theta - bf * sqrt(truth))) <=
                              z * b$se.theta.m),
    upper = b$theta.p, se.upper = b$se.theta.p,
    cover.upper = as.numeric(abs(b$theta.p - (theta + bf * sqrt(truth))) <=
                              z * b$se.theta.p))
}

summarize <- function(mat, label, regime, n, truth) {
  data.frame(model = label, regime = regime, n = n, repetitions = nrow(mat),
             nu.true = truth, nu.bias = mean(mat[, "nu"]) - truth,
             bind.rate = mean(mat[, "bind"]),
             nu.sd = sd(mat[, "nu"]), nu.mean.se = mean(mat[, "se.nu"]),
             nu.coverage = mean(mat[, "cover.nu"]),
             lower.sd = sd(mat[, "lower"]), lower.mean.se = mean(mat[, "se.lower"]),
             lower.coverage = mean(mat[, "cover.lower"]),
             upper.sd = sd(mat[, "upper"]), upper.mean.se = mean(mat[, "se.upper"]),
             upper.coverage = mean(mat[, "cover.upper"]))
}

regular <- list()
for (n in c(500L, 2000L, 8000L)) {
  for (regime in c("boundary", "interior", "local_n_quarter", "local_n_half")) {
    p <- .3
    a <- switch(regime, boundary = 0, interior = .12,
                local_n_quarter = .5 * n^(-1/4), local_n_half = n^(-1/2))
    es <- p + a * c(-1, 1)
    true.nu <- c(ATE = mean(1 / (es * (1 - es))),
                 ATT = mean(es / (1 - es)) / p^2,
                 ATU = mean((1 - es) / es) / (1 - p)^2,
                 conditional_ATT = (1 - p) / p^2 * mean(es^2 / (1 - es)),
                 conditional_ATU = p / (1 - p)^2 * mean((1 - es)^2 / es),
                 PLM_continuous = 1)
    values <- lapply(true.nu, function(.) matrix(NA_real_, B, 10,
      dimnames = list(NULL, c("nu", "se.nu", "bind", "cover.nu", "lower", "se.lower",
                             "cover.lower", "upper", "se.upper", "cover.upper"))))
    for (b in seq_len(B)) {
      x <- sample(c(-1, 1), n, replace = TRUE)
      e <- p + a * x
      d <- rbinom(n, 1, e)
      y <- d + x + rnorm(n)
      pred <- crossfit_means(d, x)
      floor <- 1 / (mean(d) * (1 - mean(d)))
      for (tg in c(ATE = "all", ATT = "treat", ATU = "untr")) {
        label <- c(all = "ATE", treat = "ATT", untr = "ATU")[[tg]]
        fit <- code$ate.npm(y, d, tg, x + 1, x, pred$dhat, pred$phat)
        values[[label]][b, ] <- collect(fit, true.nu[[label]], floor)
      }
      fit <- code$att.npm.cond(y, d, yhat0 = x, dhat = pred$dhat, phat = pred$phat)
      values$conditional_ATT[b, ] <- collect(fit, true.nu[["conditional_ATT"]], 1)
      fit <- code$atu.npm.cond(y, d, yhat1 = x + 1, dhat = pred$dhat, phat = pred$phat)
      values$conditional_ATU[b, ] <- collect(fit, true.nu[["conditional_ATU"]], 1)

      d.cont <- a * x + rnorm(n)
      y.cont <- d.cont + x + rnorm(n)
      pred.cont <- crossfit_means(d.cont, x)
      fit <- code$ate.plm(y.cont, d.cont, yhat = (a + 1) * x, dhat = pred.cont$dhat)
      values$PLM_continuous[b, ] <- collect(fit, 1,
        1 / mean((d.cont - mean(d.cont))^2))
    }
    regular <- c(regular, lapply(names(values), function(label)
      summarize(values[[label]], label, regime, n, true.nu[[label]])))
    cat("Regular:", regime, "n =", n, "complete\n")
  }
}
regular <- do.call(rbind, regular)
write.csv(regular, file.path(outdir, "nu2-review-regular.csv"), row.names = FALSE)

# Group-specific calls use their own empirical shares and sample sizes.
# G is a component of X. Global nuisance learning is saturated in (G, X).
groups.out <- list()
n <- 4000L
for (regime in c("boundary", "interior")) {
  a <- if (regime == "boundary") 0 else .1
  values <- lapply(1:2, function(.) matrix(NA_real_, B, 10,
    dimnames = list(NULL, colnames(values[[1]]))))
  for (b in seq_len(B)) {
    g <- factor(sample(c("a", "b"), n, replace = TRUE))
    x <- sample(c(-1, 1), n, replace = TRUE)
    p <- ifelse(g == "a", .25, .65)
    d <- rbinom(n, 1, p + a * x)
    y <- d + x + rnorm(n)
    pred <- crossfit_means(d, interaction(g, x))
    mock <- list(data = list(y = y, d = d, x = cbind(x = x, g = as.numeric(g))),
                 info = list(cf.reps = 1, target = "ate", conditional = FALSE),
                 fits = list(list(preds = c(pred, list(yhat0 = x, yhat1 = x + 1)))))
    fits <- code$group.ate.npm(mock, g)
    for (j in 1:2) {
      pg <- c(.25, .65)[j]
      eg <- pg + a * c(-1, 1)
      dg <- d[g == levels(g)[j]]
      values[[j]][b, ] <- collect(fits[[j]][[1]], mean(1 / (eg * (1 - eg))),
                                  1 / (mean(dg) * (1 - mean(dg))))
    }
  }
  for (j in 1:2) {
    eg <- c(.25, .65)[j] + a * c(-1, 1)
    groups.out[[length(groups.out) + 1L]] <- summarize(values[[j]],
      paste0("group_ATE_", j), regime, n, mean(1 / (eg * (1 - eg))))
  }
  cat("Groups:", regime, "complete\n")
}
write.csv(do.call(rbind, groups.out), file.path(outdir, "nu2-review-groups.csv"), row.names = FALSE)

# Balanced ATE: the first-order nu influence function vanishes. The empirical
# floor alone has n * (floor - 4) -> 4 * Z^2, so a root-n normal statement is
# degenerate; studentized nu intervals need a separate second-order argument.
degenerate <- list()
for (n in c(500L, 2000L, 8000L)) {
  val <- replicate(2 * B, {
    d <- rbinom(n, 1, .5)
    pred <- crossfit_means(d, rep(1, n))
    fit <- code$ate.npm(rnorm(n), d, "all", rep(0, n), rep(0, n), pred$dhat, pred$phat)
    e <- fit$estimates
    c(n.scaled.error = n * (e$nu2.s - 4), n.scaled.se = n * e$se.nu2.s,
      coverage = as.numeric(abs(e$nu2.s - 4) <= z * e$se.nu2.s))
  })
  degenerate[[length(degenerate) + 1L]] <- data.frame(n = n, repetitions = ncol(val),
    mean.n.error = mean(val[1, ]), sd.n.error = sd(val[1, ]),
    mean.n.se = mean(val[2, ]), nu.coverage = mean(val[3, ]))
}
write.csv(do.call(rbind, degenerate), file.path(outdir, "nu2-review-degenerate.csv"), row.names = FALSE)

# Exact centered null with an estimated propensity slope and known p = 1/2.
# With m=n/2, beta_j=mean((2D-1)X) in fold j, and out-of-fold
# ehat=.5+.5*beta_other*X, put A=(sqrt(m)*beta1+sqrt(m)*beta2)/sqrt(2),
# B=(sqrt(m)*beta1-sqrt(m)*beta2)/sqrt(2), W=A^2-3B^2.
# Then n*nu_hat ->4*max(W,0), P(W>0)=1/3. The square-root scale
# has a root-n non-Gaussian limit even though nu_hat is root-n degenerate.
centered <- list()
for (n in c(500L, 2000L, 8000L)) {
  m <- n / 2
  val <- replicate(3 * B, {
    x <- sample(c(-1, 1), n, replace = TRUE)
    d <- rbinom(n, 1, .5)
    u <- (2 * d - 1) * x
    beta <- c(mean(u[seq_len(m)]), mean(u[m + seq_len(m)]))
    dhat <- .5 + .5 * rep(rev(beta), each = m) * x
    fit <- code$att.npm.cond(rnorm(n), d, yhat0 = rep(0, n),
                             dhat = dhat, phat = .5, centered = TRUE)
    A <- sqrt(m / 2) * sum(beta)
    Bb <- sqrt(m / 2) * (beta[1] - beta[2])
    W <- A^2 - 3 * Bb^2
    e <- fit$estimates
    bounds <- code$bounds(fit, cf.y = 1, cf.d = .5)$estimates # BF=1
    c(positive = as.numeric(e$nu2.s > 0),
      n.nu = n * e$nu2.s, n.nu.limit = 4 * max(W, 0),
      rootn.S = sqrt(n * e$S2), rootn.S.limit = 2 * sqrt(max(W, 0)),
      n.se.nu = n * e$se.nu2.s, n.se.nu.limit = 8 * sqrt(A^2 + Bb^2),
      finite.se = as.numeric(is.finite(bounds$se.theta.m)),
      lower.coverage = as.numeric(abs(bounds$theta.m) <= z * bounds$se.theta.m),
      upper.coverage = as.numeric(abs(bounds$theta.p) <= z * bounds$se.theta.p))
  })
  centered[[length(centered) + 1L]] <- data.frame(n = n, repetitions = ncol(val),
    positive.rate = mean(val["positive", ]), limit.positive.rate = mean(val["n.nu.limit", ] > 0),
    mean.n.nu = mean(val["n.nu", ]), mean.limit.n.nu = mean(val["n.nu.limit", ]),
    mae.n.nu.vs.limit = mean(abs(val["n.nu", ] - val["n.nu.limit", ])),
    mean.rootn.S = mean(val["rootn.S", ]),
    mae.rootn.S.vs.limit = mean(abs(val["rootn.S", ] - val["rootn.S.limit", ])),
    finite.se.rate = mean(val["finite.se", ]),
    available.lower.coverage = mean(val["lower.coverage", ], na.rm = TRUE),
    available.upper.coverage = mean(val["upper.coverage", ], na.rm = TRUE))
  cat("Centered null: n =", n, "complete\n")
}
write.csv(do.call(rbind, centered), file.path(outdir, "nu2-review-centered.csv"), row.names = FALSE)

# Persistent trimming violates the consistent-Riesz nuisance premise. At p=.01,
# clipping oracle propensities to .02 makes the floor bind with probability one.
# The actual sampling variation is then that of 1/[p_hat(1-p_hat)], whereas
# retained clipped-score variation is about a quarter as large.
trimmed <- list()
n <- 10000L
for (trim in c(.001, .02)) {
  val <- replicate(2 * B, {
    d <- rbinom(n, 1, .01)
    y <- d + rnorm(n)
    fit <- code$ate.npm(y, d, "all", rep(1, n), rep(0, n),
                        dhat = rep(.01, n), phat = .01, trim = trim)
    e <- fit$estimates
    pg <- mean(d)
    floor.se <- abs(2 * pg - 1) / (pg * (1 - pg))^(3/2) / sqrt(n)
    c(nu = e$nu2.s, se = e$se.nu2.s, floor.se = floor.se,
      bind = as.numeric(abs(e$nu2.s - 1 / (pg * (1 - pg))) < 1e-10),
      coverage = as.numeric(abs(e$nu2.s - 1 / (.01 * .99)) <= z * e$se.nu2.s))
  })
  trimmed[[length(trimmed) + 1L]] <- data.frame(trim = trim, n = n, repetitions = ncol(val),
    nu.true = 1 / (.01 * .99), nu.bias = mean(val["nu", ]) - 1 / (.01 * .99),
    bind.rate = mean(val["bind", ]), nu.sd = sd(val["nu", ]),
    nu.mean.se = mean(val["se", ]), mean.floor.se = mean(val["floor.se", ]),
    mean.se.ratio.to.floor = mean(val["se", ] / val["floor.se", ]),
    nu.coverage = mean(val["coverage", ]))
  cat("Trimming stress: trim =", trim, "complete\n")
}
write.csv(do.call(rbind, trimmed), file.path(outdir, "nu2-review-trimming.csv"), row.names = FALSE)
writeLines(c(paste("Reviewed revision:", revision), R.version.string,
             paste("RNG seed:", 20261023), paste("Regular repetitions:", B),
             "Regular intervals: two-sided nominal 95%, fixed cf.y=.04, cf.d=.03, rho2=1.",
             "Centered-null endpoint diagnostics: BF=1, conditional on a finite reported SE.",
             "These are Monte Carlo diagnostics under specified data-generating processes."),
           file.path(outdir, "nu2-review-simulation-session.txt"))
