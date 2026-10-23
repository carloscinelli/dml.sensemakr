# Run from repository root. Exact multinomial Monte Carlo for persistent
# clipping, validated against the actual old/new R implementations; plus a
# centered local-boundary example and its analytically derived Gaussian limit.
set.seed(20261024)
outdir <- "tools/experiments"
load_scores <- function(revision) {
  env <- new.env(parent = globalenv())
  for (file in c("R/short-parameters.R", "R/bias-bounds.R")) {
    txt <- if (revision == "worktree") readLines(file, warn = FALSE) else
      system2("git", c("show", paste0(revision, ":", file)), stdout = TRUE)
    txt <- txt[!grepl("^[A-Za-z_]+ <- sensemakr::", txt)]
    eval(parse(text = txt), envir = env)
  }
  env
}
old <- load_scores("27f58b4351eb9e13179de7b670517c2f4020d6a1")
new <- load_scores("worktree")

# D ~ Bernoulli(.01), Y independent Rademacher, g0=g1=0, oracle e=.01,
# but trim=.02. All statistics depend only on the four (D,Y) cell counts.
p <- .01
q <- .02
B <- 100000L
dcell <- c(0, 0, 1, 1)
ycell <- c(-1, 1, -1, 1)
rr <- dcell / q - (1 - dcell) / (1 - q)
clipping.results <- list()
validation.error <- 0
for (n in c(20000L, 200000L)) {
counts <- rmultinom(B, n, c((1 - p) / 2, (1 - p) / 2, p / 2, p / 2))
w <- counts / n
phat <- colSums(w * dcell)
theta <- colSums(w * (ycell * rr))
rr2 <- colSums(w * rr^2)
raw <- 2 / (q * (1 - q)) - rr2
floor <- 1 / (phat * (1 - phat))
nu <- pmax(raw, floor)
bind <- raw < floor
psi.theta <- sweep(matrix(ycell * rr, 4, B), 2, theta)
psi.old <- sweep(-matrix(rr^2, 4, B), 2, rr2, "+")
psi.floor <- sweep(sweep(matrix(dcell, 4, B), 2, phat), 2,
                   (2 * phat - 1) / (phat^2 * (1 - phat)^2), "*")
psi.new <- psi.old
psi.new[, bind] <- psi.floor[, bind]
theta.minus <- theta - sqrt(nu)
theta.plus <- theta + sqrt(nu)
truth.nu <- 1 / (p * (1 - p))
truth.S <- sqrt(truth.nu)

metrics <- function(psi) {
  psi.bias <- sweep(psi, 2, 2 * sqrt(nu), "/")
  list(se.nu = sqrt(colSums(w * psi^2) / n),
       se.minus = sqrt(colSums(w * (psi.theta - psi.bias)^2) / n),
       se.plus = sqrt(colSums(w * (psi.theta + psi.bias)^2) / n))
}
mo <- metrics(psi.old)
mn <- metrics(psi.new)

# Verify weighted count calculations equal expanded native R calls, including
# the downstream covariance term and all reported endpoint standard errors.
for (j in seq_len(25)) {
  d <- rep(dcell, counts[, j])
  y <- rep(ycell, counts[, j])
  for (version in c("old", "new")) {
    env <- if (version == "old") old else new
    m <- if (version == "old") mo else mn
    fit <- env$ate.npm(y, d, "all", rep(0, n), rep(0, n),
                        rep(p, n), p, trim = q)
    bounds <- env$bounds(fit, cf.y = .5, cf.d = 2 / 3)$estimates
    actual <- c(fit$estimates$nu2.s, fit$estimates$se.nu2.s,
                bounds$theta.m, bounds$theta.p, bounds$se.theta.m, bounds$se.theta.p)
    expected <- c(nu[j], m$se.nu[j], theta.minus[j], theta.plus[j],
                  m$se.minus[j], m$se.plus[j])
    validation.error <- max(validation.error, abs(actual - expected))
    stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-11)))
  }
}
result <- lapply(c("old", "new"), function(version) {
  m <- if (version == "old") mo else mn
  data.frame(version = version, n = n, repetitions = B, bind.rate = mean(bind),
    nu.true = truth.nu, nu.bias = mean(nu) - truth.nu,
    nu.sd = sd(nu), nu.mean.se = mean(m$se.nu),
    nu.coverage95 = mean(abs(nu - truth.nu) <= qnorm(.975) * m$se.nu),
    lower.sd = sd(theta.minus), lower.mean.se = mean(m$se.minus),
    lower.coverage95.twosided = mean(abs(theta.minus + truth.S) <= qnorm(.975) * m$se.minus),
    lower.coverage95.outward = mean(theta.minus - qnorm(.95) * m$se.minus <= -truth.S),
    upper.sd = sd(theta.plus), upper.mean.se = mean(m$se.plus),
    upper.coverage95.twosided = mean(abs(theta.plus - truth.S) <= qnorm(.975) * m$se.plus),
    upper.coverage95.outward = mean(theta.plus + qnorm(.95) * m$se.plus >= truth.S))
})
clipping.results[[length(clipping.results) + 1L]] <- do.call(rbind, result)
}
write.csv(do.call(rbind, clipping.results),
          file.path(outdir, "nu2-review-clipping-old-new.csv"), row.names = FALSE)
nu.ratio <- (1 / q^2 - 1 / (1 - q)^2) / ((1 - 2 * p) / (p^2 * (1 - p)^2))
var.theta <- p / q^2 + (1 - p) / (1 - q)^2
var.S <- (1 - 2 * p)^2 / (4 * p^2 * (1 - p)^2)
endpoint.ratio <- sqrt((var.theta + nu.ratio^2 * var.S) / (var.theta + var.S))
writeLines(c(paste("Expanded-source validation max absolute difference:", format(validation.error, digits = 16)),
             paste("Asymptotic original nu SE / true SD:", format(nu.ratio, digits = 16)),
             paste("Asymptotic original endpoint SE / true SD:", format(endpoint.ratio, digits = 16)),
             paste("Asymptotic original two-sided endpoint coverage:",
                   format(2 * pnorm(qnorm(.975) * endpoint.ratio) - 1, digits = 16)),
             paste("Asymptotic original outward endpoint coverage:",
                   format(pnorm(qnorm(.95) * endpoint.ratio), digits = 16))),
           file.path(outdir, "nu2-review-clipping-analysis.txt"))
cat("Exact clipping Monte Carlo complete\n")

# Centered local sequence: true beta_n=h/sqrt(n), e_n(X)=(1+beta_n*X)/2.
# ehat is estimated by beta_train=mean((2D-1)X), in two equal folds.
# For fixed h, n*nu_hat converges to 4*max((A+h)^2-3B^2,0).
# The ordinary square-root Wald method is not uniform in this neighborhood.
h <- 6
local.reps <- as.integer(Sys.getenv("NU2_LOCAL_MC_REPS", "4000"))
local.results <- list()
for (n in c(2000L, 10000L)) {
  m <- n / 2
  beta.true <- h / sqrt(n)
  true.nu <- 4 * beta.true^2 / (1 - beta.true^2)
  true.S <- sqrt(true.nu)
  val <- replicate(local.reps, {
    x <- sample(c(-1, 1), n, replace = TRUE)
    d <- rbinom(n, 1, .5 + .5 * beta.true * x)
    u <- (2 * d - 1) * x
    beta <- c(mean(u[seq_len(m)]), mean(u[m + seq_len(m)]))
    # Estimate the treatment share in each training fold, as the package does.
    shares <- c(mean(d[seq_len(m)]), mean(d[m + seq_len(m)]))
    phat <- rep(rev(shares), each = m)
    dhat <- phat + .5 * rep(rev(beta), each = m) * x
    fit <- new$att.npm.cond(rnorm(n), d, yhat0 = rep(0, n), dhat = dhat,
                            phat = phat, centered = TRUE)
    b <- new$bounds(fit, cf.y = .5, cf.d = 2 / 3)$estimates
    c(finite = as.numeric(is.finite(b$se.theta.m)),
      lower.coverage = as.numeric(b$theta.m - qnorm(.95) * b$se.theta.m <= -true.S),
      upper.coverage = as.numeric(b$theta.p + qnorm(.95) * b$se.theta.p >= true.S))
  })
  local.results[[length(local.results) + 1L]] <- data.frame(
    n = n, h = h, repetitions = local.reps, true.centered.nu = true.nu,
    finite.se.rate = mean(val["finite", ]),
    available.lower.coverage95.outward = mean(val["lower.coverage", ], na.rm = TRUE),
    available.upper.coverage95.outward = mean(val["upper.coverage", ], na.rm = TRUE),
    lower.coverage.if.NA.always.covered = mean(ifelse(is.na(val["lower.coverage", ]), 1, val["lower.coverage", ])),
    upper.coverage.if.NA.always.covered = mean(ifelse(is.na(val["upper.coverage", ]), 1, val["upper.coverage", ])))
  cat("Centered local sequence: n =", n, "complete\n")
}
write.csv(do.call(rbind, local.results), file.path(outdir, "nu2-review-centered-local.csv"), row.names = FALSE)

# Large, inexpensive Monte Carlo of the analytically derived limit.
M <- 1000000L
A <- rnorm(M)
Bb <- rnorm(M)
Ztheta <- rnorm(M, sd = 2)
W <- (A + h)^2 - 3 * Bb^2
R <- (A + h)^2 + Bb^2
positive <- W > 0
delta <- 2 * (sqrt(pmax(W, 0)) - h)
se <- rep(NA_real_, M)
se[positive] <- 2 * sqrt(1 + R[positive] / W[positive])
upper <- Ztheta + delta + qnorm(.95) * se >= 0
lower <- Ztheta - delta - qnorm(.95) * se <= 0
limit <- data.frame(h = h, repetitions = M, finite.se.rate = mean(positive),
  available.lower.coverage95.outward = mean(lower[positive]),
  available.upper.coverage95.outward = mean(upper[positive]),
  lower.coverage.if.NA.always.covered = mean(ifelse(positive, lower, TRUE)),
  upper.coverage.if.NA.always.covered = mean(ifelse(positive, upper, TRUE)))
write.csv(limit, file.path(outdir, "nu2-review-centered-local-limit.csv"), row.names = FALSE)
