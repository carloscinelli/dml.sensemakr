# Review of the `nu2.s` constraints and boundary inference

## Findings and scope

**All the population lower bounds introduced in commit `27f58b4351` are
correct. Retaining the original influence function is first-order valid at
their regular population boundaries, because the original and lower-bound
influence functions coincide there. It is not universally valid for the
estimator returned by the implementation.** A supported, persistently
clipped propensity supplies a concrete first-order variance counterexample.
A separate centered local-boundary example establishes failure of uniform
normal endpoint coverage even with correctly specified, root-n-consistent
nuisance learners.

The review establishes the following decisions:

| Question | Finding | Action |
| --- | --- | --- |
| Population floors, including groups and continuous-treatment PLM | Correct under the stated identification and moment conditions | Preserve them |
| Original influence function at a regular boundary | Agrees with the floor influence function; projection is negligible at the root-n scale | Preserve the mathematical justification |
| Raw score below an empirical floor under persistent clipping | Retained raw score can severely understate the returned estimator's variance | Use the selected floor's influence function |
| Centered conditional polynomial near zero | Floating-point cancellation can reverse the raw score's sign | Use the equivalent normalized-odds difference formula |
| Explicitly zero sensitivity with zero scales or undefined gains | Numeric and benchmark paths can return unavailable results for quantities that reduce to the ordinary effect | Bypass the irrelevant scale or gain derivative |
| Zero first-order norm variance and centered scales near zero | Ordinary studentized normal inference is not generally justified | Document the limitation; do not introduce an unsupported boundary procedure |

Notation: `v` denotes `nu2.s` (the squared norm), `s` denotes `sigma2.s`,
`S = sqrt(s v)`, and the fixed finite sensitivity multiplier is

\[
b=\sqrt{\rho^2\,\mathrm{cf.y}\,
         \frac{\mathrm{cf.d}}{1-\mathrm{cf.d}}}.
\]

Sensitivity endpoints are `theta_minus = theta_s-b S` and
`theta_plus = theta_s+b S`. Statements about confidence coverage below
refer to these endpoints or to the norm, as explicitly labeled. They are
not claims of simultaneous coverage over sensitivity parameters or of
coverage for an arbitrary causal effect strictly inside the identified
interval. Except where a benchmark is explicitly discussed, `b` is fixed,
not estimated. Benchmark propagation additionally requires regular gain
and alignment estimators.

### Reviewed sources and implementation base

- [Requested commit `27f58b4351eb9e13179de7b670517c2f4020d6a1`](https://github.com/carloscinelli/dml.sensemakr/commit/27f58b4351eb9e13179de7b670517c2f4020d6a1), which introduced the floors and version 0.2.3.
- [Implementation base `3738a3a1fe4d75fff00f1ed57e4a8e8b104b621b`](https://github.com/carloscinelli/dml.sensemakr/commit/3738a3a1fe4d75fff00f1ed57e4a8e8b104b621b), the development branch at review time, version 0.2.3.9000. Subsequent test, documentation, and group-trimming changes are preserved.
- Primary score and inference sources: [`R/short-parameters.R`](../../R/short-parameters.R), [`R/bias-bounds.R`](../../R/bias-bounds.R), and [`R/benchmarks.R`](../../R/benchmarks.R).
- Background for the squared-norm sensitivity scale and regular DML endpoint inference: Chernozhukov, Cinelli, Newey, Sharma, and Syrgkanis, [*Long Story Short: Omitted Variable Bias in Causal Machine Learning*](https://arxiv.org/html/2112.13398v5), especially Section 4.
- Background for normalized density ratios and centered conditional imbalance: Wang, Sant'Anna, Chernozhukov, and Cinelli, [*Omitted Variable Bias in Difference-in-Differences Designs*](https://arxiv.org/pdf/2609.19386), Appendix A.3.

The proofs, counterexamples, and experiments below are derived for the
actual package scores. The paper references provide context; they are not
substitutes for verifying this implementation.

## 1. Population lower bounds

Write `p = P(D = 1)`, `e(X) = P(D = 1 | X)`, and suppose treatment overlap
and the relevant second moments hold. For unconditional targets the squared
Riesz norms are

\[
\begin{aligned}
v_{\mathrm{ATE}}&=E\!\left[\frac1{e(X)(1-e(X))}\right],\\
v_{\mathrm{ATT}}&=\frac1{p^2}E\!\left[\frac{e(X)}{1-e(X)}\right],\\
v_{\mathrm{ATU}}&=\frac1{(1-p)^2}E\!\left[\frac{1-e(X)}{e(X)}\right].
\end{aligned}
\]

Jensen's inequality gives `v >= 1/[p(1-p)]` in every case. Equivalently,
each of the three representers has `E[alpha] = 0` and `E[D alpha] = 1`;
Cauchy--Schwarz gives

\[
1=E[(D-p)\alpha]^2\le p(1-p)E[\alpha^2].
\]

Equality holds precisely when `e(X) = p` almost surely, apart from
irrelevant null sets. At equality all three representers are
`(D-p)/[p(1-p)]`.

For conditional ATT, let `P_1` and `P_0` be the conditional distributions of
`X` given treatment and control, and let

\[
r(X)=\frac{dP_1}{dP_0}(X)
    =\frac{e(X)/(1-e(X))}{p/(1-p)}.
\]

The uncentered norm is `v = E_0[r^2] = 1 + chi^2(P_1 || P_0) >= 1`;
the centered norm is `v_c = E_0[(r-1)^2] = v-1 >= 0`.
Conditional ATU reverses the two distributions. Equality means their
conditional covariate distributions coincide, equivalently `e(X) = p`.

For the partially linear model, write `m(X) = E[D | X]` and
`Q = E[(D-m(X))^2]`. Then

\[
v=\frac1Q\ge\frac1{\operatorname{Var}(D)},\qquad
\operatorname{Var}(D)=Q+\operatorname{Var}(m(X)).
\]

This applies to binary and continuous treatment. Equality requires
`m(X) = E[D]`, which is mean independence and does not require full
independence for a continuous treatment.

For a fixed covariate-defined subgroup, use all the same formulas under the
conditional law given that subgroup. Its own treatment share or variance
is required. The group must have positive probability, the treatment must
vary within it, and the nuisance regressions must be the appropriate
conditional regressions. Reusing global regressions is justified for a
group measurable with respect to the included covariates. Arbitrary groups
defined by outcomes or treatment require a separate identification argument.

### 1.1 Exact gaps and the meaning of centering

These identities make the order of approach to the boundary explicit. Put
`h(X) = e(X)-p` and `L = 1/[p(1-p)]`. Then

\[
\begin{aligned}
v_{\rm ATE}-L
 &=\frac{E[h^2/e]}{p^2}+\frac{E[h^2/(1-e)]}{(1-p)^2},\\
v_{\rm ATT}-L
 &=\frac{E[h^2/(1-e)]}{p^2(1-p)^2},\\
v_{\rm ATU}-L
 &=\frac{E[h^2/e]}{p^2(1-p)^2},\\
v_{c,\rm ATT}
 &=\frac{E[h^2/(1-e)]}{p^2(1-p)},\\
v_{c,\rm ATU}
 &=\frac{E[h^2/e]}{p(1-p)^2}.
\end{aligned}
\]

Thus the gaps are quadratic in the departure from constant propensity;
`v_ATT = L + v_c,ATT/(1-p)` and `v_ATU = L + v_c,ATU/p`.
For PLM, with `V = Var(D)` and `Q = E[(D-m(X))^2]`,

\[
v-1/V=\frac{\operatorname{Var}(m(X))}{VQ}.
\]

Centering removes the constant component of a conditional representer.
The uncentered and centered norms differ by exactly one, so their
population influence functions agree. Nevertheless, the sensitivity scale
and the interpretation of a *fixed numerical* treatment confounding
fraction change. Using the same `cf.d` in both parameterizations does not
necessarily express the same restriction on omitted variables.
In particular, if the short centered norm is zero and a long centered norm
is positive, the fraction `(v_long-v_short)/v_long` is one. This is the
singular endpoint of the factor `cf.d/(1-cf.d)`. A zero computed bias at
fixed `cf.d < 1` does not establish that hidden confounding is absent.
The uncentered version avoids a zero imbalance norm, but a zero outcome
residual variance can still make its overall scale zero.

## 2. Projection and the generic nonregular limit

Let `L(P)` be a population lower bound and suppose

\[
\widehat v-v=\mathbb P_n\phi_v+o_p(n^{-1/2}),\qquad
\widehat L-L=\mathbb P_n\phi_L+o_p(n^{-1/2}).
\]

The constrained estimator is `v_tilde = max(v_hat, L_hat)`. With
`Delta = v-L >= 0`, its exact decomposition is

\[
\widetilde v-v=(\widehat v-v)
 +\bigl\{-\Delta+(\widehat L-L)-(\widehat v-v)\bigr\}_+.
\]

At a fixed interior point `Delta > 0`, consistency makes the second term
zero with probability tending to one. At an abstract boundary `Delta = 0`,
joint asymptotic normality would give

\[
\sqrt n(\widetilde v-v)\rightsquigarrow\max(Z_v,Z_L).
\]

For a triangular sequence with `sqrt(n) Delta_n -> c`, the generic limit
is `max(Z_v, Z_L-c)`. These expressions alone do not establish that the
package's constraints produce a nonnormal first-order distribution:
the two normal variables are identical at the regular equality cases here.

## 3. Why the original influence functions agree with the floor

There is a general reason. Suppose the two functionals are differentiable
in a model allowing two-sided regular parametric submodels and
`g(P) = v(P)-L(P) >= 0` throughout that model. If `g(P_0) = 0`, then every
two-sided differentiable path through `P_0` has derivative zero. In the
nonparametric model this implies equality of their canonical influence
functions in `L2(P_0)`:

\[
\phi_v=\phi_L\quad P_0\text{-almost surely}.
\]

Thus `v_hat-L_hat = o_p(n^-1/2)` at equality, and
`v_tilde-v_hat = o_p(n^-1/2)`. Retaining the original influence function is
correct to first order under these assumptions. Using the floor's influence
function when that branch is selected is also first-order valid under the
same assumptions: it agrees with the original influence function at the
population boundary, and branch selection vanishes at an interior point.
For a data-dependent floor it is essential to keep the floor's own
influence function, rather than setting it to zero.

The equality can be checked directly against the package's scores:

* For all three unconditional binary targets, at `e(X) = p`,
  \[
  \phi_v=\phi_L=
  \frac{(2p-1)(D-p)}{p^2(1-p)^2}.
  \]
  This retains the uncertainty in the treatment share. It is zero only
  at the balanced value `p = 1/2`.
* For both parameterizations of conditional ATT and ATU, the population
  influence function of the norm is identically zero at equality. The
  floors, respectively one and zero, are constants with the same zero
  influence function.
* For PLM at mean independence, writing `mu = E[D]` and
  `V = Var(D)`,
  \[
  \phi_v=\phi_L=
  -\frac{(D-\mu)^2-V}{V^2}.
  \]
  The influence function is usually nonzero for continuous treatment;
  constant squared deviations are a special degenerate case.

Within-group influence functions use the same formulas under the group
law. If expressed on the full sample, multiply the conditional influence
function by the group indicator divided by group probability. Calculating
its variance on `n_g` observations is asymptotically equivalent when the
group is fixed and its probability is bounded away from zero.

## 4. Local sequences

Suppose a sequence of distributions approaches an equality distribution,
the asymptotic expansions hold uniformly along the sequence, and

\[
\|\phi_{v,n}-\phi_{L,n}\|_{L^2(P_n)}\longrightarrow0.
\]

Because `Delta_n >= 0`,

\[
0\le\sqrt n(\widetilde v-\widehat v)
\le\left|\frac1{\sqrt n}\sum_{i=1}^n
       (\phi_{L,n}(W_i)-\phi_{v,n}(W_i))\right|+o_p(1)
=o_p(1).
\]

Consequently the projection remains negligible on the root-n scale along
these sequences, including sequences whose population slack is of order
`n^-1/2` or smaller. A nontrivial generic max-normal limit requires
first-order differences between the two estimators, which these regular
equality sequences do not have. This conclusion is an absolute root-n
statement; it does not imply a studentized normal approximation when the
variance itself tends to zero.

The assumptions matter. They require the usual nuisance consistency and
remainder rates, uniform moment control and overlap, and well-behaved
group sizes. Persistently misspecified learners, propensity clipping that
remains active at the population truth, shrinking groups, and nearly
unidentified treatment distributions are not generally repaired by applying a floor.

## 5. Positive scales and effect-bound inference

Write `s = sigma2.s`, `S = sqrt(s v)`, and fix a finite nonnegative bias
factor `b`. When `s v > 0`,

\[
\phi_{S^2}=s\phi_v+v\phi_s,\qquad
\phi_S=\frac{s\phi_v+v\phi_s}{2\sqrt{sv}},\qquad
\phi_{\theta_\pm}=\phi_\theta\pm b\phi_S.
\]

Under the preceding assumptions, replacing `v_hat` by `v_tilde` in these
smooth coefficients preserves their limits. If the endpoint influence
function has a positive finite variance, the usual endpoint Wald standard
errors remain first-order valid, including equality of an unconditional
floor or an uncentered conditional floor. At the latter floor `v = 1`, the
norm contributes zero first-order variation, while `s` and `theta` can
still contribute ordinary first-order variation.

These claims concern the endpoint distribution. Coverage of an entire
identified interval, coverage of a particular admissible causal effect,
and simultaneous coverage over many sensitivity values are distinct
questions and require the corresponding critical-value construction.

## 6. Degenerate norms do not have ordinary first-order Wald inference

Equality of influence functions does not establish nominal coverage for
the norm itself if both influence functions are zero. A simple exact
example is a PLM with independent Rademacher treatment `D` taking values
`-1,1` and the correct predictor `m_hat = 0`. For every sample,

\[
\widehat v=1,\quad\widehat\phi_v\equiv0,\quad
\widetilde v=\frac1{1-\overline D^2}.
\]

The population norm is one. The reviewed commit's retained first-order standard
error is zero, although the constrained estimator is random at order
`n^-1`. A point Wald interval covers only if the sample is exactly
balanced, an event whose probability tends to zero. Here

\[
n(\widetilde v-1)\rightsquigarrow\chi^2_1.
\]

The first-order statement `sqrt(n)(v_tilde-1) -> 0` remains correct.
Likewise, a regular effect endpoint may still have valid root-n inference,
because this norm's second-order fluctuation is negligible relative to a
nondegenerate endpoint influence function. Replacing the retained score
with the empirical floor's score would not turn the chi-square limit into
a normal one.

## 7. Centered zero scales: a separate nonregular square root

For a centered conditional norm `v_c = 0`, `S = sqrt(s v_c) = 0`. Even
though the influence function of `v_c` is zero, the square-root functional
is not differentiable there. The absence of first-order variation in the
squared norm does not imply absence of first-order variation in its square
root. Returning unavailable bound standard errors at an exactly zero
estimated scale avoids division by zero; it cannot by itself establish
valid inference for positive estimates arising when the true scale is zero.

Here is a concrete cross-fitting counterexample with correctly specified,
root-n-consistent nuisance estimates. Let `n = 2m`, let `X` and
`T = 2D-1` be independent Rademacher variables, and let `Y` be an independent
standard normal variable. The true propensity and treatment share are
one half, the outcome regression is zero, and the true centered norm is
zero. Split observations into two equal folds and define

\[
\widehat\beta_j=\frac1m\sum_{i\in I_j}T_iX_i.
\]

Use `e_hat(X) = (1 + beta_hat_2 X)/2` on fold one and the analogous
prediction from fold one on fold two. Supply `p_hat = 1/2` and `g0_hat = 0`.
Trimming is asymptotically inactive. Put

\[
Z_j=\sqrt m\,\widehat\beta_j,\qquad
A=(Z_1+Z_2)/\sqrt2,\qquad B=(Z_1-Z_2)/\sqrt2.
\]

The limiting `A,B` are independent standard normal variables. The centered
score can be written, with `l = D/p`, `lc = (1-D)/(1-p)` and `h = r-1`, as

\[
a_c=2(l-l_c)h-l_ch^2,\qquad
\widehat v_{c,\mathrm{raw}}=
\frac{\mathbb P_n a_c}{\mathbb P_n(2l-l_c)}.
\]

For a held-out fold, `h = 2 beta_hat X + O_p(beta_hat^2)`, and averaging
its score gives, to order `n^-1`,
`8 beta_train beta_test - 4 beta_train^2`. Therefore

\[
\begin{aligned}
\widehat v_{c,\mathrm{raw}}
 &=8\widehat\beta_1\widehat\beta_2
       -2(\widehat\beta_1^2+\widehat\beta_2^2)+o_p(n^{-1}),\\
n\widehat v_{c,\mathrm{raw}}
 &\rightsquigarrow4(A^2-3B^2),\\
n\widetilde v_c
 &\rightsquigarrow4\max(A^2-3B^2,0),\\
\sqrt n\,\widehat S
 &\rightsquigarrow2\sqrt{\max(A^2-3B^2,0)}.
\end{aligned}
\]

The same limit holds with cross-fitted empirical treatment shares,
matching the package's usual construction. On training fold `j`, write
`eta_j = mean(T)` and use
`p_hat_j = (1+eta_j)/2`,
`e_hat_j(X) = p_hat_j + beta_hat_j X/2`. Then

\[
\begin{aligned}
h_j(X)
 &=\frac{2\widehat\beta_jX}
 {(1+\eta_j)(1-\eta_j-\widehat\beta_jX)}
 =2\widehat\beta_jX+2\widehat\beta_j^2+O_p(n^{-3/2}),\\
l-l_c&=2T-2\eta_j+O_p(n^{-1}).
\end{aligned}
\]

The additional held-out averages involve
`beta_hat_j^2 mean(T_test)` and
`eta_j beta_hat_j mean(X_test)`, both `O_p(n^-3/2)`. Thus the empirical
shares change neither the leading quadratic expression for the norm nor
the displayed scale and endpoint limits. The argument also applies to the
local alternatives below. Fitting an intercept and slope by ordinary
linear regression instead of using the stated fold moments only changes
the slope by `O_p(n^-1)` and has the same limit.

The constrained scale is positive with limiting probability one third;
its distribution has a point mass of two thirds at zero. Let
`W = A^2-3B^2`. On the positive branch, the package's delta standard errors
have the limits

\[
n\widehat{\mathrm{se}}(v_c)\rightsquigarrow8\sqrt{A^2+B^2},\qquad
\sqrt n\,\widehat{\mathrm{se}}(S)
\rightsquigarrow2\sqrt{\frac{A^2+B^2}{W}}.
\]

The latter is random and diverges as the positive branch approaches
`W = 0`; it is not a consistent estimate of a fixed normal-limit standard
deviation. Meanwhile `sqrt(n) theta_hat` tends to a normal variable with
variance four, independent of `A,B`. Hence the endpoint limit is

\[
Z_\theta\pm2b\sqrt{\max(W,0)},\qquad Z_\theta\sim N(0,4).
\]

This is not the regular endpoint normal limit. The example establishes a
limitation of centered boundary inference even when the program returns
a positive scale and a finite standard error. The floor does not cause the
underlying nondifferentiability of the square root, and choosing a
different first-order norm influence function does not resolve it.

Nonnormality alone does not imply undercoverage of an outward sensitivity
confidence bound. For example, in the exact-null construction just given,
the outward intervals can be conservative. A nearby sequence does provide
a direct undercoverage example. Set the true propensity to

\[
e_n(X)=\frac12+\frac{hX}{2\sqrt n},\qquad h>0,
\]

and use the same two-fold learner. The true centered norm is
`4 h^2/(n-h^2)`. Now put

\[
W_h=(A+h)^2-3B^2,\qquad R_h=(A+h)^2+B^2.
\]

On the positive branch the upper endpoint error and its reported standard
error obey

\[
\begin{aligned}
\sqrt n(\widehat\theta_+-\theta_+)
 &\rightsquigarrow Z_\theta+2b(\sqrt{W_h}-h),\\
\sqrt n\,\widehat{\mathrm{se}}(\theta_+)
 &\rightsquigarrow2\sqrt{1+b^2R_h/W_h}.
\end{aligned}
\]

For `h = 6` and `b = 1`, even granting coverage on every zero-scale branch,
the limiting coverage of the outward one-sided 95% upper confidence bound
is

\[
E\!\left[\mathbf1\{W_6\le0\}
 +\mathbf1\{W_6>0\}
 \Phi\!\left(z_{.95}\sqrt{1+R_6/W_6}+\sqrt{W_6}-6\right)\right]
\simeq0.934594607.
\]

This calculation uses a correctly specified, root-n-consistent learner.
The zero-scale event has probability about 0.0027. Thus missing output at
exactly zero does not account for the failure: positive estimated scales
near the boundary can produce undercoverage. This is a failure of uniform
nominal coverage over these local alternatives, not a failure of ordinary
fixed-interior asymptotics.

The following deterministic quadrature reproduces the coverage calculation
(requires Python, NumPy, and SciPy). Increasing the quadrature order from
160 to 320 to 640 gives respectively `0.9345948138`, `0.9345946098`, and
`0.9345946069`.

```python
import numpy as np
from scipy.special import roots_hermitenorm, ndtr
from scipy.stats import norm

x, weights = roots_hermitenorm(640)
weights = weights / np.sqrt(2 * np.pi)
a, b = x[:, None] + 6, x[None, :]
w, r = a*a - 3*b*b, a*a + b*b
positive = w > 0
coverage = np.ones_like(w)  # Grant coverage on every unavailable branch.
coverage[positive] = ndtr(
    norm.ppf(.95) * np.sqrt(1 + r[positive] / w[positive])
    + np.sqrt(w[positive]) - 6
)
print(weights @ coverage @ weights)
```

For centered sequences approaching zero, a normal delta approximation
requires stronger conditions than fixed-interior DML theory. In simple
finite-dimensional models `Var(phi_v)` is of order `v`; an oracle
calculation then requires `n v -> infinity` and a remainder
`o_p(sqrt(v/n))`. The usual absolute remainder `o_p(n^-1/2)` alone is
insufficient as `v -> 0`. In the local region `v = O(1/n)`, quadratic
nuisance-estimation terms and the square-root kink have the same order as
the target uncertainty.

## 8. A genuine failure with active propensity clipping

The equality-of-influence-functions argument requires that the original
score consistently estimate the population norm. Fixed propensity
clipping can violate that condition even if the supplied propensity is
exact. A concrete example makes the resulting standard-error defect
unambiguous.

Take an unconditional ATE, `D ~ Bernoulli(p)` with `p = .01`, and a constant
correct input propensity `.01`. Explicitly set `ps.trim = .02` in the
public API, or `trim = .02` in the score function, so clipping replaces it
with `a = .02`. The public `dml()` default is `.01`; `.02` is the internal
score function's default, not the public default.
The raw norm score is

\[
A(D)=\frac{2}{a(1-a)}-\frac{D}{a^2}
                  -\frac{1-D}{(1-a)^2}.
\]

Its probability limit is `76.0099958351`, strictly below the population
floor `1/[p(1-p)] = 101.0101010101`. Consequently the constrained estimator
equals the empirical floor with probability tending to one, and is
consistent for the true norm in this particular example. Its actual
influence function is

\[
\phi_L=-9998.9796959494(D-p).
\]

The retained raw-score influence function is instead

\[
\phi_{\rm raw}=-2498.9587671803(D-p),
\]

so its standard error is only `0.2499213763` times the correct one.
This is a regular first-order error for the estimator actually returned,
not a second-order or finite-sample concern.

For a direct endpoint example, take `Y` to be an independent Rademacher
variable and supply its exact zero outcome regressions. The estimated
outcome residual variance is exactly one. The ATE estimator has asymptotic
variance `26.0308204915`, while the true asymptotic variance of the scale
is `2449.7500255076`; the retained raw-score formula gives
`153.0130875290`. The effect and scale influence functions are uncorrelated.
At bias factor `b = 1`, the reported endpoint standard error is therefore
`0.2689203545` times its actual standard error. Its asymptotic two-sided
95% endpoint Wald coverage is about `40.19%`, and the outward one-sided
95% endpoint coverage is about `67.09%`. These figures concern a confidence
bound for the sensitivity endpoint itself; coverage of a causal effect
strictly inside the identified interval is a separate question.

When the raw functional is strictly below the floor, the projection has
selected a smooth functional whose influence function is known. Using that
floor's influence function fixes this error. The change is also valid
under the regular assumptions of Sections 3--5. In this sense, branch
selection supplies a justified improvement even though retaining the raw
influence function was not intrinsically wrong in the regular model.
It does not repair general misspecification: if the true norm is strictly
above the floor and a bad nuisance estimate pushes the raw functional
below it, the projected point estimate can remain inconsistent.

## 9. Consequences for implementation

The lower bounds and retention of the original norm influence functions
are justified for regular first-order inference. However, the active
clipping counterexample establishes that using the selected floor's
influence function is more broadly correct for the returned estimator.
For unconditional binary targets use
`(2 p_hat-1)(D-p_hat)/[p_hat^2(1-p_hat)^2]`; for PLM use
`L_hat-L_hat^2(D-Dbar)^2`; for either conditional constant floor use zero.
Keep the original influence function when the original estimate is above
the floor. Preserve unavailable estimates for undefined inputs.

The existing zero-scale handling correctly declines to apply the
square-root delta method at an exactly zero estimate. Branch-specific
norm influence functions do not resolve the centered square root's
nonregularity at a true zero norm.

Numerically stable evaluation of the centered score is worthwhile. In
particular, compute

\[
h=\frac{\widehat e-\widehat p}
       {\widehat p(1-\widehat e)},\qquad
a_c=2(l-l_c)h-l_ch^2
\]

directly instead of subtracting expanded polynomials with nearly equal
terms. The uncentered raw estimate is exactly the centered raw estimate plus
one, and their influence functions are identical. This algebra preserves
the parameterization while reducing cancellation near the boundary.

A universal finite-sample diagnosis of a true zero centered norm is not
available merely from whether its estimated floor binds. Uniformly valid
inference near that boundary would require a dedicated procedure, such as
inverting an appropriate confidence set for the underlying imbalance with
its nuisance estimation accounted for. It cannot be obtained by declaring
all positive estimated scales regular, by switching influence functions,
or by selecting an arbitrary numerical tolerance. Documentation must state
the positive-population-scale and nondegenerate-variance requirements.

## 10. Numerical cancellation and exact-zero handling

### 10.1 A sign error in the expanded centered score

For conditional ATT, set `n = 100`, take 97 treated and three control
observations, use `p_hat = .97` and constant `e_hat = .97000001`, and leave
clipping inactive. Because the fitted normalized odds `r` are constant and
`p_hat` is the sample share, the exact raw centered score is

\[
2r-r^2-1=-(r-1)^2<0.
\]

The reviewed implementation's expanded polynomial evaluates this raw score
as approximately `+1.879194541265e-12`. Direct evaluation of the
normalized-odds difference gives approximately `-1.18090323849e-13`, in
agreement with the negative-square identity. The erroneous positive sign
bypasses the floor, allowing a spurious positive sensitivity scale and a
finite square-root standard error. With the outcome fixture in the new
regression test, the old squared scale is approximately
`1.84192384953e-12` and the old bias-bound standard error is approximately
`.005117152829`; the exact constrained scale is zero and the positive-bias
square-root standard error is unavailable.

The corrected ATT calculation uses

\[
\delta_i=\frac{\widehat e_i-\widehat p_i}
                   {\widehat p_i(1-\widehat e_i)},\quad
q_i=2(l_i-l_{c,i})\delta_i-l_{c,i}\delta_i^2,\quad
A_i=2l_i-l_{c,i},
\]

\[
\widehat v_{c,\mathrm{raw}}=\frac{\overline q}{\overline A},\qquad
\widehat\phi_v=\frac{q_i-A_i\widehat v_{c,\mathrm{raw}}}{\overline A}.
\]

ATU uses `delta = (p_hat-e_hat)/[(1-p_hat)e_hat]` and the corresponding
swapped weights. Both parameterizations use this same raw centered score:
the returned norm is `max(v_c,raw, 0)` or `1+max(v_c,raw, 0)`. Testing the
floor before adding one also prevents a tiny negative centered value from
rounding to an apparent equality in the uncentered version. This is an
algebraic stability correction, not a tolerance or a small-value truncation.
The tests explicitly preserve tiny genuinely positive values and the
centered/uncentered score identity with fold-specific treatment shares.

### 10.2 Explicitly zero requested confounding

At `b = 0`, an endpoint is exactly the short effect. Its standard error is
exactly the effect standard error, irrespective of irrelevant scale
uncertainty. The score-based path already had a zero-bias influence-function
branch, but `confidence_bounds.numeric()` still evaluated terms such as
`b^2 se(S2)^2/(4 S2)` and `b cov(theta,S2)/sqrt(S2)`. At `b = S2 = 0`, these
produced `0/0`.

The numeric path now returns the effect estimates and standard errors
before evaluating those terms, and then uses the existing repetition
combination rule. For positive `b`, a zero squared scale continues to yield
an unavailable square-root standard error. This behavior is tested against
the score-based path.

For `benchmark_bounds()`, an explicit `kY = 0`, `kD = 0`, or fixed `rho2 = 0`
also fixes the bias at zero. An undefined relative gain, such as `0/0` when
both centered norms vanish, is irrelevant to that request. The correction
returns the ordinary effect confidence bounds before inspecting gain
values or multiplying their unavailable derivatives. For a positive
requested strength, an undefined gain remains unavailable and retains the
existing warning; it is not reinterpreted as a measured zero gain.

## 11. Computational evidence

All Monte Carlo results below are diagnostics for specified designs.
Neither high coverage in one design nor a passing test suite proves
uniform validity. The mathematical counterexamples identify the failure
mechanisms; the computations check their consequences in the actual code.

### 11.1 Regular boundaries, interiors, and local approaches

[`nu2-review-simulation.R`](nu2-review-simulation.R) reads the actual source
at the requested commit using `git show`. The main grid uses 2,000
replications per scenario and two-fold cross-fitting. It includes six
models: unconditional ATE, ATT, and ATU; uncentered conditional ATT and ATU;
and continuous-treatment PLM. Sample sizes are 500, 2,000, and 8,000.
There are four regimes: exact equality, a fixed interior point, a nuisance
slope proportional to `n^-1/4`, and a slope proportional to `n^-1/2`.
The last two produce different orders of slack because the norm gap is
quadratic. Binary models use `p = .3`, `X` equal to `-1` or `1`, and
`e(X) = .3+a X`. PLM uses `D = a X+U`, with independent standard normal
`U`. Correct outcome regressions are supplied, with independent normal
outcome residuals. The script specifies the complete designs.

These diagnostics use two-sided nominal 95% intervals for each endpoint,
with fixed `cf.y = .04`, `cf.d = .03`, and `rho2 = 1`. At `n = 8,000` and
the exact population boundary:

| Model | Floor binding | Lower endpoint coverage | Upper endpoint coverage |
| --- | ---: | ---: | ---: |
| ATE | 86.80% | 95.25% | 95.05% |
| ATT | 64.65% | 95.25% | 95.10% |
| ATU | 85.50% | 95.30% | 95.05% |
| Conditional ATT, uncentered | 65.95% | 95.30% | 95.05% |
| Conditional ATU, uncentered | 66.30% | 95.30% | 95.00% |
| PLM, continuous treatment | 86.15% | 94.90% | 95.15% |

Mean reported endpoint standard errors divided by empirical standard
deviations range from `0.9874` to `1.0085` in this boundary table. Across
all 72 regular scenarios, endpoint coverage ranges from 93.4% to 95.9%.
The Monte Carlo standard error of a coverage estimate near 95% is about
0.49 percentage points with 2,000 replications. Frequent selection of a
floor is compatible with regular endpoint inference, as the proof predicts.
These results use the **original** influence-function policy, deliberately
testing the disputed mathematical claim rather than merely the correction.
Full results: [`nu2-review-regular.csv`](nu2-review-regular.csv).

Covariate-defined group ATE experiments use total sample size 4,000,
approximately 2,000 observations per group, group treatment shares .25 and
.65, and 2,000 replications. Group endpoint coverage at equality ranges
from 94.95% to 95.30%; fixed-interior coverage ranges from 93.9% to 95.65%.
These are per-group endpoint diagnostics, not simultaneous group coverage.
Full results: [`nu2-review-groups.csv`](nu2-review-groups.csv).

### 11.2 Exact old/new comparison for persistent clipping

[`nu2-review-counterexamples.R`](nu2-review-counterexamples.R) implements
the clipping construction in Section 8. It uses 100,000 exact multinomial
replications at each of `n = 20,000` and `n = 200,000`. Because `(D,Y)` has
four possible values, all sample estimates and influence-function moments
can be calculated from the four cell counts. This avoids constructing
billions of individual observations. Twenty-five samples at each size
were expanded and compared against **both actual old and corrected R
functions**; the maximum absolute discrepancy was `2.70e-13`.

At `n = 200,000`, with `b = 1`:

| Quantity | Original | Corrected |
| --- | ---: | ---: |
| Empirical SD of `nu2.s` | 2.22108 | 2.22108 |
| Mean reported SE of `nu2.s` | 0.55594 | 2.22681 |
| Two-sided 95% coverage for `nu2.s` | 38.161% | 95.113% |
| Empirical SD of lower endpoint | 0.111052 | 0.111052 |
| Mean reported SE of lower endpoint | 0.029918 | 0.111320 |
| Two-sided 95% lower endpoint coverage | 40.513% | 95.053% |
| Outward one-sided 95% lower endpoint coverage | 67.553% | 94.940% |
| Outward one-sided 95% upper endpoint coverage | 67.633% | 94.975% |

The analytic asymptotic coverages for the original endpoint procedure are
40.186% (two-sided) and 67.088% (outward one-sided). At `n = 20,000`,
corrected outward coverage is about 94.4%, converging toward 95% at the
larger size. This finite-sample skew is distinct from the original
nonvanishing variance error. With 100,000 replications, Monte Carlo
standard errors are approximately 0.069 percentage points near 95%
coverage and 0.155 percentage points near 40% coverage.

The input propensity `.01` is correct in this example. The public API must
explicitly use `ps.trim = .02`; the public default is `.01`. Thus the
example establishes a problem under a supported clipping setting, not an
assertion about the public default at `p = .01`. The correction fixes the
variance of the returned empirical floor in this example; it does not
make general clipped or misspecified nuisance estimates consistent.

A separate direct call to the original native score functions with normal
outcomes, `n = 10,000`, and 4,000 replications gives mean norm SE 9.948
against empirical SD 10.283 for inactive clipping `.001`, and mean SE
2.486 against empirical SD 10.350 for clipping `.02`. Corresponding norm
coverage is 93.525% and 38.025%.

Sources: [`nu2-review-clipping-old-new.csv`](nu2-review-clipping-old-new.csv),
[`nu2-review-clipping-analysis.txt`](nu2-review-clipping-analysis.txt), and
[`nu2-review-trimming.csv`](nu2-review-trimming.csv).

### 11.3 Degenerate norms and centered local alternatives

For balanced randomized ATE with an intercept-only cross-fitted propensity,
the original-score experiment verifies `n*(nu_hat-4) -> 4 chi-square_1`.
At `n = 8,000`, over 4,000 replications, the scaled error has mean 4.0207
and SD 5.6628, compared with theoretical values 4 and 5.6569. Ordinary
norm Wald coverage is 100%, not 95%. At the uncentered conditional boundary,
the norm itself likewise has 100% coverage in the regular grid despite
well-calibrated smooth endpoints. These observations illustrate why a
correct zero first-order influence function does not imply a valid
studentized approximation at the smaller, second-order scale.
Source: [`nu2-review-degenerate.csv`](nu2-review-degenerate.csv).

For the centered-null construction in Section 7, 6,000 replications per
sample size give:

| Sample size | Positive estimated centered norm | Mean absolute difference between `n*nu_hat` and the coupled quadratic limiting expression |
| --- | ---: | ---: |
| 500 | 33.783% | 0.17877 |
| 2,000 | 33.800% | 0.09054 |
| 8,000 | 33.900% | 0.04582 |

The observed positive frequencies are consistent with one third, and the
approximation error halves when the sample size quadruples, agreeing with
the derived limit.
An unavailable standard error on the zero branch is not counted as
ordinary nominal coverage. The CSV labels any conditional-on-availability
coverage explicitly. Source: [`nu2-review-centered.csv`](nu2-review-centered.csv).

For the local sequence with `h = 6`, the corrected code, estimated
out-of-fold treatment shares, `b = 1`, and 4,000 replications at `n = 10,000`
give outward endpoint coverage of 93.125% and 94.050%, even **counting every
unavailable case as covered**. The Monte Carlo standard error is about
0.4 percentage points. A million-draw simulation of the Gaussian limit
gives 93.4725% and 93.4998%. Independent deterministic Gauss-Hermite
quadrature at orders 160, 320, and 640 gives 93.4594814%, 93.4594610%, and
93.4594607%. The deterministic result supplies a precise undercoverage
counterexample independently of Monte Carlo noise.

Sources: [`nu2-review-centered-local.csv`](nu2-review-centered-local.csv),
[`nu2-review-centered-local-limit.csv`](nu2-review-centered-local-limit.csv),
and [`nu2-review-quadrature.csv`](nu2-review-quadrature.csv).

## 12. Implemented changes and verification

The patch changes four production R files, their relevant documentation,
the development NEWS entry, and focused tests. The development version
remains `0.2.3.9000`.

1. `R/short-parameters.R`: propagate the selected empirical floor's score
   for unconditional binary models and PLM; use zero for selected constant
   conditional floors; compute both conditional parameterizations through
   the stable centered identity. All scale variances and covariances are
   then recomputed through the existing propagation path. Inactive scores
   are retained. NA/NaN estimates remain unavailable; nonfinite original
   influence functions are not replaced with finite floor scores.
2. `R/bias-bounds.R`: make numeric confidence bounds reduce exactly to
   effect inference for zero requested confounding; align positive-bias
   zero-scale behavior with the score-based path.
3. `R/benchmarks.R`: handle explicit zero strengths before requiring gains
   and their derivatives. Preserve the distinction between an unavailable
   positive-strength benchmark and an explicitly zero request.
4. `R/dml.R` and synchronized help files: explain selected-floor inference,
   the regular equality argument, and the remaining zero-variance and
   centered square-root limitations.

[`test-21-nu2-floors.R`](../../tests/testthat/test-21-nu2-floors.R) now checks
the justified selected-floor influence functions, their propagated scale
uncertainty, and the existing group cases. The original test's assertion
that the raw influence function remained unchanged was a behavioral check;
it did not establish inferential validity.

[`test-22-nu2-boundary-review.R`](../../tests/testthat/test-22-nu2-boundary-review.R)
adds nine test blocks with 112 expectations. They check boundary influence
identities for all binary targets, binary and continuous PLM, the exact
clipping variance, cancellation and parameterization identities, tiny
positive scores, numeric/score confidence-bound agreement, repetition
combination, and zero-strength benchmarks with unavailable gains or
derivatives. They use deterministic fixtures; long Monte Carlo diagnostics
are kept outside the package test suite.

Verification against the installed corrected package under R 4.3.3:

- Successful package installation.
- Complete testthat suite: **810 expectations in 169 test blocks across 20
  files; zero failed expectations, errors, warnings, or skipped blocks**.
  Initially unavailable `hdm` and `glmnet` dependencies were installed and
  the affected files rerun; the CSV contains the final unique result for
  every block.
- `tools::checkRd()` reports no issues in the three changed help files.
- `git diff --check` passes.
- Both experiment scripts and the independent quadrature complete.

The per-test results are in [`nu2-review-tests.csv`](nu2-review-tests.csv).
This review ran installation and the full test suite; it does not claim a
new local full `R CMD check` or coverage-workflow run.

## 13. Reproduction

Run these commands from the repository root, with the original commit
available in local Git history, R and the package's dependencies installed,
and Python with NumPy and SciPy available:

```sh
R CMD INSTALL .
Rscript -e 'library(testthat); library(dml.sensemakr); test_dir("tests/testthat", reporter="summary", stop_on_failure=TRUE)'
Rscript tools/experiments/nu2-review-simulation.R
Rscript tools/experiments/nu2-review-counterexamples.R
python tools/experiments/nu2-review-quadrature.py
```

The baseline script defaults to the reviewed source commit and seed
20261023. `NU2_MC_REPS` sets `B`, the regular and group replication count;
the degenerate and trimming diagnostics use `2*B` and the centered-null
diagnostics use `3*B`. The saved results use `B = 2,000`. `NU2_SOURCE_REV=worktree` selects the corrected implementation
for an additional baseline run; it overwrites the corresponding result
CSVs, so retain copies if comparing runs. The counterexample script uses
seed 20261024, compares the original source with the working tree,
performs 100,000 clipping replications per sample size, and defaults to
4,000 local replications (`NU2_LOCAL_MC_REPS` can change the latter).
It also uses one million Gaussian draws for the limiting calculation.
The quadrature script is deterministic.

The recorded baseline runtime and conventions are in
[`nu2-review-simulation-session.txt`](nu2-review-simulation-session.txt).
The test runtime used `testthat 3.2.1`, `caret 6.0-94`, `ranger 0.16.0`,
`hdm 0.3.2`, and `glmnet 4.1-8`, with R 4.3.3. The `tools` directory is
already excluded from package builds by the repository's `.Rbuildignore`.

## 14. Scope of the remaining inferential limitation

The implemented changes resolve established score-propagation and
floating-point defects. They preserve the valid fixed-interior and regular
boundary first-order theory. They do **not** claim uniform validity of
centered normal endpoint intervals near zero, nominal normal norm intervals
with zero first-order variance, a cure for arbitrary nuisance misspecification,
or simultaneous coverage across groups or sensitivity values.

A uniformly justified centered boundary procedure would need a new
inferential construction, accounting for the quadratic nuisance estimation
terms and the square root. The local counterexample rules out declaring
finite positive output sufficient. The mathematical review supports
explicitly documenting that limitation now, while avoiding an arbitrary
cutoff or a purported universal standard-error adjustment without a proof.
