# dml.sensemakr (development version)

* Binding `nu2.s` constraints now propagate the influence function of the
  selected lower bound, including its empirical treatment share or variance.
  This fixes understated uncertainty when fixed propensity trimming keeps the
  raw score below an estimated floor. Inactive constraints keep their scores.
* Conditional ATT/ATU imbalance scores use an algebraically equivalent stable
  expression near the centered boundary, shared by both parameterizations.
* Numeric confidence bounds and covariate benchmark bounds preserve treatment
  effect uncertainty under explicitly zero confounding, including zero scales
  and irrelevant undefined relative gains.
* Documentation clarifies the limits of ordinary normal inference at a
  degenerate imbalance and of square-root inference near a zero centered
  sensitivity scale.
* Group-specific estimates now inherit the fitted model's propensity-score
  trimming bounds, including groups added later with `dml_gate()`.
* Package checks now cover the declared testing dependencies, vignettes, and
  PDF manual. Test fixtures and documentation have been updated for clean,
  reproducible package development.
* Invalid negative sensitivity scales return unavailable bounds with the
  diagnostic warning, without an additional square-root warning.
* Regression-only learners no longer request class probabilities. Rigorous
  lasso uses its declared `hdm` dependency without trying to attach `rlasso`.
* The warning about different outcome learners for a partially linear model
  is emitted once per fit rather than once per cross-fitting fold.

# dml.sensemakr 0.2.3

* Estimates of `nu2.s` now respect their parameter-space lower bounds in all
  implemented models and parameterizations, including group-specific effects.
  The original influence functions are retained, and sensitivity scales are
  recomputed using the constrained estimates.
* At a zero sensitivity scale, zero-confounding bounds retain the treatment
  effect standard error. Bound standard errors requiring the square-root
  delta method are reported as unavailable when confounding is positive.

# dml.sensemakr 0.2.0

* Initial CRAN submission.
* Updated reference to Chernozhukov, Cinelli, Newey, Sharma, and Syrgkanis (2026), Review of Economics and Statistics.
* Bug fixes in `robustness_value()` for `dml.bounds` objects.
* Improved documentation throughout.
