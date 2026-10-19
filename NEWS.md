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
