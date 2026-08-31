# dml.sensemakr 0.2.2

* `confidence_bounds()` now returns the widest confidence envelope over all
  confounding no stronger than the supplied sensitivity values by default.
  Set `max = FALSE` to recover the previous fixed-corner calculation.
* Robustness values now use the exact confidence-envelope definition for mean
  and median aggregation, including interior extrema.

# dml.sensemakr 0.2.0

* Initial CRAN submission.
* Updated reference to Chernozhukov, Cinelli, Newey, Sharma, and Syrgkanis (2026), Review of Economics and Statistics.
* Bug fixes in `robustness_value()` for `dml.bounds` objects.
* Improved documentation throughout.
