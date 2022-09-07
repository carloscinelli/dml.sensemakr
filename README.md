
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dml.sensemakr

<!-- badges: start -->
<!-- badges: end -->

## Development version

To install the development version on GitHub make sure you have the
package devtools installed.

``` r
# install.packages("devtools") 
devtools::install_github("carloscinelli/dml.sensemakr")
```

## Example

### Data

``` r
# loads package
library(dml.sensemakr)

## loads data
data("pension")
y <- pension$net_tfa  # net total financial assets
d <-  pension$e401    # 401K eligibility
x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)

## compute income quartiles
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE), 
          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)
```

### Fit DML

``` r
# run DML (nonparametric model)
dml.401k <- dml(y, d, x, model = "npm", groups = g1, cf.folds = 5, cf.reps = 5)
#> Debiased Machine Learning
#> 
#> ======================================
#> Repeating 5-fold cross-fitting 5 times
#> ======================================
#> 
#> -- Rep 1 -- Folds: 1  2  3  4  5  
#> 
#> -- Rep 2 -- Folds: 1  2  3  4  5  
#> 
#> -- Rep 3 -- Folds: 1  2  3  4  5  
#> 
#> -- Rep 4 -- Folds: 1  2  3  4  5  
#> 
#> -- Rep 5 -- Folds: 1  2  3  4  5
```

### Explore results

``` r
# summary of results with median method (default)
summary(dml.401k)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.27), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8057.6     1216.8   6.622 3.543e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1  4465.47     849.08  5.2592 1.447e-07 ***
#> gate.q2  2804.35    1278.76  2.1930 0.0283062 *  
#> gate.q3  6795.01    1850.59  3.6718 0.0002408 ***
#> gate.q4 18363.82    4250.20  4.3207 1.555e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.

# summary of results with mean method
summary(dml.401k, combine.method = "mean")
#> 
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.271), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8154.3     1182.0  6.8987 5.247e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1   4452.1      798.4  5.5762 2.458e-08 ***
#> gate.q2   2753.1     1264.2  2.1778 0.0294244 *  
#> gate.q3   6819.1     1852.2  3.6817 0.0002317 ***
#> gate.q4  18591.2     4083.2  4.5531 5.286e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the mean method.
```

### extract coefs, se, confidence intervals

``` r
# coef median method (default)
coef(dml.401k)
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8057.599  4465.470  2804.347  6795.006 18363.822

# coef mean method (default)
coef(dml.401k, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8154.348  4452.086  2753.063  6819.148 18591.185

# se median method (default)
se(dml.401k); 
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#> 1216.7906  849.0826 1278.7627 1850.5870 4250.1997

# se median method (default)
se(dml.401k, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#> 1182.0061  798.4031 1264.1759 1852.1556 4083.2002

# confint median method (default)
confint(dml.401k) 
#>              2.5 %    97.5 %
#> ate      5672.7336 10442.465
#> gate.q1  2801.2983  6129.641
#> gate.q2   298.0183  5310.676
#> gate.q3  3167.9221 10422.090
#> gate.q4 10033.5841 26694.061

# confint mean method
confint(dml.401k, combine.method = "mean")
#>              2.5 %    97.5 %
#> ate      5837.6581 10471.037
#> gate.q1  2887.2443  6016.927
#> gate.q2   275.3235  5230.802
#> gate.q3  3188.9895 10449.306
#> gate.q4 10588.2596 26594.110
```

### plot

``` r
plot(dml.401k)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### You can add groups after the model is fit

``` r
## compute GATE by married
g2 <- ifelse(pension$marr, "married", "not.married")
dml.401k.g2 <- dml_gate(dml.fit = dml.401k, groups = g2)
summary(dml.401k.g2)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.27), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8057.6     1216.8   6.622 3.543e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>                  Estimate Std. Error t value   P(>|t|)    
#> gate.married       9239.6     1769.3  5.2222 1.768e-07 ***
#> gate.not.married   6485.3     1427.8  4.5422 5.567e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.

coef(dml.401k.g2)
#>              ate     gate.married gate.not.married 
#>         8057.599         9239.578         6485.308
confint(dml.401k.g2)
#>                     2.5 %    97.5 %
#> ate              5672.734 10442.465
#> gate.married     5771.854 12707.303
#> gate.not.married 3686.891  9283.726
plot(dml.401k.g2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Sensitivity Analysis

### Robustness Values

``` r
robustness_value(dml.401k, alpha = 0.05)
#>         ate     gate.q1     gate.q2     gate.q3     gate.q4 
#> 0.045546544 0.072689224 0.009763251 0.035774387 0.057782410
```

### Confidence Bounds

``` r
confidence_bounds(dml.401k, r2ya.dx = 0.04, r2.rr = 0.03, level = 0.95)
#>                lwr        upr
#> ate      1511.6061 14643.3425
#> gate.q1  1658.0422  7320.2909
#> gate.q2 -1857.9060  6733.6677
#> gate.q3   134.0551 13204.0372
#> gate.q4  4699.8475 32190.4832
#> 
#> Confidence levels: point = 95%; region = 90%.
```

### Further results

``` r
bounds.401k <- dml_bounds(dml.401k, r2ya.dx = 0.04, r2.rr = 0.03)
bounds.401k
#> 
#> Debiased Machine Learning: Bounds on Omitted Variable Bias
#> 
#> Sensitivity Parameters
#>   r2ya.dx = 0.04
#>   r2rr = 0.03
#>   rho = 1 
#> 
#> Bounds on Average Treatment Effect: 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  8057.60    1216.79  6.6220 3.543e-11 ***
#> |Bias| Bound    4486.44     340.86 13.1622 < 2.2e-16 ***
#> Lower Bound     3571.16    1252.12  2.8521  0.004343 ** 
#> Upper Bound    12548.67    1273.47  9.8539 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Bounds on Group Average Treatment Effect: Group q1 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  4465.47     849.08  5.2592 1.447e-07 ***
#> |Bias| Bound    1422.55     115.36 12.3316 < 2.2e-16 ***
#> Lower Bound     3041.81     841.27  3.6157 0.0002995 ***
#> Upper Bound     5889.13     870.08  6.7685 1.301e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q2 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  2804.35    1278.76  2.1930   0.02831 *  
#> |Bias| Bound    2105.40     434.34  4.8474 1.251e-06 ***
#> Lower Bound      693.09    1550.89  0.4469   0.65495    
#> Upper Bound     4900.68    1114.37  4.3977 1.094e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q3 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  6795.01    1850.59  3.6718 0.0002408 ***
#> |Bias| Bound    3350.16     571.49  5.8622 4.568e-09 ***
#> Lower Bound     3439.73    2009.71  1.7116 0.0869783 .  
#> Upper Bound    10150.28    1856.55  5.4673 4.570e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q4 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate 18363.82    4250.20  4.3207 1.555e-05 ***
#> |Bias| Bound    6659.66     605.67 10.9956 < 2.2e-16 ***
#> Lower Bound    11671.58    4238.51  2.7537  0.005893 ** 
#> Upper Bound    25056.07    4337.42  5.7767 7.617e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.
```

``` r
coef(bounds.401k)
#>                  ate  gate.q1   gate.q2   gate.q3   gate.q4
#> theta.s     8057.599 4465.470 2804.3471  6795.006 18363.822
#> bias.bound  4486.442 1422.550 2105.3968  3350.160  6659.656
#> theta.m     3571.157 3041.807  693.0888  3439.730 11671.576
#> theta.p    12548.674 5889.132 4900.6846 10150.282 25056.069
se(bounds.401k)
#>                  ate  gate.q1   gate.q2   gate.q3   gate.q4
#> theta.s    1216.7906 849.0826 1278.7627 1850.5870 4250.1997
#> bias.bound  340.8583 115.3583  434.3359  571.4852  605.6666
#> theta.m    1252.1180 841.2693 1550.8947 2009.7076 4238.5099
#> theta.p    1273.4678 870.0828 1114.3746 1856.5514 4337.4160
confint(bounds.401k)
#> $ate
#>                2.5 %    97.5 %
#> theta.s     5672.734 10442.465
#> bias.bound  3818.372  5154.512
#> theta.m     1117.051  6025.263
#> theta.p    10052.723 15044.625
#> 
#> $gate.q1
#>               2.5 %   97.5 %
#> theta.s    2801.298 6129.641
#> bias.bound 1196.452 1648.648
#> theta.m    1392.949 4690.665
#> theta.p    4183.801 7594.463
#> 
#> $gate.q2
#>                 2.5 %   97.5 %
#> theta.s      298.0183 5310.676
#> bias.bound  1254.1140 2956.680
#> theta.m    -2346.6089 3732.787
#> theta.p     2716.5507 7084.819
#> 
#> $gate.q3
#>                2.5 %    97.5 %
#> theta.s    3167.9221 10422.090
#> bias.bound 2230.0698  4470.250
#> theta.m    -499.2246  7378.684
#> theta.p    6511.5081 13789.056
#> 
#> $gate.q4
#>                2.5 %   97.5 %
#> theta.s    10033.584 26694.06
#> bias.bound  5472.571  7846.74
#> theta.m     3364.249 19978.90
#> theta.p    16554.890 33557.25
```
