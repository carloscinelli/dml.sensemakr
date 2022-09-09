
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
#> See details in:
#> - Chernozhukov, V. Cinelli, C. Newey, W. Sharma, A. Syrgkanis, V. (2021). Long Story Short: Omitted Variable Bias in Causal Machine Learning. National Bureau of Economic Research, Working Paper Series, 30302.
#> - Available at: http://www.nber.org/papers/w30302

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
#> Debiased Machine Learning
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
#>  ML Method: outcome (ranger, R2 = 0.272), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   7974.7     1196.1  6.6671 2.609e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1  4543.95     800.67  5.6752 1.385e-08 ***
#> gate.q2  2862.56    1235.35  2.3172 0.0204925 *  
#> gate.q3  6608.25    1897.20  3.4832 0.0004955 ***
#> gate.q4 18017.94    4117.30  4.3762 1.208e-05 ***
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
#>  ML Method: outcome (ranger, R2 = 0.267), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8018.8     1174.6  6.8266 8.694e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1  4551.49     793.59  5.7353 9.732e-09 ***
#> gate.q2  2836.34    1239.64  2.2880 0.0221353 *  
#> gate.q3  6516.32    1850.55  3.5213 0.0004295 ***
#> gate.q4 18169.11    4047.24  4.4893 7.147e-06 ***
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
#>  7974.679  4543.954  2862.556  6608.254 18017.938

# coef mean method (default)
coef(dml.401k, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8018.813  4551.489  2836.339  6516.319 18169.114

# se median method (default)
se(dml.401k); 
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#> 1196.1215  800.6714 1235.3476 1897.2027 4117.3007

# se median method (default)
se(dml.401k, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#> 1174.6391  793.5864 1239.6381 1850.5549 4047.2440

# confint median method (default)
confint(dml.401k) 
#>             2.5 %    97.5 %
#> ate     5630.3240 10319.034
#> gate.q1 2974.6672  6113.241
#> gate.q2  441.3189  5283.793
#> gate.q3 2889.8054 10326.703
#> gate.q4 9948.1773 26087.700

# confint mean method
confint(dml.401k, combine.method = "mean")
#>              2.5 %    97.5 %
#> ate      5716.5626 10321.063
#> gate.q1  2996.0886  6106.890
#> gate.q2   406.6928  5265.985
#> gate.q3  2889.2984 10143.340
#> gate.q4 10236.6617 26101.567
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
#>  ML Method: outcome (ranger, R2 = 0.272), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   7974.7     1196.1  6.6671 2.609e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>                  Estimate Std. Error t value   P(>|t|)    
#> gate.married       8859.1     1820.2  4.8670 1.133e-06 ***
#> gate.not.married   6471.2     1430.2  4.5246 6.052e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.

coef(dml.401k.g2)
#>              ate     gate.married gate.not.married 
#>         7974.679         8859.084         6471.190
confint(dml.401k.g2)
#>                     2.5 %    97.5 %
#> ate              5630.324 10319.034
#> gate.married     5291.488 12426.681
#> gate.not.married 3667.978  9274.403
plot(dml.401k.g2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Sensitivity Analysis

### Robustness Values

``` r
robustness_value(dml.401k, alpha = 0.05)
#>        ate    gate.q1    gate.q2    gate.q3    gate.q4 
#> 0.04483993 0.07668492 0.01161599 0.03329131 0.05751967
```

### Confidence Bounds

``` r
confidence_bounds(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, level = 0.95)
#>                lwr        upr
#> ate      1389.5027 14617.5424
#> gate.q1  1815.2113  7344.9735
#> gate.q2 -1733.5463  6758.9240
#> gate.q3  -158.8846 13079.2771
#> gate.q4  4586.7343 31737.8295
#> 
#> Confidence level: point = 95%; region = 90%.
#> Sensitivity parameters: r2ya.dx = 0.03; r2.rr = 0.04; rho2 = 1.
```

### Contour Plots

``` r
ovb_contour_plot(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, level = 0.95)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

### Further results

``` r
bounds.401k <- dml_bounds(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04)
bounds.401k
#> 
#> Debiased Machine Learning: Bounds on Omitted Variable Bias
#> 
#> Sensitivity Parameters
#>   r2ya.dx = 0.03
#>   r2rr = 0.04
#>   rho = 1 
#> 
#> Bounds on Average Treatment Effect: 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  7974.68    1196.12  6.6671 2.609e-11 ***
#> |Bias| Bound    4501.22     348.05 12.9328 < 2.2e-16 ***
#> Lower Bound     3421.72    1235.50  2.7695  0.005614 ** 
#> Upper Bound    12551.81    1255.88  9.9945 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> 
#> Bounds on Group Average Treatment Effect: Group q1 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  4543.95     800.67  5.6752 1.385e-08 ***
#> |Bias| Bound    1433.17     115.67 12.3900 < 2.2e-16 ***
#> Lower Bound     3109.46     786.85  3.9518 7.756e-05 ***
#> Upper Bound     5978.27     830.90  7.1950 6.247e-13 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q2 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  2862.56    1235.35  2.3172   0.02049 *  
#> |Bias| Bound    2116.86     440.48  4.8058 1.542e-06 ***
#> Lower Bound      746.27    1507.62  0.4950   0.62060    
#> Upper Bound     4978.84    1082.21  4.6006 4.212e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q3 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate  6608.25    1897.20  3.4832 0.0004955 ***
#> |Bias| Bound    3363.56     573.57  5.8642 4.512e-09 ***
#> Lower Bound     3244.69    2069.23  1.5681 0.1168650    
#> Upper Bound     9971.82    1889.20  5.2783 1.304e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Bounds on Group Average Treatment Effect: Group q4 
#> 
#>                Estimate Std. Error t value   P(>|t|)    
#> Short Estimate 18017.94    4117.30  4.3762 1.208e-05 ***
#> |Bias| Bound    6682.23     623.34 10.7201 < 2.2e-16 ***
#> Lower Bound    11335.71    4103.09  2.7627  0.005732 ** 
#> Upper Bound    24791.25    4223.22  5.8702 4.352e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.
```

``` r
coef(bounds.401k)
#>                  ate  gate.q1  gate.q2  gate.q3   gate.q4
#> theta.s     7974.679 4543.954 2862.556 6608.254 18017.938
#> bias.bound  4501.221 1433.168 2116.865 3363.564  6682.229
#> theta.m     3421.721 3109.456  746.268 3244.690 11335.709
#> theta.p    12551.810 5978.272 4978.843 9971.819 24791.253
se(bounds.401k)
#>                  ate  gate.q1   gate.q2   gate.q3   gate.q4
#> theta.s    1196.1215 800.6714 1235.3476 1897.2027 4117.3007
#> bias.bound  348.0474 115.6709  440.4846  573.5722  623.3374
#> theta.m    1235.5011 786.8452 1507.6200 2069.2264 4103.0853
#> theta.p    1255.8760 830.8954 1082.2121 1889.2005 4223.2186
confint(bounds.401k)
#> $ate
#>                2.5 %    97.5 %
#> theta.s     5630.324 10319.034
#> bias.bound  3819.061  5183.381
#> theta.m     1000.184  5843.259
#> theta.p    10090.338 15013.282
#> 
#> $gate.q1
#>               2.5 %   97.5 %
#> theta.s    2974.667 6113.241
#> bias.bound 1206.457 1659.879
#> theta.m    1567.268 4651.645
#> theta.p    4349.747 7606.797
#> 
#> $gate.q2
#>                 2.5 %   97.5 %
#> theta.s      441.3189 5283.793
#> bias.bound  1253.5308 2980.199
#> theta.m    -2208.6130 3701.149
#> theta.p     2857.7466 7099.940
#> 
#> $gate.q3
#>                2.5 %    97.5 %
#> theta.s    2889.8054 10326.703
#> bias.bound 2239.3836  4487.745
#> theta.m    -810.9193  7300.299
#> theta.p    6269.0538 13674.584
#> 
#> $gate.q4
#>                2.5 %    97.5 %
#> theta.s     9948.177 26087.700
#> bias.bound  5460.510  7903.948
#> theta.m     3293.810 19377.609
#> theta.p    16513.897 33068.609
```
