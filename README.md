
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
# run DML (partially linear model)
dml.plm <- dml(y, d, x, model = "plm", groups = g1, cf.folds = 5, cf.reps = 5)
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
summary(dml.plm)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Partially Linear 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.263), treatment (ranger, R2 = 0.117)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8978.9     1365.1  6.5776 4.782e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1   4470.4     1036.9  4.3112 1.624e-05 ***
#> gate.q2   3180.6     1254.3  2.5357 0.0112208 *  
#> gate.q3   6785.9     1925.4  3.5245 0.0004244 ***
#> gate.q4  18226.3     4071.0  4.4771 7.567e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.

# summary of results with mean method
summary(dml.plm, combine.method = "mean")
#> 
#> Debiased Machine Learning
#> 
#>  Model: Partially Linear 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.262), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8983.4     1337.7  6.7156 1.873e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1   4464.5     1034.9  4.3138 1.605e-05 ***
#> gate.q2   3225.3     1201.2  2.6850 0.0072523 ** 
#> gate.q3   6804.1     1853.3  3.6713 0.0002414 ***
#> gate.q4  18164.4     3990.9  4.5514 5.329e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the mean method.
```

### extract coefs, se, confidence intervals

``` r
# coef median method (default)
coef(dml.plm)
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8978.883  4470.375  3180.599  6785.947 18226.276

# coef mean method (default)
coef(dml.plm, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8983.350  4464.520  3225.331  6804.128 18164.356

# se median method (default)
se(dml.plm); 
#>      ate  gate.q1  gate.q2  gate.q3  gate.q4 
#> 1365.076 1036.921 1254.304 1925.390 4071.009

# se median method (default)
se(dml.plm, combine.method = "mean")
#>      ate  gate.q1  gate.q2  gate.q3  gate.q4 
#> 1337.685 1034.942 1201.227 1853.348 3990.944

# confint median method (default)
confint(dml.plm) 
#>              2.5 %    97.5 %
#> ate      6303.3823 11654.383
#> gate.q1  2438.0482  6502.702
#> gate.q2   722.2076  5638.990
#> gate.q3  3012.2521 10559.641
#> gate.q4 10247.2451 26205.306

# confint mean method
confint(dml.plm, combine.method = "mean")
#>             2.5 %    97.5 %
#> ate      6361.535 11605.165
#> gate.q1  2436.071  6492.969
#> gate.q2   870.969  5579.694
#> gate.q3  3171.633 10436.622
#> gate.q4 10342.249 25986.463
```

### plot

``` r
plot(dml.plm)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### You can add groups after the model is fit

``` r
## compute GATE by married
g2 <- ifelse(pension$marr, "married", "not.married")
dml.g2 <- dml_gate(dml.fit = dml.plm, groups = g2)
summary(dml.g2)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Partially Linear 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.263), treatment (ranger, R2 = 0.117)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8978.9     1365.1  6.5776 4.782e-11 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>                  Estimate Std. Error t value   P(>|t|)    
#> gate.married       9946.8     1849.8  5.3772 7.566e-08 ***
#> gate.not.married   7545.4     1928.3  3.9129 9.119e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.

coef(dml.g2)
#>              ate     gate.married gate.not.married 
#>         8978.883         9946.838         7545.380
confint(dml.g2)
#>                     2.5 %   97.5 %
#> ate              6303.382 11654.38
#> gate.married     6321.253 13572.42
#> gate.not.married 3765.924 11324.84
plot(dml.g2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />
