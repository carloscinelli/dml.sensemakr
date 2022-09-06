
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
#>  ML Method: outcome (ranger, R2 = 0.271), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8965.2     1388.6  6.4564 1.072e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1   4466.0     1058.5  4.2190 2.454e-05 ***
#> gate.q2   3303.7     1213.3  2.7229 0.0064703 ** 
#> gate.q3   6788.9     1859.7  3.6504 0.0002618 ***
#> gate.q4  18393.8     4051.2  4.5403 5.616e-06 ***
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
#>  ML Method: outcome (ranger, R2 = 0.269), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   9073.0     1328.7  6.8283 8.591e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1   4484.8     1025.4  4.3738 1.221e-05 ***
#> gate.q2   3240.8     1191.1  2.7208 0.0065119 ** 
#> gate.q3   6731.6     1844.4  3.6498 0.0002624 ***
#> gate.q4  18522.3     3939.8  4.7013 2.585e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.
```

### extract coefs, se, confidence intervals

``` r
# coef median method (default)
coef(dml.plm)
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  8965.198  4466.042  3303.666  6788.857 18393.828

# coef mean method (default)
coef(dml.plm, combine.method = "mean")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  9073.003  4484.794  3240.847  6731.565 18522.309

# se median method (default)
se(dml.plm); 
#>      ate  gate.q1  gate.q2  gate.q3  gate.q4 
#> 1388.567 1058.547 1213.269 1859.737 4051.194

# se median method (default)
se(dml.plm, combine.method = "mean")
#>      ate  gate.q1  gate.q2  gate.q3  gate.q4 
#> 1328.729 1025.371 1191.127 1844.364 3939.797

# confint median method (default)
confint(dml.plm) 
#>              2.5 %    97.5 %
#> ate      6243.6563 11686.740
#> gate.q1  2391.3276  6540.756
#> gate.q2   925.7016  5681.630
#> gate.q3  3143.8398 10433.875
#> gate.q4 10453.6339 26334.022

# confint mean method
confint(dml.plm, combine.method = "mean")
#>              2.5 %    97.5 %
#> ate      6468.7433 11677.264
#> gate.q1  2475.1033  6494.486
#> gate.q2   906.2816  5575.412
#> gate.q3  3116.6781 10346.452
#> gate.q4 10800.4491 26244.168
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

## compute GATEs
dml.g2 <- dml_gate(dml.fit = dml.plm, groups = g2)
dml.g2
#> 
#> Debiased Machine Learning
#> 
#> Call:
#> dml(y = y, d = d, x = x, model = "plm", groups = g2, cf.folds = 5, 
#>     cf.reps = 5)
#> 
#> Estimates:
#>              ate      gate.married  gate.not.married  
#>             8965              9890              7346

summary(dml.g2)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Partially Linear 
#>  Cross-Fitting: 5 folds, 5 reps 
#>  ML Method: outcome (ranger, R2 = 0.271), treatment (ranger, R2 = 0.116)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8965.2     1388.6  6.4564 1.072e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>                  Estimate Std. Error t value   P(>|t|)    
#> gate.married       9889.7     1906.3  5.1879 2.126e-07 ***
#> gate.not.married   7346.2     1742.4  4.2161 2.486e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.
coef(dml.g2)
#>              ate     gate.married gate.not.married 
#>         8965.198         9889.721         7346.183
confint(dml.g2)
#>                     2.5 %   97.5 %
#> ate              6243.656 11686.74
#> gate.married     6153.456 13625.99
#> gate.not.married 3931.085 10761.28
plot(dml.g2)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />
