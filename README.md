
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dml.sensemakr

<!-- badges: start -->
<!-- badges: end -->

`dml.sensemakr` implements a general suite of sensitivity analysis tools
for Causal Machine Learning as discussed in [Chernozhukov, V., Cinelli,
C., Newey, W., Sharma A., and Syrgkanis, V. (2021). “Long Story Short:
Omitted Variable Bias in Causal Machine
Learning.”](https://www.nber.org/papers/w30302)

## Development version

To install the development version on GitHub make sure you have the
package devtools installed.

``` r
# install.packages("devtools") 
devtools::install_github("carloscinelli/dml.sensemakr")
```

# CRAN

CRAN version coming soon.

# Details

For theoretical details of the method [please see the working
paper.](https://www.nber.org/papers/w30302) For a primer on Debiased
Machine Learning, [please check Chernozukohv et al
(2018).](https://academic.oup.com/ectj/article/21/1/C1/5056401)

Some presentations that may be useful:

-   [Carlos’ presentation at the
    ICLR.](http://interactivecausallearning.com/Carlos_Cinelli.html)
-   [Victor’s tutorial at the Chamberlain
    Seminar.](https://www.youtube.com/watch?v=PQtYqKfxH_I)

# Quick Example

## Fit DML model

### Load Data

``` r
# loads package
library(dml.sensemakr)
#> See details in:
#> - Chernozhukov, V. Cinelli, C. Newey, W. Sharma, A. Syrgkanis, V. (2021). Long Story Short: Omitted Variable Bias in Causal Machine Learning. National Bureau of Economic Research, Working Paper Series, 30302.
#> - Available at: http://www.nber.org/papers/w30302

## loads data
data("pension")

# set the outcome
y <- pension$net_tfa  # net total financial assets

# set the treatment
d <- pension$e401    # 401K eligibility

# set the covariates (a matrix)
x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)

## compute income quartiles for group ATE.
g1 <- cut(x[,"inc"], quantile(x[,"inc"], c(0, 0.25,.5,.75,1), na.rm = TRUE), 
          labels = c("q1", "q2", "q3", "q4"), include.lowest = T)
```

### Use DML to estimate the ATE

``` r
# run DML (nonparametric model)
## 2 folds (change as needed)
## 1 repetition (change as needed)
dml.401k <- dml(y, d, x, model = "npm", groups = g1, cf.folds = 2, cf.reps = 1)
#> Debiased Machine Learning
#> 
#> ======================================
#> Repeating 2-fold cross-fitting 1 times
#> ======================================
#> 
#> -- Rep 1 -- Folds: 1  2
```

### Explore results

``` r
# summary of results with median method (default)
summary(dml.401k, combine.method = "median")
#> 
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Cross-Fitting: 2 folds, 1 reps 
#>  ML Method: outcome (ranger, R2 = 24.56%), treatment (ranger, R2 = 11.278%)
#>  Tuning: clean 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   7652.3     1190.5  6.4278 1.295e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Group Average Treatment Effect: 
#> 
#>         Estimate Std. Error t value   P(>|t|)    
#> gate.q1  4621.43     787.44  5.8690 4.385e-09 ***
#> gate.q2  2431.07    1313.50  1.8508 0.0641936 .  
#> gate.q3  6675.07    1871.00  3.5676 0.0003602 ***
#> gate.q4 16879.29    4096.46  4.1205 3.781e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Note: DML estimates combined using the median method.
```

### extract coefs, se, confidence intervals

``` r
# coef median method (default)
coef(dml.401k, combine.method = "median")
#>       ate   gate.q1   gate.q2   gate.q3   gate.q4 
#>  7652.253  4621.429  2431.071  6675.066 16879.286

# se median method (default)
se(dml.401k, combine.method = "median")
#>      ate  gate.q1  gate.q2  gate.q3  gate.q4 
#> 1190.493  787.436 1313.501 1871.001 4096.461

# confint median method
confint(dml.401k, combine.method = "median")
#>             2.5 %    97.5 %
#> ate     5318.9302  9985.576
#> gate.q1 3078.0828  6164.775
#> gate.q2 -143.3435  5005.485
#> gate.q3 3007.9722 10342.160
#> gate.q4 8850.3694 24908.202
```

<!-- ### Coefficients Plot -->
<!-- ```{r} -->
<!-- plot(dml.401k) -->
<!-- ``` -->
<!-- ### You can add groups after the model is fit -->
<!-- ```{r} -->
<!-- ## compute GATE by married -->
<!-- g2 <- ifelse(pension$marr, "married", "not.married") -->
<!-- dml.401k.g2 <- dml_gate(dml.fit = dml.401k, groups = g2) -->
<!-- summary(dml.401k.g2) -->
<!-- coef(dml.401k.g2) -->
<!-- confint(dml.401k.g2) -->
<!-- plot(dml.401k.g2) -->
<!-- ``` -->

## Sensitivity Analysis

### Robustness Values

``` r
robustness_value(dml.401k, alpha = 0.05)
#>          ate      gate.q1      gate.q2      gate.q3      gate.q4 
#> 4.248086e-02 7.715037e-02 9.046863e-09 3.427966e-02 5.114538e-02
```

### Confidence Bounds

``` r
confidence_bounds(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, level = 0.95)
#>                 lwr         upr
#> ate      1072.24907 14235.65414
#> gate.q1  1881.87070  7423.34994
#> gate.q2 -2289.88456  6474.80801
#> gate.q3   -50.34364 13164.39540
#> gate.q4  3343.89125 30523.65679
#> 
#> Confidence level: point = 95%; region = 90%.
#> Sensitivity parameters: r2ya.dx = 0.03; r2.rr = 0.04; rho2 = 1.
```

### Contour Plots

``` r
ovb_contour_plot(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, bound.label = "Max Match (3x years)")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

<!-- ### Further results -->
<!-- ```{r} -->
<!-- bounds.401k <- dml_bounds(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04) -->
<!-- bounds.401k -->
<!-- ``` -->
<!-- ```{r} -->
<!-- coef(bounds.401k) -->
<!-- se(bounds.401k) -->
<!-- confint(bounds.401k) -->
<!-- ``` -->
