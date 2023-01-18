
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dml.sensemakr

<!-- badges: start -->
<!-- badges: end -->

`dml.sensemakr` implements a general suite of sensitivity analysis tools
for Causal Machine Learning as discussed in [Chernozhukov, V., Cinelli,
C., Newey, W., Sharma A., and Syrgkanis, V. (2021). “Long Story Short:
Omitted Variable Bias in Causal Machine
Learning.”](https://www.nber.org/papers/w30302)

# Development version

To install the development version on GitHub make sure you have the
package devtools installed.

``` r
# install.packages("devtools") 
devtools::install_github("carloscinelli/dml.sensemakr")
```

# CRAN

CRAN version coming soon.

# Details

For theoretical details [please see the working
paper.](https://www.nber.org/papers/w30302)

For a primer on Debiased Machine Learning, [please check Chernozukohv et
al (2018).](https://academic.oup.com/ectj/article/21/1/C1/5056401)

Some presentations that may be useful:

-   [Carlos’ presentation at the
    ICLR.](http://interactivecausallearning.com/Carlos_Cinelli.html)
-   [Victor’s tutorial at the Chamberlain
    Seminar.](https://www.youtube.com/watch?v=PQtYqKfxH_I)

# Basic Usage

``` r
# loads package
library(dml.sensemakr)
#> See details in:
#> - Chernozhukov, V. Cinelli, C. Newey, W. Sharma, A. Syrgkanis, V. (2021). Long Story Short: Omitted Variable Bias in Causal Machine Learning. National Bureau of Economic Research, Working Paper Series, 30302.
#> - Available at: http://www.nber.org/papers/w30302

## loads data
data("pension")

# set treatment, outcome and covariates
y <- pension$net_tfa  # net total financial assets
d <- pension$e401     # 401K eligibility
x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)

# run DML (nonparametric model)
dml.401k <- dml(y, d, x, model = "npm")
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Target: ate 
#>  Cross-Fitting: 5 folds, 1 reps 
#>  ML Method: outcome (ranger), treatment (ranger)
#>  Tuning: dirty 
#> 
#> 
#> ====================================
#> Tuning parameters using all the data
#> ====================================
#> 
#> - Tuning Model for D.
#> -- Best Tune:
#>   mtry min.node.size splitrule
#> 1    2             5  variance
#> 
#> - Tuning Model for Y (non-parametric).
#> -- Best Tune:
#>   mtry min.node.size splitrule
#> 1    3             5  variance
#> 
#> 
#> ======================================
#> Repeating 5-fold cross-fitting 1 times
#> ======================================
#> 
#> -- Rep 1 -- Folds: 1  2  3  4  5

# summary of results with median method (default)
summary(dml.401k)
#> 
#> Debiased Machine Learning
#> 
#>  Model: Nonparametric 
#>  Cross-Fitting: 5 folds, 1 reps 
#>  ML Method: outcome (ranger, R2 = 26.274%), treatment (ranger, R2 = 11.469%)
#>  Tuning: dirty 
#> 
#> Average Treatment Effect: 
#> 
#>     Estimate Std. Error t value   P(>|t|)    
#> ate   8081.6     1171.3  6.8997 5.211e-12 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> Note: DML estimates combined using the median method.

# robustness values
robustness_value(dml.401k, alpha = 0.05)
#>        ate 
#> 0.04628754

# confidence bounds
confidence_bounds(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, level = 0.95)
#>           lwr       upr
#> ate  1588.546 14628.766
#> 
#> Confidence level: point = 95%; region = 90%.
#> Sensitivity parameters: r2ya.dx = 0.03; r2.rr = 0.04; rho2 = 1.

# contour plots
ovb_contour_plot(dml.401k, r2ya.dx = 0.03, r2.rr = 0.04, bound.label = "Max Match (3x years)")
```

<img src="man/figures/README-basic-usage-1.png" width="100%" style="display: block; margin: auto;" />

<!-- ### Use DML to estimate the ATE -->
<!-- ```{r} -->
<!-- ``` -->
<!-- ### Explore results -->
<!-- ```{r} -->
<!-- ``` -->
<!-- ### Extract coefs, se, confidence intervals -->
<!-- ```{r} -->
<!-- # coef median method (default) -->
<!-- coef(dml.401k, combine.method = "median") -->
<!-- # se median method (default) -->
<!-- se(dml.401k, combine.method = "median") -->
<!-- # confint median method -->
<!-- confint(dml.401k, combine.method = "median") -->
<!-- ``` -->
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
<!-- ## Sensitivity Analysis -->
<!-- ### Robustness Values -->
<!-- ```{r} -->
<!-- ``` -->
<!-- ### Confidence Bounds -->
<!-- ```{r} -->
<!-- ``` -->
<!-- ### Contour Plots -->
<!-- ```{r, fig.width=12} -->
<!-- ``` -->
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
