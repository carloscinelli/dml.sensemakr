#' @importFrom graphics contour par points text
#' @importFrom stats coef confint median optim pnorm predict printCoefmat qnorm setNames var
#' @importFrom utils capture.output
NULL

# Suppress R CMD check NOTE for ggplot2 NSE variables used in aes()
utils::globalVariables(c("coefficient"))

.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("See details in:")
    packageStartupMessage("- Chernozhukov, V. Cinelli, C. Newey, W. Sharma, A. Syrgkanis, V. (2026). Long Story Short: Omitted Variable Bias in Causal Machine Learning. Review of Economics and Statistics.")
    packageStartupMessage( "- Available at: https://doi.org/10.1162/REST.a.1705" )
    packageStartupMessage("- Wang, J. Sant'Anna, P.H.C. Chernozhukov, V. Cinelli, C. (2026). Omitted Variable Bias in Difference-in-Differences Designs. Working Paper.")
  }
