% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensemakr.R
\name{sensemakr}
\alias{sensemakr}
\alias{sensemakr.dml}
\title{Sensitivity Analysis for Causal Machine Learning}
\usage{
sensemakr(...)

\method{sensemakr}{dml}(
  model,
  benchmark_covariates = NULL,
  cf.y = NULL,
  cf.d = cf.y,
  rho2 = 1,
  bound_label = "Confounding Scenario",
  theta = 0,
  alpha = 0.05,
  ...
)
}
\arguments{
\item{model}{a model created with the function \code{\link{dml}}.}

\item{benchmark_covariates}{character vector of the names of covariates that will be used to bound the plausible strength of the latent variables.}

\item{cf.y}{(optional) R2 based strength of confounding in the outcome regression. It corresponds to the parameter R^2_\{y-g_s ~ g-g_s\} in Chernozhukov et al (2023). Generally, it is equal by the (nonparametric) partial R2 of the confounders with the outcome. Default is NULL.}

\item{cf.d}{(optional) R2 based strength of confounding in the Riesz representer (RR). It corresponds to the parameter 1-R^2_\{alpha ~ alpha_s\} in Chernozhukov et al (2023). It quantifies how much variation latent variables create in the RR. This interpretation can be refined for specific cases. For instance, if the target is the ATE in a partially linear model, this quantity reduces to the (nonparametric) partial R2 of the confounders with the treatment. If the target is the ATE in a nonparametric model with a binary treatment, this quantity reduces to the gains in precision in the treatment model due to latent variables.}

\item{bound_label}{label to bounds provided manually in \code{cf.y} and \code{cf.d}.}

\item{theta}{null hypothesis.}

\item{alpha}{significance level.}
}
\value{
An object of class \code{dml.sensemakr}, containing sensitivity analysis results.
}
\description{
This function performs sensitivity analysis of causal effect estimates as discussed in Chernozhukov et al (2023).
The main input is an object of class \code{\link{dml}}. It returns an object of class \code{dml.sensemakr} with several pre-computed sensitivity statistics for reporting. After running \code{sensemakr} you may directly use the \code{plot}, \code{print} and \code{summary} methods in the returned object.
}
\examples{
# loads package
library(dml.sensemakr)

# loads data
data("pension")

# set treatment, outcome and covariates
y <- pension$net_tfa  # net total financial assets
d <- pension$e401     # 401K eligibility
x <- model.matrix(~ -1 + age + inc  + educ+ fsize + marr + twoearn + pira + hown, data = pension)

# run DML (nonparametric model)
dml.401k <- dml(y, d, x, model = "npm")

# sensitivity analysis
sens.401k <- sensemakr(dml.401k, cf.y = 0.04, cf.d = 0.03)

# summary
summary(sens.401k)

# contout plots
plot(sens.401k)

}
