% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensemakr.R
\name{plot.dml.sensemakr}
\alias{plot.dml.sensemakr}
\title{Sensitivity analysis plots for dml.sensemakr}
\usage{
\method{plot}{dml.sensemakr}(
  model,
  parameter = c("ate", "att", "atu"),
  which.bound = c("lwr", "upr"),
  level = 0.95,
  combine.method = "median",
  ...
)
}
\arguments{
\item{model}{an object of class \code{\link{dml}}.}

\item{which.bound}{show contours for the lower limit (\code{lwr}) or upper limit (\code{upr}) of confidence bounds?}

\item{level}{confidence level. Default is \code{0.95}.}

\item{combine.method}{method to combine the results of each repetition of the DML fit. Options are \code{mean} and \code{median}. Default is \code{median}.}

\item{...}{arguments passed to other methods.}

\item{x}{an object of class \code{dml.sensemakr} created with the \code{\link{sensemakr}} function.}
}
\description{
This function provides the contour plots of the sensitivity analysis results obtained with the function \code{\link{sensemakr}} for IV. It is basically a dispatcher to the core plot function \code{\link{ovb_contour_plot}}.
}
