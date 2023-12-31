% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_pip_type.R
\name{identify_pip_type}
\alias{identify_pip_type}
\title{Identify PIP type}
\usage{
identify_pip_type(
  welfare,
  weight = rep(1, length(welfare)),
  imputation_id = NULL,
  groupdata_threshold = getOption("pipster.gd_threshold"),
  verbose = getOption("pipster.verbose")
)
}
\arguments{
\item{welfare}{numeric: welfare variable, either income of consumption}

\item{weight}{numeric: expansion sample weighs. Default is a vector o 1s of
the length of \code{welfare}}

\item{imputation_id}{numeric: vector that identifies different imputations.
Default is NULL}

\item{groupdata_threshold}{numeric: threshold to discriminate between micro
data an group data. Default is 200 observations}

\item{verbose}{logical: Whether to display important messages about your data
or query}
}
\value{
character of length 1.
}
\description{
Based on welfare and weight vectors it identifies whether the data is
microdata, group data or imputed data
}
\examples{
# Group data -------
# W: Weights, share of population, sum up to 100
# X: welfare vector with mean welfare by decile
# P:Cumulative share of population
# L: Cumulative share of welfare
# R: share of welfare, sum up to 1.

W = c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64,
16.99, 10, 9.78, 3.96, 1.81, 2.49)
X = c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75,
110.64, 134.9, 167.76, 215.48, 261.66, 384.97)
P = c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133,
0.5497, 0.7196, 0.8196, 0.9174, 0.957, 0.9751, 1)
L = c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808,
0.23498, 0.34887, 0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)

R = (W * X) / sum(W * X)

## type 1 ------
identify_pip_type(welfare = L, weight = P)
identify_pip_type(welfare = L*100, weight = P)

## type 2 -----------
identify_pip_type(welfare = R, weight = W/100)
identify_pip_type(welfare = R*100, weight = W)

## type 5 -----------
identify_pip_type(welfare = X, weight = W/100)
identify_pip_type(welfare = X, weight = W)

## type 3 -----------
identify_pip_type(welfare = X, weight  = P)
identify_pip_type(welfare = X, weight = P*100)

# Microdata -------

l <- 300
Y <- sample(1000, l,replace = TRUE)
Q <- sample(35, l,replace = TRUE)
I <- sample(1:5, l,replace = TRUE)

identify_pip_type(welfare = Y, weight  = Q)
identify_pip_type(welfare = Y, weight  = Q, imputation_id = I)
}
