% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipgd_lorenz.R
\name{pipgd_validate_lorenz}
\alias{pipgd_validate_lorenz}
\title{Check validity of Lorenz Curve}
\usage{
pipgd_validate_lorenz(
  params = NULL,
  welfare = NULL,
  weight = NULL,
  mean = 1,
  times_mean = 1,
  popshare = NULL,
  povline = ifelse(is.null(popshare), mean * times_mean, NA_real_),
  complete = getOption("pipster.return_complete")
)
}
\arguments{
\item{params}{list of parameters from \code{pipgd_validate_lorenz()}}

\item{welfare}{numeric vector of cumulative share of welfare (income/consumption)}

\item{weight}{numeric vector of cumulative share of the population}

\item{mean}{numeric: welfare mean of distribution.}

\item{times_mean}{numeric factor that multiplies the mean to create a
relative povertyline. Default is 1}

\item{popshare}{numeric: range (0,1). Share of population. Provide share of
population instead of poverty line}

\item{povline}{numeric: value of poverty line. Default is the \code{mean} value}

\item{complete}{logical: If TRUE, returns a list a cumulative returns from
previously used \code{get_gd} functions. Default is \code{FALSE}}
}
\value{
list of distributional validity of each Lorenz model
}
\description{
Check validity of Lorenz Curve
}
\examples{
# Using Lorenz parameters from pipgd_validate_lorenz
 res <-
pipgd_params(welfare = pip_gd$L,
             weight = pip_gd$P) |>
  pipgd_validate_lorenz() |>
  pipgd_select_lorenz()

# Using welfare and population vecotrs
res2 <- pipgd_select_lorenz(welfare = pip_gd$L,
                            weight = pip_gd$P)
identical(res, res2)
}
