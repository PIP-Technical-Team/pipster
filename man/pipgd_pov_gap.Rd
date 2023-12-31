% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipgd_pov.R
\name{pipgd_pov_gap}
\alias{pipgd_pov_gap}
\title{Estimate poverty gap (FGT1)}
\usage{
pipgd_pov_gap(
  params = NULL,
  welfare = NULL,
  weight = NULL,
  mean = 1,
  times_mean = 1,
  popshare = NULL,
  povline = ifelse(is.null(popshare), mean * times_mean, NA_real_),
  format = c("dt", "list", "atomic"),
  lorenz = NULL,
  complete = getOption("pipster.return_complete")
)
}
\arguments{
\item{params}{list of parameters from \code{pipgd_validate_lorenz()}}

\item{welfare}{numeric vector of cumulative share of welfare (income/consumption)}

\item{weight}{numeric vector of cumulative share of the population}

\item{mean}{numeric scalar of distribution mean. Default is NULL}

\item{times_mean}{numeric factor that multiplies the mean to create a
relative povertyline. Default is 1}

\item{popshare}{numeric: range (0,1). Share of population. Provide share of
population instead of poverty line}

\item{povline}{numeric: value of poverty line. Default is the \code{mean} value}

\item{format}{character: either "dt" for data.table, "list" or "atomic" for a
single numeric vector, whose names are corresponding selected Lorenz for
each value.  Default is "dt"}

\item{lorenz}{character or NULL. Lorenz curve selected. It could be "lq" for
Lorenz Quadratic or "lb" for Lorenz Beta}

\item{complete}{logical: If TRUE, returns a list a cumulative returns from
previously used \code{get_gd} functions. Default is \code{FALSE}}
}
\value{
list of poverty gaps. See \code{format} parameter
}
\description{
Estimate poverty gap (FGT1)
}
\examples{
pipgd_pov_gap(
welfare = pip_gd$L,
weight  = pip_gd$P,
mean = 109.90,
povline = 89,
complete = FALSE)
# Return data.table
pipgd_pov_gap(
welfare = pip_gd$L,
weight = pip_gd$P,
povline = c(.5, 1, 2, 3),
complete = FALSE)

# Return list
pipgd_pov_gap(
welfare = pip_gd$L,
weight = pip_gd$P,
povline = c(.5, 1, 2, 3),
format = "list")

# Return list complete
pipgd_pov_gap(
welfare = pip_gd$L,
weight = pip_gd$P,
povline = c(.5, 1, 2, 3),
format = "list",
complete = TRUE)

# Return data.table
pipgd_pov_gap(
welfare = pip_gd$L,
weight = pip_gd$P,
povline = c(.5, 1, 2, 3),
format = "atomic",
complete = FALSE)
}
