% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_pip_type.R
\name{is_cumulative}
\alias{is_cumulative}
\title{Check whether the vector is cumulative up to 1}
\usage{
is_cumulative(x, digits = 8)
}
\arguments{
\item{x}{numeric: vector whose elements might be cumulative up to 1}

\item{digits}{numeric: number of digits for cumulative sum threshold. Default
is 8 decimals}
}
\value{
logical
}
\description{
Check whether the vector is cumulative up to 1
}
\keyword{internal}
