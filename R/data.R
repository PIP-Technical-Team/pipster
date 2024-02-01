#' pip_gd
#'
#' PIP Group Data example.
#'
#' @format A grouped data frame with five variables:
#' \describe{
#' \item{\code{W}}{Weights, share of population, sum up to 100}
#' \item{\code{X}}{welfare vector with mean welfare by group}
#' \item{\code{P}}{Cumulative share of population}
#' \item{\code{L}}{Cumulative share of welfare}
#' \item{\code{R}}{share of welfare, sum up to 1}
#' }
#' @source Datt, Gaurav. 1998. “Computational Tools for Poverty Measurement and Analysis.” <http://ebrary.ifpri.org/utils/getfile/collection/p15738coll2/id/125673/filename/125704.pdf>, downloaded 2023-07-18
#'
#' For further details, see \url{https://datanalytics.worldbank.org/PIP-Methodology/welfareaggregate.html#tgd}
#'
"pip_gd"



#' pip_md
#'
#' PIP microdata example.
#'
#' @format A microdata frame with two variables:
#' \describe{
#' \item{\code{welfare}}{welfare (income or consumption)}
#' \item{\code{weight}}{population weights}
#' }
#'
"pip_md"





#' pip_md_s
#'
#' Small PIP microdata example.
#'
#' @format A 100 obs microdata frame with two variables:
#' \describe{
#' \item{\code{welfare}}{welfare (income or consumption)}
#' \item{\code{weight}}{population weights}
#' }
#'
"pip_md_s"




#' pip_id
#'
#' PIP imputed data example.
#'
#' @format An imputed data frame with three variables:
#' \describe{
#' \item{\code{welfare}}{welfare (income or consumption)}
#' \item{\code{weight}}{population weights}
#' \item{\code{imputation_id}}{Imputation ID}
#' }
#'
"pip_id"




