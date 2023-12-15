#' pipster: Poverty and Inequality methodology of WB methodology
#'
#' A higher-level package to estimate Socioeconomic indicators on poverty and
#' inequality using the methodology by the WorlD Bank. This packages is mainly a
#' wrapper of the lower-level package `wbpip`
#'
#' @section pipster functions: The pipster functions are divided in X groups.
#'   The first, and most important is a set of functions to estimate especif
#'   indicators using as input the welfare and weights vector of microdata or
#'   group data.
#'
#' @docType package
#' @name pipster
#' @import collapse
#' @import wbpip


# Make sure data.table knows we know we're using it
# .datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":="
    ),
    package = utils::packageName()
  )
}



NULL
