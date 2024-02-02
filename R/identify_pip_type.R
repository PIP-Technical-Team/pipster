#' Identify PIP type
#'
#' Based on welfare and weight vectors it identifies whether the data is
#' microdata, group data or imputed data
#'
#' @param welfare numeric: welfare variable, either income of consumption
#' @param weight numeric: expansion sample weighs. Default is a vector o 1s of
#'   the length of `welfare`
#' @param imputation_id numeric: vector that identifies different imputations.
#'   Default is NULL
#' @param groupdata_threshold numeric: threshold to discriminate between micro
#'   data an group data. Default is 200 observations
#' @param verbose logical: Whether to display important messages about your data
#'   or query
#'
#' @return character of length 1.
#' @export
#'
#' @examples
#' # Group data -------
#' # W: Weights, share of population, sum up to 100
#' # X: welfare vector with mean welfare by decile
#' # P:Cumulative share of population
#' # L: Cumulative share of welfare
#' # R: share of welfare, sum up to 1.
#'
#' W = c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64,
#' 16.99, 10, 9.78, 3.96, 1.81, 2.49)
#' X = c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75,
#' 110.64, 134.9, 167.76, 215.48, 261.66, 384.97)
#' P = c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133,
#' 0.5497, 0.7196, 0.8196, 0.9174, 0.957, 0.9751, 1)
#' L = c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808,
#' 0.23498, 0.34887, 0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)
#'
#' R = (W * X) / sum(W * X)
#'
#' ## type 1 ------
#' identify_pip_type(welfare = L, weight = P)
#' identify_pip_type(welfare = L*100, weight = P)
#'
#' ## type 2 -----------
#' identify_pip_type(welfare = R, weight = W/100)
#' identify_pip_type(welfare = R*100, weight = W)
#'
#' ## type 5 -----------
#' identify_pip_type(welfare = X, weight = W/100)
#' identify_pip_type(welfare = X, weight = W)
#'
#' ## type 3 -----------
#' identify_pip_type(welfare = X, weight  = P)
#' identify_pip_type(welfare = X, weight = P*100)
#'
#' # Microdata -------
#'
#' l <- 300
#' Y <- sample(1000, l,replace = TRUE)
#' Q <- sample(35, l,replace = TRUE)
#' I <- sample(1:5, l,replace = TRUE)
#'
#' identify_pip_type(welfare = Y, weight  = Q)
#' identify_pip_type(welfare = Y, weight  = Q, imputation_id = I)
identify_pip_type <- function(welfare,
                              weight              = rep(1, length(welfare)),
                              imputation_id       = NULL,
                              groupdata_threshold = getOption("pipster.gd_threshold"),
                              verbose             = getOption("pipster.verbose")
                              ){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  identify_pip_type_check()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sort data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.unsorted(welfare)) {
    if (verbose) {
      cli::cli_alert_warning("vectors not sorted")
    }
    o <- order(welfare)
    welfare <- welfare[o]
    weight  <- weight[o]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # identify if it is microdata or group data   --------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check if welfare and weight sum up to one --------

  welfare_to_one <- sum_up_to_one(welfare)
  weight_to_one  <- sum_up_to_one(weight)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check if it is cumulative --------

  if (welfare_to_one) {
    welfare_is_cum <- FALSE
  } else {
    welfare_is_cum <- is_cumulative(welfare)
  }

  if (weight_to_one) {
    weight_is_cum <- FALSE
  } else {
    weight_is_cum <- is_cumulative(weight)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## First identification of group data --------

  if (isTRUE(welfare_is_cum) && isTRUE(weight_is_cum)) {
    return("gd_1")
  }
  if (isTRUE(welfare_to_one) && isTRUE(weight_to_one)) {
    return("gd_2")
  }
  if (all(c(welfare_to_one, welfare_is_cum) == FALSE) && isTRUE(weight_to_one)) {
    return("gd_5")
  }
  if (all(c(welfare_to_one, welfare_is_cum) == FALSE) && isTRUE(weight_is_cum)) {
    return("gd_3")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## identification of microdata --------

  if (is.null(imputation_id)) {
    return("md")
  } else {
    n_imp <- unique(imputation_id) |>
      length()
    if (n_imp == 1) {
      return("md")
    } else {
      return("id")
    }
  }

}



identify_pip_type_check <- function() {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  l <- as.list(parent.frame())
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## errors --------

  stopifnot(exprs = {
    length(l$welfare) == length(l$weight)
    length(l$groupdata_threshold) == 1
    all(l$weight >= 0)
  })

  if (!is.null(l$imputation_id)) {
    stopifnot(exprs = {
      length(l$welfare) == length(l$imputation_id)
    })
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## add warnings for NA values --------
  # if (any(is.na(l$welfare))) {
  #   ww_n <- which(is.na(l$welfare))
  #   lw_n <- length(ww_n)
  #   cli::cli_warn("{.var welfare} has {lw_n} NA value{?s}")
  # }
  if (any(na_omit(l$welfare) < 0)) {
    ww_n <- which(l$welfare < 0)
    lw_n <- length(ww_n)
    cli::cli_warn("{.var welfare} has {lw_n} negative value{?s}")
  }

  if (is.character(l$imputation_id)) {
    cli::cli_warn("{.var imputation_id} must be numeric in PIP format.")
  }

  return(invisible(TRUE))

}


#' check if elements of vector sum up to 1 or (100)
#'
#' This functions uses a heuristic to round up the sum to the closest integers,
#' as it is possible to have vectors that are intended to sum up to 1, but for
#' they fail because of precision.
#'
#' @param x numeric: vector whose elements might sum up to 1 or 100
#' @param digits numeric: threshold of the sum. Default is 0 decimals
#'
#' @return logical
#' @keywords internal
sum_up_to_one <- function(x, digits = 0) {
  sumx <-
    sum(x) |>
    round(digits)

  if (sumx == 100) {
    return(TRUE)
  } else if (sumx == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}

#' Check whether the vector is cumulative up to 1
#'
#' @param x numeric: vector whose elements might be cumulative up to 1
#' @param digits numeric: number of digits for cumulative sum threshold. Default
#'   is 8 decimals
#'
#' @return logical
#' @keywords internal
is_cumulative <- function(x, digits = 8) {

  share_x <- c(x[1], diff(x))
  ret     <- sum_up_to_one(share_x,
                           digits = digits)

  ret
}

# # Check that share of income is always increasing
# norm_wel <- diff(share_wel / share_pop) # normalize welfare by population
# assertthat::assert_that(all(norm_wel >= 0),
#                         msg = paste0(
#                           "share of `welfare` must increase with each\n",
#                           "subsequent bin relative to its corresponging\n",
#                           "population. Make sure data is sorted correctly."
#                         )
# )
