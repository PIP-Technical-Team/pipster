#' Identify PIP type
#'
#' Based on welfare and weight vectors it identifies whether the data is
#' microdata, group data or imputed data
#'
#' @param welfare numeric: welfare variable, either income of consumption
#' @param weight numeric: expansion sample weiths. Default is a vector o 1s of
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
identify_pip_type <- function(welfare,
                              weight              = rep(1, length(welfare)),
                              imputation_id       = NULL,
                              groupdata_threshold = 200,
                              verbose             = getOption("pipster.verbose")
                              ){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## errors --------

  stopifnot(exprs = {
    length(welfare) == length(weight)
    length(groupdata_threshold) == 1
  })

  if (!is.null(imputation_id)) {
    stopifnot(exprs = {
      length(welfare) == length(imputation_id)
    })
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## add warnings for NA values --------


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
  sum_up_to_one(share_x, digits = digits)

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
