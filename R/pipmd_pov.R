# This file contains all the functions related to absolute poverty measures
#  on microdata (md)



#' Calculate poverty headcount using microdata
#'
#' Non-vectorized poverty headcount microdata function. Use the vectorized
#' function [pipmd_pov_headcount]
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param weight 	numeric: A vector of population weights. If NULL, a vector of 1s
#' is used to give equal weight to each observation.
#' @param povline numeric: Poverty line in international dollars, same units as welfare.
#' @param mean numeric scalar of distribution mean. Default is
#' @param times_mean numeric factor that multiplies the mean to create a relative poverty line. Default is 1
#'
#' @return numeric: Poverty headcount ratio
#' @keywords internal
pipmd_pov_headcount_nv <- function(
  welfare    = NULL,
  weight     = NULL,
  povline    = ifelse(!is.na(mean*times_mean),
    mean*times_mean,
    NULL
  ),
  mean       = 1,
  times_mean = 1
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NULL")
  }
  if (is.null(weight)) {
    weight <- rep(1, length = length(welfare))
    cli::cli_alert_warning(
      text = "No weight vector specified, each observation assigned equal weight"
    )
  }
  if (is.null(povline) && is.numeric(povline)) {
    cli::cli_abort(
      text = "A numeric poverty line must be specified"
    )
  } else if (povline < min(welfare) && povline > max(welfare)) {
    cli::cli_alert_info(
      text = "Note: specified poverty line is not within the welfare range"
    )
  }

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- list()
  hc <- wbpip:::md_compute_poverty_stats(
    welfare      = welfare,
    weight       = weight,
    povline_lcu = povline
  )
  output$pov_headcount <- hc$headcount

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  return(output)

}


#' Calculate poverty headcount from microdata
#'
#' @inheritParams pipmd_pov_headcount_nv
#' @param format atomic character vector: specifies the format of output, either
#' "dt", "list", or "atomic"
#'
#' @return list: contains numeric poverty headcount. See `format`
#' @export
#'
#' @examples
pipmd_pov_headcount <- function(
    welfare = NULL,
    weight  = NULL,
    povline = NULL,
    format  = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_pov_headcount_v <- Vectorize(
    FUN            = pipmd_pov_headcount_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_headcount <- pipmd_pov_headcount_v(
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md(
    ld      = list_headcount,
    var     = "pov_headcount",
    format  = format,
    povline = povline
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  return(out)


}


