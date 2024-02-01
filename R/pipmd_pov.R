# This file contains all the functions related to absolute poverty measures
# on microdata (md).



#' Calculate poverty headcount using microdata
#'
#' Non-vectorized poverty headcount microdata function. Use the vectorized
#' function [pipmd_pov_headcount]
#'
#' @param welfare numeric: A vector of income or consumption values
#' @param weight 	numeric: A vector of population weights. If NULL, a vector of 1s
#' is used to give equal weight to each observation.
#' @param povline numeric: Poverty line in international dollars, same units as welfare.
#' @param times_mean numeric factor that multiplies the mean to create a relative poverty line. Default is 1
#'
#' @return numeric: Poverty headcount ratio
#' @keywords internal
pipmd_pov_headcount_nv <- function(
  welfare    ,
  weight     = rep(1, length = length(welfare)),
  povline    = fmean(welfare, w = weight)*times_mean,
  times_mean = 1
){


  #   Defenses -------------
  check_pipmd_params()

  # Computations ------------
  output <- list()
  hc <- wbpip::md_compute_fgt(
    welfare      = welfare,
    weight       = weight,
    povline      = povline
  )
  attributes(hc) <- NULL
  output$pov_headcount <- hc

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  return(output)

}


#' Calculate poverty headcount from microdata
#'
#' @inheritParams pipmd_pov_headcount_nv
#' @param format atomic character vector: specifies the format of output, either
#'   "dt", "list", or "atomic"
#'
#' @return A `data.table` and `data.frame` object of length equal to the povline
#'   vector with variables `povline` and `pov_headcount`. See `format` to change
#'   the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic poverty headcount calculation
#' pipmd_pov_headcount(welfare = pip_md_s$welfare,
#'                     weight  = pip_md_s$weight,
#'                     povline = 1.3,
#'                     format  = "list")
#'
#' # Example 2: Returning data.table format, multiple povline.
#' pipmd_pov_headcount(welfare = pip_md_s$welfare,
#'                     weight  = pip_md_s$weight,
#'                     povline = c(1.3, 1.2),
#'                     format  = "dt")
#'
#' # Example 3: Returning atomic format
#' pipmd_pov_headcount(welfare = pip_md_s$welfare,
#'                     weight  = pip_md_s$weight,
#'                     povline = 1.3,
#'                     format  = "atomic")
#'
pipmd_pov_headcount <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1,
    format     = c("dt", "list", "atomic")
){

  # ______________________________________________________________
  # Arguments ----------------------------------------------------
  format <- match.arg(format)

  # ______________________________________________________________
  # Computations -------------------------------------------------
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
  out

}









# POV GAP-----------------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Calculate poverty gap using microdata
#'
#' Non-vectorized poverty gap microdata function. Use the vectorized
#' function [pipmd_pov_gap]
#'
#' @inheritParams pipmd_pov_headcount
#'
#' @return numeric: Poverty gap
#' @keywords internal
pipmd_pov_gap_nv <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1)
  {
  #   Defenses -------------
  check_pipmd_params()

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- list()
  pg <- wbpip::md_compute_fgt(
    welfare      = welfare,
    weight       = weight,
    povline      = povline,
    alpha        = 1
  )
  attributes(pg) <- NULL
  output$pov_gap <- pg

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  output

}


#' Calculate poverty gap from microdata
#'
#' @inheritParams pipmd_pov_gap_nv
#' @param format atomic character vector: specifies the format of output, either
#' "dt", "list", or "atomic"
#'
#' @return A `data.table` and `data.frame` object of length equal to the povline
#' vector with variables `povline` and `pov_gap`.
#' See `format` to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic poverty gap calculation
#' pipmd_pov_gap(welfare = pip_md_s$welfare,
#'               weight  = pip_md_s$weight,
#'               povline = 1.3,
#'               format  = "list")
#'
#' # Example 2: Returning data.table format, multiple povline.
#' pipmd_pov_gap(welfare = pip_md_s$welfare,
#'               weight  = pip_md_s$weight,
#'               povline = c(1.3, 1.2),
#'               format  = "dt")
#'
#' # Example 3: Returning atomic format
#' pipmd_pov_gap(welfare = pip_md_s$welfare,
#'               weight  = pip_md_s$weight,
#'               povline = 1.3,
#'               format  = "atomic")
#'
pipmd_pov_gap <- function(
    welfare    = NULL,
    weight     = NULL,
    povline    = NULL,
    mean       = 1,
    times_mean = 1,
    format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_pov_gap_v <- Vectorize(
    FUN            = pipmd_pov_gap_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_pov_gap <- pipmd_pov_gap_v(
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md(
    ld      = list_pov_gap,
    var     = "pov_gap",
    format  = format,
    povline = povline
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}







# POV SEVERITY -----------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Calculate poverty severity using microdata
#'
#' Non-vectorized poverty severity microdata function. Use the vectorized
#' function [pipmd_pov_severity]
#'
#' @inheritParams pipmd_pov_headcount
#'
#' @return numeric: Poverty severity
#' @keywords internal
pipmd_pov_severity_nv <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1)
  {
  #   Defenses -------------
  check_pipmd_params()

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- list()
  ps <- wbpip::md_compute_fgt(
    welfare      = welfare,
    weight       = weight,
    povline      = povline,
    alpha        = 2
  )
  attributes(ps) <- NULL
  output$pov_severity <- ps

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  output

}


#' Calculate poverty severity from microdata
#'
#' @inheritParams pipmd_pov_severity_nv
#' @param format atomic character vector: specifies the format of output, either
#' "dt", "list", or "atomic"
#'
#' @return A `data.table` and `data.frame` object of length equal to the povline
#' vector with variables `povline` and `pov_severity`.
#' See `format` to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic poverty headcount calculation
#' pipmd_pov_severity(welfare = pip_md_s$welfare,
#'                    weight  = pip_md_s$weight,
#'                    povline = 1.3,
#'                    format  = "list")
#'
#' # Example 2: Returning data.table format and multiple povline
#' pipmd_pov_severity(welfare = pip_md_s$welfare,
#'                    weight  = pip_md_s$weight,
#'                    povline = c(1.3, 1.2),
#'                    format  = "dt")
#'
#' # Example 3: Returning atomic format
#' pipmd_pov_severity(welfare = pip_md_s$welfare,
#'                    weight  = pip_md_s$weight,
#'                    povline = 1.3,
#'                    format  = "atomic")
#'
pipmd_pov_severity <- function(
    welfare    = NULL,
    weight     = NULL,
    povline    = NULL,
    mean       = 1,
    times_mean = 1,
    format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_pov_severity_v <- Vectorize(
    FUN            = pipmd_pov_severity_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_pov_severity <- pipmd_pov_severity_v(
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md(
    ld      = list_pov_severity,
    var     = "pov_severity",
    format  = format,
    povline = povline
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}




# WATTS -----------------------------------------------------------------
#-------------------------------------------------------------------------------


#' Calculate Watts index using microdata
#'
#' Non-vectorized Watts index microdata function. Use the vectorized
#' function [pipmd_watts]
#'
#' @inheritParams pipmd_pov_headcount
#'
#' @return numeric: Watts index
#' @keywords internal
pipmd_watts_nv <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1
){
  #   Defenses -------------
  check_pipmd_params()

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- list()
  wi <- wbpip::md_compute_watts(
    welfare      = welfare,
    weight       = weight,
    povline      = povline
  )
  attributes(wi) <- NULL
  output$watts <- wi

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  output

}


#' Calculate Watts index from microdata
#'
#' @inheritParams pipmd_watts_nv
#' @param format atomic character vector: specifies the format of output, either
#' "dt", "list", or "atomic"
#'
#' @return A `data.table` and `data.frame` object of length equal to the povline
#' vector with variables `povline` and `pov_severity`.
#' See `format` to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic poverty headcount calculation
#' pipmd_watts(welfare = pip_md_s$welfare,
#'             weight  = pip_md_s$weight,
#'             povline = 1.3,
#'             format  = "list")
#'
#' # Example 2: Returning data.table format
#' pipmd_watts(welfare = pip_md_s$welfare,
#'             weight  = pip_md_s$weight,
#'             povline = c(1.3, 1.2),
#'             format  = "dt")
#'
#' # Example 3: Returning atomic format
#' pipmd_watts(welfare = pip_md_s$welfare,
#'             weight  = pip_md_s$weight,
#'             povline = 1.3,
#'             format  = "atomic")
#'
pipmd_watts <- function(
    welfare    = NULL,
    weight     = NULL,
    povline    = NULL,
    mean       = 1,
    times_mean = 1,
    format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_watts_v <- Vectorize(
    FUN            = pipmd_watts_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_watts <- pipmd_watts_v(
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md(
    ld      = list_watts,
    var     = "watts",
    format  = format,
    povline = povline
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}




