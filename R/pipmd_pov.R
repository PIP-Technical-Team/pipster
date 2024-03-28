# This file contains all the functions related to absolute poverty measures
# on microdata (md).



#' Calculate poverty headcount using microdata
#'
#' Non-vectorized poverty headcount microdata function. Use the vectorized
#' function [pipmd_pov_headcount]
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @param welfare numeric: A vector of income or consumption values
#' @param weight 	numeric: A vector of population weights. If NULL, a vector of 1s
#' is used to give equal weight to each observation.
#' @param povline numeric: Poverty line in international dollars, same units as welfare.
#' @param times_mean numeric factor that multiplies the mean to create a relative poverty line. Default is 1
#'
#' @return numeric: Poverty headcount ratio
#' @keywords internal
pipmd_pov_headcount_nv <- function(
  pipster_object = NULL,
  welfare        = NULL,
  weight         = rep(1, length = length(welfare)),
  povline        = fmean(welfare, w = weight)*times_mean,
  times_mean     = NULL #1
){

  # ----------------------------------------------------------------------------
  # Arguments ------------------------------------------------------------------

  pl <- as.list(environment())
  po <- is_valid_inputs_md(pl)


  # Adjust povline and times_mean giving pipster_object precedent
  if (!po & is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = pipster_object$welfare |> unclass(),
                                            weight  = pipster_object$weight |> unclass(),
                                            povline = povline)
  }

  if (!po & !is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = welfare,
                                            weight  = weight,
                                            povline = povline)
  }

  check_pipmd_pov(pl)

  # ----------------------------------------------------------------------------
  # Computations ---------------------------------------------------------------

  fgt_data <- wbpip::md_compute_fgt(
    welfare      = pipster_object$welfare |> unclass(),
    weight       = pipster_object$weight |> unclass(),
    povline      = pipster_object$args$povline,
    alpha = 0,
    return_data = TRUE # this is to carry results to use in next calculations
  )


  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------

  results <- pipster_object$results
  results$pov_stats$fgt_data <- fgt_data
  results$pov_stats$pov_headcount <- fgt_data$FGT0
  results$pov_stats$povline <- fgt_data$povline


  pipster_object$results <- results

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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1,
    format     = c("dt", "list", "atomic"),
    complete = getOption('pipster.return_complete')
){



  # ______________________________________________________________
  # Arguments ----------------------------------------------------
  format <- match.arg(format)

  ## povline check: user-defined povline has priority
  if (is.null(povline) && !is.null(pipster_object$args$povline)) {
    povline <- pipster_object$args$povline
  }

  # ______________________________________________________________
  # Computations -------------------------------------------------
  pipmd_pov_headcount_v <- Vectorize(
    FUN            = pipmd_pov_headcount_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )


  list_headcount <- pipmd_pov_headcount_v(
    pipster_object = pipster_object,
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md_pov(
    ld      = list_headcount,
    var     = "pov_headcount",
    format  = format,
    povline = povline,
    complete = complete
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
    pipster_object = NULL,
    welfare        = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = NULL)
  {

  # ----------------------------------------------------------------------------
  # Arguments ------------------------------------------------------------------

  pl <- as.list(environment())
  po <- is_valid_inputs_md(pl)


  # Adjust povline and times_mean giving pipster_object precedent
  if (!po & is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = pipster_object$welfare |> unclass(),
                                            weight  = pipster_object$weight |> unclass(),
                                            povline = povline)
  }

  if (!po & !is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = welfare,
                                            weight  = weight,
                                            povline = povline)
  }

  check_pipmd_pov(pl)

  # ----------------------------------------------------------------------------
  # Computations ---------------------------------------------------------------

  fgt_data <- wbpip::md_compute_fgt(
    welfare      = pipster_object$welfare |> unclass(),
    weight       = pipster_object$weight |> unclass(),
    povline      = pipster_object$args$povline,
    alpha        = 1,
    return_data  = TRUE # this is to carry results to use in next calculations
  )


  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------

  results <- pipster_object$results
  results$pov_stats$fgt_data <- fgt_data
  results$pov_stats$pov_gap <- fgt_data$FGT1
  results$pov_stats$povline <- fgt_data$povline


  pipster_object$results <- results

}


#' Calculate poverty gap from microdata
#'
#' @param pipster_object pipster object created using [create_pipster_object]
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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1,
    format     = c("dt", "list", "atomic"),
    complete = getOption('pipster.return_complete')
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  ## povline check: user-defined povline has priority
  if (is.null(povline) && !is.null(pipster_object$args$povline)) {
    povline <- pipster_object$args$povline
  }

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_pov_gap_v <- Vectorize(
    FUN            = pipmd_pov_gap_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )

  list_pov_gap <- pipmd_pov_gap_v(
    pipster_object = pipster_object,
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md_pov(
    ld      = list_pov_gap,
    var     = "pov_gap",
    format  = format,
    povline = povline,
    complete = complete
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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = NULL)
  {

  # ----------------------------------------------------------------------------
  # Arguments ------------------------------------------------------------------

  pl <- as.list(environment())
  po <- is_valid_inputs_md(pl)

  # Adjust povline and times_mean giving pipster_object precedent
  if (!po & is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = pipster_object$welfare |> unclass(),
                                            weight  = pipster_object$weight |> unclass(),
                                            povline = povline)
  }

  if (!po & !is.null(welfare)) {
    pipster_object <- create_pipster_object(welfare = welfare,
                                            weight  = weight,
                                            povline = povline)
  }

  check_pipmd_pov(pl)

  # ----------------------------------------------------------------------------
  # Computations ---------------------------------------------------------------

  fgt_data <- wbpip::md_compute_fgt(
    welfare      = pipster_object$welfare |> unclass(),
    weight       = pipster_object$weight |> unclass(),
    povline      = pipster_object$args$povline,
    alpha        = 2,
    return_data  = TRUE # this is to carry results to use in next calculations
  )


  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------

  results <- pipster_object$results
  results$pov_stats$fgt_data <- fgt_data
  results$pov_stats$pov_severity <- fgt_data$FGT2
  results$pov_stats$povline <- fgt_data$povline


  pipster_object$results <- results

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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1,
    format     = c("dt", "list", "atomic"),
    complete = getOption('pipster.return_complete')
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  ## povline check: user-defined povline has priority
  if (is.null(povline) && !is.null(pipster_object$args$povline)) {
    povline <- pipster_object$args$povline
  }

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipmd_pov_severity_v <- Vectorize(
    FUN            = pipmd_pov_severity_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )

  list_pov_severity <- pipmd_pov_severity_v(
    pipster_object = pipster_object,
    welfare    = welfare,
    weight     = weight,
    povline    = povline
  )


  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format_md_pov(
    ld      = list_pov_severity,
    var     = "pov_severity",
    format  = format,
    povline = povline,
    complete = complete
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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
    times_mean = 1
){
  #   Defenses -------------
  if (!is.null(pipster_object)) {
    welfare <- pipster_object$welfare |> unclass()
    weight  <- pipster_object$weight |> unclass()
  }
  check_pipmd_pov()

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
    pipster_object = NULL,
    welfare    = NULL,
    weight     = rep(1, length = length(welfare)),
    povline    = fmean(welfare, w = weight)*times_mean,
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
    pipster_object = pipster_object,
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




