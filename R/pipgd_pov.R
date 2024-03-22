# This file contains all the functions related to absolute poverty measures
#  on grouped data (gd)


#' Estimate poverty headcount (FGT0)
#'
#' This function is not vectorized and thus is not exported. Use
#' [pipgd_pov_headcount] instead.
#'
#' @inheritParams pipgd_select_lorenz
#' @inheritParams pipgd_welfare_share_at
#'
#' @return numeric poverty headcount
#' @keywords internal
pipgd_pov_headcount_nv <-
  function(pipster_object = NULL,
           welfare        = NULL,
           weight         = NULL,
           mean           = NULL,
           times_mean     = NULL,
           povshare       = NULL,
           povline        = NULL,
           lorenz         = NULL,
           complete       = getOption("pipster.return_complete")) {

  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)
  po <- is_valid_inputs_pov(pl)

  # Preserve the original lorenz if pipster_object exists and lorenz is not provided
  original_lorenz <- if (is.null(lorenz) && !is.null(pipster_object) && !is.null(pipster_object$args$lorenz)) {
    pipster_object$args$lorenz
  } else {
    lorenz
  }

  # __________________________________________________________________________
  # params--------------------------------------------------------------------
  if (po) {
    params <- pipster_object$params
  } else {
    pipster_object <- validate_params(pipster_object = pipster_object,
                                      welfare        = welfare,
                                      weight         = weight,
                                      mean           = mean,
                                      times_mean     = times_mean,
                                      povshare       = povshare,
                                      lorenz         = original_lorenz,
                                      povline        = povline)
    params <- pipster_object$params
  }

  # Lorenz----------------------------------------------------------------------
  #_____________________________________________________________________________

  lorenz <- choose_lorenz_for_pov(pipster_object,
                                  params,
                                  lorenz)

  # Headcount ------------------------------------------------------------------
  # ____________________________________________________________________________

  headcount <- params$gd_params[[lorenz]]$validity$headcount

  # Return----------------------------------------------------------------------
  #_____________________________________________________________________________
  results <- list()
  results$pov_stats$headcount <- headcount
  results$pov_stats$lorenz    <- lorenz

  if (isFALSE(complete)) {
    return(results)
  }

  pipster_object$results <- results

  pipster_object
}


#' Estimate poverty headcount (FGT0)
#'
#' @inheritParams pipgd_pov_headcount_nv
#' @inheritParams return_format
#'
#' @return Returns a `data.table` and `data.frame` object with three variables:
#' `povline`, `headcount`, and `lorenz`.  Check `format` argument to change
#' the output format.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with specified mean and poverty line
#' pipgd_pov_headcount(welfare = pip_gd$L,
#'                     weight  = pip_gd$P,
#'                     mean    = 109.90,
#'                     povline = 89,
#'                     complete = FALSE)
#'
#' # Example 2: Multiple poverty lines, returning data.table
#' pipgd_pov_headcount(welfare = pip_gd$L,
#'                     weight  = pip_gd$P,
#'                     povline = c(0.5, 1, 2, 3),
#'                     complete = FALSE)
#'
#' # Example 3: Multiple poverty lines, returning list format
#' pipgd_pov_headcount(welfare = pip_gd$L,
#'                     weight  = pip_gd$P,
#'                     povline = c(0.5, 1, 2, 3),
#'                     format  = "list")
#'
#' # Example 4: Multiple poverty lines, returning detailed list format
#' pipgd_pov_headcount(welfare = pip_gd$L,
#'                     weight  = pip_gd$P,
#'                     povline = c(0.5, 1, 2, 3),
#'                     format  = "list",
#'                     complete = TRUE)
#'
#' # Example 5: Multiple poverty lines, returning atomic format
#' pipgd_pov_headcount(welfare = pip_gd$L,
#'                     weight  = pip_gd$P,
#'                     povline = c(0.5, 1, 2, 3),
#'                     format  = "atomic",
#'                     complete = FALSE)
#'
pipgd_pov_headcount <-
  function(pipster_object = NULL,
           welfare        = NULL,
           weight         = NULL,
           mean           = 1,
           times_mean     = 1,
           povshare       = NULL,
           povline        = ifelse(is.null(povshare),
                               mean*times_mean,
                               NA_real_),
           format         = c("dt", "list", "atomic"),
           lorenz         = NULL,
           complete       = getOption("pipster.return_complete")) {

    format <- match.arg(format)

    # Vectorize-----------------------------------------------------------------
    #___________________________________________________________________________
    pipgd_pov_headcount_v <- Vectorize(pipgd_pov_headcount_nv,
                                       vectorize.args = "povline",
                                       SIMPLIFY       = FALSE)


    ld <- pipgd_pov_headcount_v(pipster_object = pipster_object,
                                welfare        = welfare,
                                weight         = weight,
                                povline        = povline,
                                povshare       = povshare,
                                complete       = complete,
                                lorenz         = lorenz,
                                mean           = mean,
                                times_mean     = times_mean)

    # Return--------------------------------------------------------------------
    #___________________________________________________________________________
    out <- return_format(ld,
                         var      = "headcount",
                         povline  = povline,
                         complete = complete,
                         format   = format)
    out
  }

#' Estimate poverty gap (FGT1)
#'
#' This function is not vectorized. Use [pipgd_pov_gap] instead
#'
#' @inheritParams pipgd_pov_headcount
#'
#' @return numeric poverty gap value
#' @keywords internal
pipgd_pov_gap_nv <- function(pipster_object = NULL,
                             welfare        = NULL,
                             weight         = NULL,
                             mean           = 1,
                             times_mean     = 1,
                             povshare       = NULL,
                             povline        = ifelse(is.null(povshare),
                                                 mean*times_mean,
                                                 NA_real_),
                             lorenz         = NULL,
                             complete       = getOption("pipster.return_complete")
                             ){
  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  po <- is_valid_inputs_pov(pl)

  # __________________________________________________________________________
  # Params--------------------------------------------------------------------
  if (po &
      (!is.null(pipster_object$results$pov_stats$headcount))) {
    params  <- pipster_object$params
    results <- pipster_object$results
  } else {
    pipster_object <- pipgd_pov_headcount_nv(pipster_object = pipster_object,
                                             welfare        = welfare,
                                             weight         = weight,
                                             mean           = mean,
                                             times_mean     = times_mean,
                                             povshare       = povshare,
                                             povline        = povline,
                                             lorenz         = lorenz,
                                             complete       = TRUE)
    params  <- pipster_object$params
    results <- pipster_object$results
  }

  check_pipgd_params(pl)

  # Lorenz----------------------------------------------------------------------
  #_____________________________________________________________________________

  lorenz <- choose_lorenz_for_pov(pipster_object,
                                  params,
                                  lorenz)

  # povline---------------------------------------------------------------------
  #_____________________________________________________________________________
  if (!is.null(povshare) & (is.null(povline) || is.na(povline))) {
    derive_ <-
      paste0("wbpip::derive_", lorenz) |>
      parse(text = _)

    povline <-
      mean * eval(derive_)(povshare,
                           params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["C"]])


  }

  # wbpip-----------------------------------------------------------------------
  #_____________________________________________________________________________
  pov_gap_ <-
    paste0("wbpip::gd_compute_pov_gap_", lorenz) |>
    parse(text = _)

  pov_gap <-
    eval(pov_gap_)(mean      = mean,
                   povline   = povline,
                   headcount = pipster_object$results$pov_stats$headcount,
                   A         = params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                   B         = params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                   C         = params$gd_params[[lorenz]]$reg_results$coef[["C"]])
  attributes(pov_gap) <- NULL

  # Return----------------------------------------------------------------------
  #_____________________________________________________________________________
  if (isFALSE(complete)) {
    results <- list()
    results$pov_stats$pov_gap <- pov_gap
    results$pov_stats$lorenz  <- lorenz
    return(results)
  }
  results$pov_stats$pov_gap <- pov_gap
  results$pov_stats$lorenz  <- lorenz
  pipster_object$results    <- results

  pipster_object
}

#' Estimate poverty gap (FGT1)
#'
#' @inheritParams pipgd_pov_headcount
#'
#' @return Returns a `data.table` and `data.frame` object with three variables:
#' `povline`, `pov_gap`, and `lorenz`.  Check `format` argument to change
#' the output format.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with specified mean and poverty line
#' pipgd_pov_gap(welfare = pip_gd$L,
#'               weight  = pip_gd$P,
#'               mean    = 109.90,
#'               povline = 89,
#'               complete = FALSE)
#'
#' # Example 2: Multiple poverty lines, returning data.table
#' pipgd_pov_gap(welfare = pip_gd$L,
#'               weight  = pip_gd$P,
#'               povline = c(0.5, 1, 2, 3),
#'               complete = FALSE)
#'
#' # Example 3: Multiple poverty lines, returning list format
#' pipgd_pov_gap(welfare = pip_gd$L,
#'               weight  = pip_gd$P,
#'               povline = c(0.5, 1, 2, 3),
#'               format  = "list")
#'
#' # Example 4: Multiple poverty lines, returning detailed list format
#' pipgd_pov_gap(welfare = pip_gd$L,
#'               weight  = pip_gd$P,
#'               povline = c(0.5, 1, 2, 3),
#'               format  = "list",
#'               complete = TRUE)
#'
#' # Example 5: Multiple poverty lines, returning atomic format
#' pipgd_pov_gap(welfare = pip_gd$L,
#'               weight  = pip_gd$P,
#'               povline = c(0.5, 1, 2, 3),
#'               format  = "atomic",
#'               complete = FALSE)
#'
pipgd_pov_gap <- function(pipster_object = NULL,
                          welfare        = NULL,
                          weight         = NULL,
                          mean           = 1,
                          times_mean     = 1,
                          povshare       = NULL,
                          povline        = ifelse(is.null(povshare),
                                              mean*times_mean,
                                              NA_real_),
                          format         = c("dt", "list", "atomic"),
                          lorenz         = NULL,
                          complete       = getOption("pipster.return_complete")) {

  format <- match.arg(format)

  #   ____________________________________________________
  #   Computations                                     ####
  pipgd_pov_gap_v <- Vectorize(pipgd_pov_gap_nv,
                               vectorize.args = "povline",
                               SIMPLIFY       = FALSE)

  ld <- pipgd_pov_gap_v(pipster_object = pipster_object,
                        welfare        = welfare,
                        weight         = weight,
                        povshare       = povshare,
                        povline        = povline,
                        complete       = complete,
                        lorenz         = lorenz,
                        mean           = mean,
                        times_mean     = times_mean)

  #   ____________________________________________________
  #   Return                                           ####
  out <- return_format(ld,
                       var      = "pov_gap",
                       povline  = povline,
                       complete = complete,
                       format   = format)
  out

}


#' Estimate poverty severity (non-vectorized)
#'
#' This function is not vectorized and thus is not exported. Use
#' [pipgd_pov_severity] instead.
#'
#' @inheritParams pipgd_pov_gap_nv
#' @param pov_gap list: When NULL (default), the welfare and weight
#' arguments are used to estimate all underlying parameters.
#' Else, should be a list of output from [pipgd_pov_gap_nv].
#'
#' @return list: contains numeric poverty severity and, if `complete=TRUE`,
#' also returns all params.
#' @keywords internal
pipgd_pov_severity_nv <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = 1,
    times_mean     = 1,
    povshare       = NULL,
    povline        = ifelse(is.null(povshare),
                            mean*times_mean,
                            NA_real_),
    lorenz         = NULL,
    complete       = getOption("pipster.return_complete")
  ){

  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  po <- is_valid_inputs_pov(pl)

  # __________________________________________________________________________
  # Params--------------------------------------------------------------------
  if (po &
      (!is.null(pipster_object$results$pov_stats$pov_gap))) {
    params  <- pipster_object$params
    results <- pipster_object$results
  } else {
    pipster_object <- pipgd_pov_gap_nv(pipster_object = pipster_object,
                               welfare        = welfare,
                               weight         = weight,
                               mean           = mean,
                               times_mean     = times_mean,
                               povshare       = povshare,
                               povline        = povline,
                               lorenz         = lorenz,
                               complete       = TRUE)
    params  <- pipster_object$params
    results <- pipster_object$results
  }

  check_pipgd_params(pl)

  # Lorenz----------------------------------------------------------------------
  #_____________________________________________________________________________

  lorenz <- choose_lorenz_for_pov(pipster_object,
                                  params,
                                  lorenz)

  # povline-------------------------------------------------------------------
  #___________________________________________________________________________
  if (!is.null(povshare) & (is.null(povline) || is.na(povline))) {
    derive_ <-
      paste0("wbpip::derive_", lorenz) |>
      parse(text = _)

    povline <-
      mean * eval(derive_)(povshare,
                           params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["C"]])
    }

  # __________________________________________________________________________
  # Calculate Severity -------------------------------------------------------
  if (lorenz == "lb") {
    pov_severity <-
      wbpip::gd_compute_pov_severity_lb(
          mean      = mean,
          headcount = pipster_object$results$pov_stats$headcount,
          pov_gap   = pipster_object$results$pov_stats$pov_gap,
          povline   = povline,
          A         = params$gd_params$lb$reg_results$coef[["A"]],
          B         = params$gd_params$lb$reg_results$coef[["B"]],
          C         = params$gd_params$lb$reg_results$coef[["C"]]
        )
  } else if (lorenz == "lq") {
    pov_severity <-
      wbpip::gd_compute_pov_severity_lq(
          mean      = mean,
          povline   = povline,
          headcount = pipster_object$results$pov_stats$headcount,
          pov_gap   = pipster_object$results$pov_stats$pov_gap,
          A         = params$gd_params$lq$reg_results$coef[["A"]],
          B         = params$gd_params$lq$reg_results$coef[["B"]],
          C         = params$gd_params$lq$reg_results$coef[["C"]],
          e         = params$gd_params$lq$key_values$e,
          m         = params$gd_params$lq$key_values$m,
          n         = params$gd_params$lq$key_values$n,
          r         = params$gd_params$lq$key_values$r,
          s1        = params$gd_params$lq$key_values$s1,
          s2        = params$gd_params$lq$key_values$s2
        )
  }

  attributes(pov_severity) <- NULL

  # __________________________________________________________________________
  # Return--------------------------------------------------------------------
  if (isFALSE(complete)) {
    results <- list()
    results$pov_stats$pov_severity <- pov_severity
    results$pov_stats$lorenz       <- lorenz
    return(results)
  }
  results$pov_stats$pov_severity <- pov_severity
  results$pov_stats$lorenz       <- lorenz
  pipster_object$results         <- results

  pipster_object

}


#' Estimate poverty severity
#'
#' @inheritParams pipgd_pov_gap_nv
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#' single numeric vector, whose names are corresponding selected Lorenz for
#' each value.  Default is "dt"
#' @inheritParams pipgd_pov_severity_nv
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `pov_severity` and `lorenz`.  Check `format` argument to change
#' the output format.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with specified mean and poverty line
#' pipgd_pov_severity(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    mean    = 109.90,
#'                    povline = 89,
#'                    complete = FALSE)
#'
#' # Example 2: Multiple poverty lines, returning data.table
#' pipgd_pov_severity(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    povline = c(0.5, 1, 2, 3),
#'                    complete = FALSE)
#'
#' # Example 3: Multiple poverty lines, returning list format
#' pipgd_pov_severity(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    povline = c(0.5, 1, 2, 3),
#'                    format  = "list")
#'
#' # Example 4: Multiple poverty lines, returning detailed list format
#' pipgd_pov_severity(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    povline = c(0.5, 1, 2, 3),
#'                    format  = "list",
#'                    complete = TRUE)
#'
#' # Example 5: Multiple poverty lines, returning atomic format
#' pipgd_pov_severity(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    povline = c(0.5, 1, 2, 3),
#'                    format  = "atomic",
#'                    complete = FALSE)
#'
pipgd_pov_severity <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = 1,
    times_mean     = 1,
    povshare       = NULL,
    povline        = ifelse(is.null(povshare),
                            mean*times_mean,
                            NA_real_),
    format         = c("dt", "list", "atomic"),
    lorenz         = NULL,
    complete       = getOption("pipster.return_complete")
  ) {

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipgd_pov_severity_v <- Vectorize(
    FUN            = pipgd_pov_severity_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_povsev <- pipgd_pov_severity_v(
    pipster_object = pipster_object,
    welfare        = welfare,
    weight         = weight,
    mean           = mean,
    times_mean     = times_mean,
    povshare       = povshare,
    povline        = povline,
    lorenz         = lorenz,
    complete       = complete
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format(
    ld       = list_povsev,
    povline  = povline,
    var      = "pov_severity",
    format   = format,
    complete = complete
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}






#' Estimate Watts poverty index (non-vectorized)
#'
#' This function is not vectorized and thus is not exported. Use
#' [pipgd_watts] instead.
#'
#' @inheritParams pipgd_pov_gap_nv
#'
#' @return list: contains numeric Watts ratio and, if `complete=TRUE`,
#' also returns all params.
#'
#' @keywords internal
pipgd_watts_nv <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = 1,
    times_mean     = 1,
    povshare       = NULL,
    povline        = ifelse(is.null(povshare),
                            mean*times_mean,
                            NA_real_),
    lorenz         = NULL,
    complete       = getOption("pipster.return_complete")
){
  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  po <- is_valid_inputs_pov(pl)

  # __________________________________________________________________________
  # Params--------------------------------------------------------------------
  if (po &
      (!is.null(pipster_object$results$pov_stats$headcount))) {
    params  <- pipster_object$params
    results <- pipster_object$results
  } else {
    pipster_object <- pipgd_pov_headcount_nv(pipster_object = pipster_object,
                                             welfare        = welfare,
                                             weight         = weight,
                                             mean           = mean,
                                             times_mean     = times_mean,
                                             povshare       = povshare,
                                             povline        = povline,
                                             lorenz         = lorenz,
                                             complete       = TRUE)
    params  <- pipster_object$params
    results <- pipster_object$results
  }

  check_pipgd_params(pl)

  # Lorenz----------------------------------------------------------------------
  #_____________________________________________________________________________

  lorenz <- choose_lorenz_for_pov(pipster_object,
                                  params,
                                  lorenz)

  # povline---------------------------------------------------------------------
  #_____________________________________________________________________________
  if (!is.null(povshare) & (is.null(povline) || is.na(povline))) {
    derive_ <-
      paste0("wbpip::derive_", lorenz) |>
      parse(text = _)

    povline <-
      mean * eval(derive_)(povshare,
                           params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["C"]])

  }

  # ____________________________________________________________________________
  # Calculate Watts-------------------------------------------------------------
  watts_ <-
    paste0("wbpip::gd_compute_watts_", lorenz) |>
    parse(text = _)

  wr <- eval(watts_)(mean      = mean,
                     povline   = povline,
                     headcount = pipster_object$results$pov_stats$headcount,
                     A         = params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                     B         = params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                     C         = params$gd_params[[lorenz]]$reg_results$coef[["C"]])

  attributes(wr) <- NULL

  # ____________________________________________________________________________
  # Return----------------------------------------------------------------------
  if (isFALSE(complete)) {
    results <- list()
    results$pov_stats$watts  <- wr
    results$pov_stats$lorenz <- lorenz
    return(results)
  }
  results$pov_stats$watts  <- wr
  results$pov_stats$lorenz <- lorenz
  pipster_object$results   <- results

  pipster_object


}




#' Estimate Watts poverty index
#'
#' Computes Watts Index from either beta or quadratic Lorenz fit.
#' The first distribution-sensitive poverty measure was proposed in 1968 by Watts.
#' It is defined as the mean across the population of the proportionate poverty
#' gaps, as measured by the log of the ratio of the poverty line to income,
#' where the mean is formed over the whole population, counting the nonpoor as
#' having a zero poverty gap.
#'
#' @inheritParams pipgd_pov_gap_nv
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#' single numeric vector, whose names are corresponding selected Lorenz for
#' each value.  Default is "dt"
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `watts` and `lorenz`.  Check `format` argument to change
#' the output format.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with the pip_gd dataset and default poverty line
#' pipgd_watts(welfare = pip_gd$L,
#'             weight  = pip_gd$P)
#'
#' # Example 2: Specifying a different poverty line and output as a list
#' pipgd_watts(welfare = pip_gd$L,
#'             weight  = pip_gd$P,
#'             povline = 1.9,
#'             format  = "list")
#'
#'
#' # Example 3: Detailed output with complete = TRUE
#' pipgd_watts(welfare  = pip_gd$L,
#'             weight   = pip_gd$P,
#'             format   = "list",
#'             complete = TRUE)
#'
#' # Example 4: Custom mean and times_mean with data.table format
#' pipgd_watts(welfare    = pip_gd$L,
#'             weight     = pip_gd$P,
#'             mean       = 109.9,
#'             times_mean = 1.5)
#'
pipgd_watts <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = 1,
    times_mean     = 1,
    povshare       = NULL,
    povline        = ifelse(is.null(povshare),
                            mean*times_mean,
                            NA_real_),
    format         = c("dt", "list", "atomic"),
    lorenz         = NULL,
    complete       = getOption("pipster.return_complete")
) {

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipgd_watts_v <- Vectorize(
    FUN            = pipgd_watts_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_watts <- pipgd_watts_v(
    pipster_object = pipster_object,
    welfare        = welfare,
    weight         = weight,
    mean           = mean,
    times_mean     = times_mean,
    povshare       = povshare,
    povline        = povline,
    lorenz         = lorenz,
    complete       = complete
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format(
    ld       = list_watts,
    var      = "watts",
    povline  = povline,
    format   = format,
    complete = complete
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}


# WRAPPERS -------

#' Validate group data parameters
#'
#' Always used for poverty measures. For dist. only used only when
#' `pipster_object` is null. These are NULL because not every function
#' takes them as arguments, in which case they should maintain the same
#' functionality as when they are NULL and the [create_pipster_object]
#' default should be given to them.
#'
#' @inheritParams pipgd_welfare_share_at
#'
#' @return list: `params` to be used in gd functions
#' @keywords internal
validate_params <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = NULL,
    times_mean     = NULL,
    n              = NULL,
    popshare       = NULL,
    povshare       = NULL,
    povline        = NULL,
    lorenz         = NULL
) {

  # set null args to correct defaults
  if (is.null(mean))       mean       <- 1
  if (is.null(times_mean)) times_mean <- 1
  if (is.null(n))          n          <- 10
  if (is.null(popshare))   popshare   <- seq(from = 1/n,
                                             to   = 1,
                                             by   = 1/n)
  if (is.null(povline)) {
    povline <- ifelse(is.null(povshare),
                      mean * times_mean,
                      NA_real_)
  }

  if (!is.null(pipster_object)) {
    welfare <- pipster_object$params$data$welfare
    weight  <- pipster_object$params$data$weight
  } else if (is.null(weight)) {
    weight  <- rep(1, length(welfare))
  }

  # create pipster object
  p_object <- create_pipster_object(welfare    = welfare,
                                    weight     = weight,
                                    mean       = mean,
                                    times_mean = times_mean,
                                    n          = n,
                                    povshare   = povshare,
                                    popshare   = popshare,
                                    povline    = povline,
                                    lorenz     = lorenz)
  p_object
}


is_valid_inputs_pov <- function(pl) {
  # check that all of `pl`
  # are the same as the arguments in
  # pipster_object$args
  if (is.null(pl$pipster_object)) {
    return(FALSE)
  }

  if (is.null(c(pl$mean,
                pl$times_mean,
                pl$povshare,
                pl$povline,
                pl$lorenz))) {
    return(TRUE)
  }

  c_mean    <- identical(pl$pipster_object$args$mean,
                          pl$mean)
  c_tmean   <- identical(pl$pipster_object$args$times_mean,
                           pl$times_mean)
  c_popsh   <- identical(pl$pipster_object$args$povshare,
                           pl$povshare)
  c_povline <- identical(pl$pipster_object$args$povline,
                           pl$povline)
  c_lorenz  <- identical(pl$pipster_object$args$lorenz,
                             pl$lorenz)
  if (all(c_mean, c_tmean, c_popsh, c_povline, c_lorenz)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

choose_lorenz_for_pov <- function(pipster_object = NULL,
                                  params = NULL,
                                  lorenz = NULL) {
  if (!is.null(lorenz)) {
    chosen_lorenz <- match.arg(lorenz, c("lq", "lb"))
  } else {
    # If lorenz is specified in pipster_object and not overridden by the function call, use it
    if (!is.null(pipster_object$args$lorenz)) {
      chosen_lorenz <- pipster_object$args$lorenz
    } else {
      # Use the selected lorenz if not specified in the function call or pipster_object
      chosen_lorenz <- params$selected_lorenz$for_pov
    }
  }

  return(chosen_lorenz)
}



