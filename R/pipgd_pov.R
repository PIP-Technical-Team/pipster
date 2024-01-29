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
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           lorenz     = NULL,
           complete   = getOption("pipster.return_complete")){
  #   _________________________________________________________________
  #   Defenses                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)


  #   ____________________________________________________
  #   Computations                              ####
  if (!is.null(welfare)) {
    popshare = popshare
    params <- pipgd_select_lorenz(welfare  = welfare,
                                  weight   = weight,
                                  complete = TRUE,
                                  #mean     = mean,
                                  popshare = popshare)
                                  #,povline  = povline)
  } else {
    params <- pipgd_select_lorenz(welfare  =  params$data$welfare,
                                  weight   =  params$data$weight,
                                  complete = TRUE,
                                  mean     = mean,
                                  popshare = popshare,
                                  povline  = povline)
  }

  # force selection of lorenz
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_pov
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  headcount <- params$gd_params[[lorenz]]$validity$headcount

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$pov_stats$headcount <- headcount
  params$pov_stats$lorenz    <- lorenz
  return(params)
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
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           format = c("dt", "list", "atomic"),
           lorenz     = NULL,
           complete   = getOption("pipster.return_complete")) {

    format <- match.arg(format)

    #   ____________________________________________________
    #   Computations                                     ####
    pipgd_pov_headcount_v <- Vectorize(pipgd_pov_headcount_nv,
                                    vectorize.args = "povline",
                                    SIMPLIFY = FALSE)


    ld <- pipgd_pov_headcount_v(welfare    = welfare,
                                weight     = weight,
                                params     = params,
                                povline    = povline,
                                popshare = popshare,
                                complete   = complete,
                                lorenz     = lorenz,
                                mean       = mean,
                                times_mean = times_mean)

    #   ____________________________________________________
    #   Return                                           ####

    out <- return_format(ld,
                         var = "headcount",
                         povline = povline,
                         complete = complete,
                         format = format)
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
pipgd_pov_gap_nv <- function(params     = NULL,
                             welfare    = NULL,
                             weight     = NULL,
                             mean       = 1,
                             times_mean = 1,
                             popshare   = NULL,
                             povline    = ifelse(is.null(popshare),
                                                 mean*times_mean,
                                                 NA_real_),
                             lorenz     = NULL,
                             complete   = getOption("pipster.return_complete")
                             ){
  #   _________________________________________________________________
  #   Defenses                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)


  #   ____________________________________________________
  #   Computations                              ####
  
  # Compute params when welfare is (is not) supplied
  if (!is.null(welfare)) {
    
    params <- pipgd_pov_headcount_nv(welfare  = welfare,
                                     weight   = weight,
                                     complete = TRUE,
                                     popshare = popshare,
                                     povline = povline)
  } else {
    params <- pipgd_pov_headcount_nv(welfare  =  params$data$welfare,
                                     weight   =  params$data$weight,
                                     complete = TRUE,
                                     popshare = popshare,
                                     povline  = povline)
  }

  # Compute povline when popshare is supplied 
  if (is.na(povline)) {
    welfare = params$data$welfare
    weight = params$data$weight
    povline = collapse::fquantile(x     = welfare, 
                                  probs = popshare, 
                                  w     = weight) |>
                                  unname()
  }

  # force selection of lorenz
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_pov
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  # Call wbpip:: function
  fun_to_vc <-
    paste0("wbpip:::gd_compute_pov_gap_", lorenz) |>
    parse(text = _)

  pov_gap <-
    eval(fun_to_vc)(mean      = mean,
                    povline   = povline,
                    headcount = params$pov_stats$headcount,
                    A         = params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                    B         = params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                    C         = params$gd_params[[lorenz]]$reg_results$coef[["C"]])
  attributes(pov_gap) <- NULL

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$pov_stats$pov_gap <- pov_gap
  params$pov_stats$lorenz <- lorenz

  params
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
pipgd_pov_gap <- function(params     = NULL,
                          welfare    = NULL,
                          weight     = NULL,
                          mean       = 1,
                          times_mean = 1,
                          popshare   = NULL,
                          povline    = ifelse(is.null(popshare),
                                              mean*times_mean,
                                              NA_real_),
                          format = c("dt", "list", "atomic"),
                          lorenz     = NULL,
                          complete   = getOption("pipster.return_complete")) {

  format <- match.arg(format)

  #   ____________________________________________________
  #   Computations                                     ####

  # Compute the povline (or the vector of poverty lines) when popshare is supplied 
  if (!is.null(popshare)) {
    #popshare = popshare
    povline = collapse::fquantile(x     = welfare, 
                                  probs = popshare, 
                                  w     = weight) |>
                                  unname()
  }
  pipgd_pov_gap_v <- Vectorize(pipgd_pov_gap_nv,
                               vectorize.args = "povline",
                               SIMPLIFY = FALSE)


  ld <- pipgd_pov_gap_v(welfare    = welfare,
                        weight     = weight,
                        params     = params,
                        # set popshare as NULL to not trigger the check_gd_params error 
                        popshare   = NULL,
                        povline    = povline,
                        complete   = complete,
                        lorenz     = lorenz,
                        mean       = mean,
                        times_mean = times_mean)

  #   ____________________________________________________
  #   Return                                           ####

  out <- return_format(ld,
                       var = "pov_gap",
                       povline = povline,
                       complete = complete,
                       format = format)
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
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    times_mean = 1,
    popshare   = NULL,
    povline    = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    lorenz     = NULL,
    pov_gap    = NULL,
    complete   = getOption("pipster.return_complete")
  ){
    # __________________________________________________________________________
    #   Defenses ---------------------------------------------------------------
    pl <- as.list(environment())
    check_pipgd_params(pl)

    # __________________________________________________________________________
    #   Computations -----------------------------------------------------------
  
    if (!is.null(pov_gap)) {
      if (!pov_gap == pipgd_pov_gap_nv(welfare = welfare, weight = weight)$pov_stats$pov_gap) {
        stop("argument `pov_gap` should be the output of `pipster:::pipgd_pov_gap_nv`, else leave `pov_gap = NULL`")
      } else {
        params <- pov_gap
      }
    } 
    
    if (!is.null(welfare)) {
        params <- pipgd_pov_gap_nv(
          welfare  = welfare,
          weight   = weight,
          complete = TRUE,
          mean     = mean,
          popshare = popshare,
          povline  = povline
        )} 
      
      else {
        params <- pipgd_pov_gap_nv(
          welfare  =  params$data$welfare,
          weight   =  params$data$weight,
          complete = TRUE,
          mean     = mean,
          popshare = popshare,
          povline  = povline
        )
      }
    
     if (is.na(povline)) {
      welfare = params$data$welfare
      weight = params$data$weight
      povline = collapse::fquantile(x     = welfare, 
                                  probs   = popshare, 
                                  w       = weight) |>
                                  unname()
    }

    # __________________________________________________________________________
    #   Select Lorenz ----------------------------------------------------------
    if (is.null(lorenz)) {
      lorenz <- params$selected_lorenz$for_pov
    } else {
      match.arg(lorenz, c("lq", "lb"))
    }

    # __________________________________________________________________________
    #   Calculate Severity -----------------------------------------------------
    if (lorenz == "lb") {
      pov_severity <-
        wbpip:::gd_compute_pov_severity_lb(
          u      = mean,
          headcount = params$pov_stats$headcount,
          pov_gap   = params$pov_stats$pov_gap,
          A         = params$gd_params$lb$reg_results$coef[["A"]],
          B         = params$gd_params$lb$reg_results$coef[["B"]],
          C         = params$gd_params$lb$reg_results$coef[["C"]]
        )
    } else if (lorenz == "lq") {
      pov_severity <-
        wbpip:::gd_compute_pov_severity_lq(
          mean      = mean,
          povline   = povline,
          headcount = params$pov_stats$headcount,
          pov_gap   = params$pov_stats$pov_gap,
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

    #   ____________________________________________________
    #   Return                                           ####
    if (isFALSE(complete)) {
      params <- vector("list")
    }

    params$pov_stats$pov_severity <- pov_severity
    params$pov_stats$lorenz       <- lorenz

    params


}


#' Estimate poverty severity
#'
#' @inheritParams pipgd_pov_gap_nv
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#' single numeric vector, whose names are corresponding selected Lorenz for
#' each value.  Default is "dt"
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
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    times_mean = 1,
    popshare   = NULL,
    povline    = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    format     = c("dt", "list", "atomic"),
    lorenz     = NULL,
    pov_gap    = NULL,
    complete   = getOption("pipster.return_complete")
  ) {

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # Compute the povline (or the vector of poverty lines) when popshare is supplied 
  if (!is.null(popshare)) {
    povline = collapse::fquantile(x     = welfare, 
                                  probs = popshare, 
                                  w     = weight) |>
                                  unname()
  }

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  pipgd_pov_severity_v <- Vectorize(
    FUN            = pipgd_pov_severity_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )
  list_povsev <- pipgd_pov_severity_v(
    params     = params,
    welfare    = welfare,
    weight     = weight,
    mean       = mean,
    times_mean = times_mean,
    popshare   = NULL,
    povline    = povline,
    lorenz     = lorenz,
    pov_gap    = pov_gap,
    complete   = complete
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format(
    ld       = list_povsev,
    povline = povline,
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
    params       = NULL,
    welfare      = NULL,
    weight       = NULL,
    mean         = 1,
    times_mean   = 1,
    popshare     = NULL,
    povline      = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    lorenz       = NULL,
    complete     = getOption("pipster.return_complete")
){
  # __________________________________________________________________________
  #   Defenses ---------------------------------------------------------------
  pl <- as.list(environment())
  check_pipgd_params(pl)

  # __________________________________________________________________________
  #   Computations -----------------------------------------------------------

    popshare = popshare
    povline = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_)

    if (!is.null(welfare)) {
  
      params <- pipgd_pov_headcount_nv(
        welfare  = welfare,
        weight   = weight,
        complete = TRUE,
        popshare = popshare
      )
    } else {
      params <- pipgd_pov_headcount_nv(
        welfare  =  params$data$welfare,
        weight   =  params$data$weight,
        complete = TRUE,
        mean     = mean,
        popshare = popshare,
        povline  = povline
      )
    }

  
  # __________________________________________________________________________
  #   Select Lorenz ----------------------------------------------------------
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_pov
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  # __________________________________________________________________________
  #   Calculate Watts -----------------------------------------------------

  if (lorenz == "lb") {
    wr <-
      wbpip:::gd_compute_watts_lb(
        mean      = mean,
        povline   = povline,
        headcount = params$pov_stats$headcount,
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        dd        = 0.005
      )
  } else if (lorenz == "lq") {
    wr <-
      wbpip:::gd_compute_watts_lq(
        mu        = mean,
        povline   = povline,
        headcount = params$pov_stats$headcount,
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        dd        = 0.01
      )
  }

  attributes(wr) <- NULL

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$pov_stats$watts  <- wr
  params$pov_stats$lorenz <- lorenz

  params


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
#' pipgd_watts(welfare = pip_gd$L,
#'             weight  = pip_gd$P,
#'             complete = TRUE)
#'
#' # Example 4: Custom mean and times_mean with data.table format
#' pipgd_watts(welfare = pip_gd$L,
#'             weight  = pip_gd$P,
#'             mean    = 109.9,
#'             times_mean = 1.5)
#'
pipgd_watts <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    times_mean = 1,
    popshare   = NULL,
    povline    = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    format     = c("dt", "list", "atomic"),
    lorenz     = NULL,
    complete   = getOption("pipster.return_complete")
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
    params     = params,
    welfare    = welfare,
    weight     = weight,
    mean       = mean,
    times_mean = times_mean,
    popshare   = popshare,
    povline    = povline,
    lorenz     = lorenz,
    complete   = complete
  )

  # ____________________________________________________________________________
  # Format ---------------------------------------------------------------------
  out <- return_format(
    ld       = list_watts,
    var      = "watts",
    povline = povline,
    format   = format,
    complete = complete
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  out

}
#
#
# pipgd_pov_headcount_v <- Vectorize(pipgd_pov_headcount_nv,
#                                    vectorize.args = "povline",
#                                    SIMPLIFY = FALSE)
#
#
# ld <- pipgd_pov_headcount_v(welfare    = welfare,
#                             weight     = weight,
#                             params     = params,
#                             povline    = povline,
#                             complete   = complete,
#                             lorenz     = lorenz,
#                             mean       = mean,
#                             times_mean = times_mean)
#
# #   ____________________________________________________
# #   Return                                           ####
#
# out <- return_format(ld,
#                      var = "headcount",
#                      povline = povline,
#                      complete = complete,
#                      format = format)
#pip
# out <- return_format(
#   ld       = list_povsev,
#   var      = "pov_severity",
#   format   = format,
#   complete = complete
# )
