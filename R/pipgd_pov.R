# This file contains all the functions related to absolute poverty measures


#' Estimate poverty headcount (FGT0)
#'
#' This function is not vectorized and thus is not exported.Use
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
    params <- pipgd_select_lorenz(welfare  = welfare,
                                  weight   = weight,
                                  complete = TRUE,
                                  mean     = mean,
                                  povline  = povline)
  } else {
    params <- pipgd_select_lorenz(welfare  =  params$data$welfare,
                                  weight   =  params$data$weight,
                                  complete = TRUE,
                                  mean     = mean,
                                  povline  = povline)
  }

  # force selection of lorenz
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
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
  params$pov_stats$lorenz <- lorenz
  return(params)
}


#' Estimate poverty headcount (FGT0)
#'
#' @inheritParams pipgd_pov_headcount_nv
#' @inheritParams return_format
#'
#' @return list of numeric vector. Check `format` argument
#' @export
#'
#' @examples
#' pipgd_pov_headcount(
#' welfare = pip_gd$L,
#' weight  = pip_gd$P,
#' mean = 109.90,
#' povline = 89,
#' complete = FALSE)
#' # Return data.table
#' pipgd_pov_headcount(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' complete = FALSE)
#'
#' # Return list
#' pipgd_pov_headcount(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "list")
#'
#' # Return list complete
#' pipgd_pov_headcount(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "list",
#' complete = TRUE)
#'
#' # Return data.table
#' pipgd_pov_headcount(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "atomic",
#' complete = FALSE)
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
  if (!is.null(welfare)) {
    params <- pipgd_pov_headcount_nv(welfare  = welfare,
                                     weight   = weight,
                                    complete = TRUE,
                                    mean     = mean,
                                    povline  = povline)
  } else {
    params <- pipgd_pov_headcount_nv(welfare  =  params$data$welfare,
                                     weight   =  params$data$weight,
                                     complete = TRUE,
                                     mean     = mean,
                                     povline  = povline)
  }

  # force selection of lorenz
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

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
#' @return list of poverty gaps. See `format` parameter
#' @export
#'
#' @examples
#' pipgd_pov_gap(
#' welfare = pip_gd$L,
#' weight  = pip_gd$P,
#' mean = 109.90,
#' povline = 89,
#' complete = FALSE)
#' # Return data.table
#' pipgd_pov_gap(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' complete = FALSE)
#'
#' # Return list
#' pipgd_pov_gap(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "list")
#'
#' # Return list complete
#' pipgd_pov_gap(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "list",
#' complete = TRUE)
#'
#' # Return data.table
#' pipgd_pov_gap(
#' welfare = pip_gd$L,
#' weight = pip_gd$P,
#' povline = c(.5, 1, 2, 3),
#' format = "atomic",
#' complete = FALSE)
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
  pipgd_pov_gap_v <- Vectorize(pipgd_pov_gap_nv,
                               vectorize.args = "povline",
                               SIMPLIFY = FALSE)


  ld <- pipgd_pov_gap_v(welfare    = welfare,
                        weight     = weight,
                        params     = params,
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


pipgd_pov_severity_nv <- function() {

}

pipgd_pov_severity <- function() {

}

