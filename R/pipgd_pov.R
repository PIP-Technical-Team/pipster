# This file contains all the functions related to absolute poverty measures


#' Estimate poverty headcount (FGT0)
#'
#' This function is not vectorized and thus is not exported.Use
#' [pipgd_pov_headcount] instead.
#'
#' @inheritParams pipgd_select_lorenz
#' @inheritParams pipgd_welfare_share_at
#' @pov
#'
#' @return numeric poverty headcount
#' @keywords internal
#' @examples
#' pipgd_pov_headcount_nv(
#' welfare = pip_gd$L,
#' weight  = pip_gd$P,
#' mean = 109.90,
#' povline = 89,
#' complete = FALSE)
#'
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
#' povline = c(.5, 1, 2, 3))
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
#' format = "atomic")
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
