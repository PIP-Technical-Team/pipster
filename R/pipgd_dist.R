# This file contains all the functions related to distributional measures.

#' Welfare share by quantile in group data
#'
#' `pipgd_welfare_share_at` returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 be default), to estimate the corresponding
#' share of welfare in each.
#'
#' @inheritParams pipgd_select_lorenz
#' @param lorenz character or NULL. Lorenz curve selected. It could be "lq" for
#'   Lorenz Quadratic or "lb" for Lorenz Beta
#' @param popshare numeric: vector of share of population. Default is `seq(from
#'   = 1/n, to = 1, by = 1/n)`
#' @param n numeric scalar for the number of quantiles to be used in `popshare`
#'
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                         weight = pip_gd$P,
#'                         complete = FALSE)
pipgd_welfare_share_at <- function(params   = NULL,
                         welfare    = NULL,
                         weight     = NULL,
                         complete   = getOption("pipster.return_complete"),
                         lorenz     = NULL,
                         n          = 10,
                         popshare   = seq(from = 1/n, to = 1, by = 1/n)) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)


  #   ____________________________________________________
  #   Computations                              ####
  if (!is.null(welfare)) {
    params <- pipgd_select_lorenz(welfare = welfare,
                                  weight =  weight,
                                  complete   = TRUE)
  }

  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    lorenz <- match.arg(lorenz, c("lq", "lb"))
  }

  qfun <- paste0("wbpip:::value_at_", lorenz) |>
    parse(text = _)
  value_at_vc <- Vectorize(eval(qfun),
                           vectorize.args = "x",
                           SIMPLIFY = TRUE)

  value_at <-  value_at_vc(x = popshare,
                           params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                           params$gd_params[[lorenz]]$reg_results$coef[["C"]])


  #   ____________________________________________________________
  #   Return                                                ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$popshare         <- popshare
  params$dist_stats$welfare_share_at <- value_at
  return(params)

}

#' Quantile welfare share
#'
#' `pipgd_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that `pipgd_welfare_share_at` get the share of
#' welfare held by a particular share of the population. Instead,
#' `pipgd_quantile_welfare_share` returns the proportion of welfare that only
#' the specified quantile holds.
#'
#' @inheritParams pipgd_welfare_share_at
#'
#' @return list with welfare shares
#' @export
#'
#' @examples
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                         weight = pip_gd$P,
#'                         complete = FALSE)
pipgd_quantile_welfare_share <-
  function(params   = NULL,
           welfare    = NULL,
           weight     = NULL,
           complete   = getOption("pipster.return_complete"),
           lorenz     = NULL,
           n          = 10,
           popshare   = seq(from = 1/n, to = 1, by = 1/n)) {

 #   _________________________________________________________________
 #   Defenses                                                ####
    pl <- as.list(environment())
    check_pipgd_params(pl)


    #   ____________________________________________________
    #   Computations                              ####
    if (!is.null(welfare)) {
      params <- pipgd_select_lorenz(welfare = welfare,
                                    weight =  weight,
                                    complete   = TRUE)
    }

    if (is.null(lorenz)) {
      lorenz <- params$selected_lorenz$for_dist
    } else {
      lorenz <- match.arg(lorenz, c("lq", "lb"))
    }

    # get shares ------------------------
    shr <- pipgd_welfare_share_at(params = params,
                                  complete = FALSE)
    shr <- c(shr$dist_stats$welfare_share_at[1],
             diff(shr$dist_stats$welfare_share_at))

    #   ____________________________________________________________
    #   Return                                                ####
    if (isFALSE(complete))
      params <- vector("list")

    params$dist_stats$popshare         <- popshare
    params$dist_stats$quantile_welfare_share <- shr
    return(params)

}


#' Get quantile at specified shared of population
#'
#' `pipgd_quantile` returns the quantile (i.e., monetary value) that corresponds
#' to shared of the population that lives below that threshold.
#'
#' This is basically the inverse of estimated the poverty rate (headcount or
#' population share) below the poverty line. In this case, you provide the
#' headcount and `pipgs_quantile` returns the "poverty line".
#'
#' The quantiles are calculated as function of the mean of the distribution
#' times an `x` factor. Basically, the quantile is `x` times the mean. By
#' default, the mean is equal to 1, which implies that, if no mean value if
#' provided, the return value is equal to `x`.
#'
#'
#' @inheritParams pipgd_welfare_share_at
#' @inheritParams pipgd_validate_lorenz
#' @return vector of quantiles
#' @export
#'
#' @examples
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P)
pipgd_quantile <-
  function(params   = NULL,
           welfare    = NULL,
           weight     = NULL,
           n          = 10,
           popshare   = seq(from = 1/n, to = 1, by = 1/n),
           mean       = 1,
           complete   = getOption("pipster.return_complete"),
           lorenz     = NULL) {

    #   _________________________________________________________________
    #   Defenses                                                ####
    pl <- as.list(environment())
    check_pipgd_params(pl)


    #   ____________________________________________________
    #   Computations                              ####
    if (!is.null(welfare)) {
      params <- pipgd_select_lorenz(welfare = welfare,
                                    weight =  weight,
                                    complete   = TRUE)
    }

    if (is.null(lorenz)) {
      lorenz <- params$selected_lorenz$for_dist
    } else {
      lorenz <- match.arg(lorenz, c("lq", "lb"))
    }


    qfun <- paste0("wbpip:::derive_", lorenz) |>
      parse(text = _)
    # value_at_vc <- Vectorize(eval(qfun),
    #                          vectorize.args = "x",
    #                          SIMPLIFY = TRUE)

    qt <-  eval(qfun)(x = popshare,
                      params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                      params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                      params$gd_params[[lorenz]]$reg_results$coef[["C"]])
    qt <- qt*mean
    #   ____________________________________________________________
    #   Return                                                ####
    if (isFALSE(complete))
      params <- vector("list")

    params$dist_stats$popshare <- popshare
    params$dist_stats$quantile <- qt
    return(params)
}

