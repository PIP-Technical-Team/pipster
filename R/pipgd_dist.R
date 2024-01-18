# This file contains all the functions related to distributional measures.
#  on grouped data


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
#' # Example 1: Basic usage with default parameters
#' welfare_share_default <- pipgd_welfare_share_at(welfare = pip_gd$L,
#'                                                 weight = pip_gd$P,
#'                                                 complete = FALSE)
#'
#' # Example 2: Specifying a custom number of quantiles (n = 5)
#' welfare_share_custom_quantiles <- pipgd_welfare_share_at(welfare = pip_gd$L,
#'                                                          weight = pip_gd$P,
#'                                                          complete = FALSE,
#'                                                          n = 5)
#'
#' # Example 3: Using a custom population share vector
#' welfare_share_custom_popshare <- pipgd_welfare_share_at(welfare = pip_gd$L,
#'                                                         weight = pip_gd$P,
#'                                                         complete = FALSE,
#'                                                         popshare = pip_gd$R)
#'
#'
#' # Example 4: Using a different Lorenz curve (e.g., Lorenz Beta)
#' welfare_share_lorenz_beta <- pipgd_welfare_share_at(welfare = pip_gd$L,
#'                                                     weight = pip_gd$P,
#'                                                     complete = FALSE,
#'                                                     lorenz = "lb")
#'
#'
#' # Example 5: Detailed output with complete = TRUE
#' welfare_share_detailed <- pipgd_welfare_share_at(welfare = pip_gd$L,
#'                                                  weight = pip_gd$P,
#'                                                  complete = TRUE)
pipgd_welfare_share_at <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    complete   = getOption("pipster.return_complete"),
    lorenz     = NULL,
    n          = 10,
    popshare   = seq(from = 1/n, to = 1, by = 1/n)
) {

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
    match.arg(lorenz, c("lq", "lb"))
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
#' welfare held by a particular share of the population, which is in a sense
#' the cumulative share. Instead, `pipgd_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
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
  function(params     = NULL,
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
      match.arg(lorenz, c("lq", "lb"))
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


#' Get quantile at specified shared of population - grouped data
#'
#' `pipgd_quantile` returns the quantile (i.e., monetary value) that corresponds
#' to share of the population that lives below that threshold.
#'
#' This is basically the inverse of estimating the poverty rate (headcount or
#' population share) below the poverty line. In this case, you provide the
#' headcount and `pipgd_quantile` returns the "poverty line".
#'
#' The quantiles are calculated as function of the mean of the distribution
#' times an `x` factor. Basically, the quantile is `x` times the mean. By
#' default, the mean is equal to 1, which implies that, if no mean value if
#' provided, the return value is equal to `x`.
#'
#' **NOTE:** the outcome from `pipgd_quantile` is not necessarily the inverse
#' of [pipgd_pov_headcount]. The reason for this is that, [pipgd_pov_headcount]
#' selects the Lorenz parametrization that fits better at that specified point
#' of the distribution (i.e., the poverty lines). [pipgd_quantile], in contrast,
#' use the same Lorenz parametrization for any point. The lorenz used is the one
#' that fits best for all distributional measures.
#'
#' @inheritParams pipgd_welfare_share_at
#' @inheritParams pipgd_select_lorenz
#' @return vector of quantiles
#' @export
#'
#' @examples
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P)
pipgd_quantile <-
  function(params     = NULL,
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
      match.arg(lorenz, c("lq", "lb"))
    }


    qfun <- paste0("wbpip::derive_", lorenz) |>
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





#-------------------------------------------------
# GINI -------------------------------------------
#-------------------------------------------------

#' Compute Gini coefficient
#'
#' Gini is computed using either the beta or quadratic Lorenz
#' functions.
#'
#' @inheritParams pipgd_pov_headcount_nv
#'
#' @return list: contains numeric MLD and, if `complete=TRUE`,
#' also returns all params.
#' @export
#'
#' @examples
#' pipgd_gini(welfare = pip_gd$L,
#'            weight  = pip_gd$P)
pipgd_gini <- function(
  params     = NULL,
  welfare    = NULL,
  weight     = NULL,
  mean       = 1,
  times_mean = 1,
  popshare   = NULL,
  povline    = ifelse(is.null(popshare),
                      mean*times_mean,
                      NA_real_),
  complete   = getOption("pipster.return_complete"),
  lorenz     = NULL
){

  #   _________________________________________________________________
  #   Defenses
  #   _________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   _________________________________________________________________
  #   Params
  #   _________________________________________________________________
  if (!is.null(welfare)) {
    params <- pipgd_select_lorenz(
      welfare  = welfare,
      weight   = weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  } else {
    params <- pipgd_select_lorenz(
      welfare  =  params$data$welfare,
      weight   =  params$data$weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  }

  #   _________________________________________________________________
  #   Select Lorenz
  #   _________________________________________________________________
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  #   _________________________________________________________________
  #   Gini
  #   _________________________________________________________________
  if (lorenz == "lb") {
    gini <-
      wbpip::gd_compute_gini_lb(
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        nbins     = 499
      )
  } else if (lorenz == "lq") {
    gini <-
      wbpip::gd_compute_gini_lq(
        A         = params$gd_params$lq$reg_results$coef[["A"]],
        B         = params$gd_params$lq$reg_results$coef[["B"]],
        C         = params$gd_params$lq$reg_results$coef[["C"]],
        e         = params$gd_params$lq$key_values$e,
        m         = params$gd_params$lq$key_values$m,
        n         = params$gd_params$lq$key_values$n,
        r         = params$gd_params$lq$key_values$r
      )
  }

  attributes(gini) <- NULL

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$gini  <- gini
  params$dist_stats$lorenz <- lorenz

  return(
    params
  )

}




#-------------------------------------------------
# MLD -------------------------------------------
#-------------------------------------------------

#' Compute MLD
#'
#' MLD is computed using either the beta or quadratic Lorenz
#' functions.
#'
#' @inheritParams pipgd_gini
#'
#' @return list: contains numeric MLD and, if `complete=TRUE`,
#' also returns all params.
#' @export
#'
#' @examples
#' pipgd_mld(welfare = pip_gd$L,
#'           weight  = pip_gd$P)
pipgd_mld <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    times_mean = 1,
    popshare   = NULL,
    povline    = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    complete   = getOption("pipster.return_complete"),
    lorenz     = NULL
){

  #   _________________________________________________________________
  #   Defenses
  #   _________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   _________________________________________________________________
  #   Params
  #   _________________________________________________________________
  if (!is.null(welfare)) {
    params <- pipgd_select_lorenz(
      welfare  = welfare,
      weight   = weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  } else {
    params <- pipgd_select_lorenz(
      welfare  =  params$data$welfare,
      weight   =  params$data$weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  }

  #   _________________________________________________________________
  #   Select Lorenz
  #   _________________________________________________________________
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  #   _________________________________________________________________
  #   Gini
  #   _________________________________________________________________
  if (lorenz == "lb") {
    mld <-
      wbpip::gd_compute_gini_lb(
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        dd        = 0.01
      )
  } else if (lorenz == "lq") {
    mld <-
      wbpip::gd_compute_mld_lq(
        A         = params$gd_params$lq$reg_results$coef[["A"]],
        B         = params$gd_params$lq$reg_results$coef[["B"]],
        C         = params$gd_params$lq$reg_results$coef[["C"]],
        dd        = 0.01
      )
  }

  attributes(mld) <- NULL

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$mld    <- mld
  params$dist_stats$lorenz <- lorenz

  return(
    params
  )

}
















