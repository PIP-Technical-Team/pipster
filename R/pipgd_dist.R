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
#' @return Returns a nested list containing distribution statistics:
#' `$dist_stats$pop_share` is a numeric vector containing the share of the
#' population.
#' `$dist_stats$welfare_share_at` is a numeric vector showing the
#' corresponding share of welfare.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                        weight = pip_gd$P,
#'                        complete = FALSE)
#'
#' # Example 2: Specifying a custom number of quantiles (n = 5)
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                        weight = pip_gd$P,
#'                        complete = FALSE,
#'                        n = 5)
#'
#' # Example 3: Using a custom population share vector
#' custom_popshare_vector <- seq(from = 1/13, to = 1, length.out = 13)
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                        weight = pip_gd$P,
#'                        complete = FALSE,
#'                        popshare = custom_popshare_vector)
#'
#'
#' # Example 4: Using a specified Lorenz curve (e.g., Lorenz Beta)
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                        weight = pip_gd$P,
#'                        complete = FALSE,
#'                        lorenz = "lb")
#'
#' # Example 5: Detailed output with complete = TRUE
#' pipgd_welfare_share_at(welfare = pip_gd$L,
#'                        weight = pip_gd$P,
#'                        complete = TRUE)
#'
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
    params <- pipgd_select_lorenz(welfare    = welfare,
                                  weight     = weight,
                                  complete   = TRUE)
  } else {
    params <- pipgd_select_lorenz(welfare    = params$data$welfare,
                                  weight     = params$data$weight,
                                  complete   = TRUE)
  }

  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  qfun <- paste0("wbpip::value_at_", lorenz) |>
    parse(text = _)

  value_at_vc <- Vectorize(eval(qfun),
                           vectorize.args = "x",
                           SIMPLIFY       = TRUE)

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

  params

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
#' @return Returns a nested list containing distribution statistics:
#' `$dist_stats$pop_share` is a numeric vector containing the share of the
#' population.
#' `$dist_stats$quantile_welfare_share` is a numeric vector showing the
#' corresponding share of welfare.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                              weight = pip_gd$P,
#'                              complete = FALSE)
#'
#' # Example 2: Specifying a custom quantile (e.g. 5)
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                              weight = pip_gd$P,
#'                              complete = FALSE,
#'                              n = 5)
#'
#' # Example 3: Using a custom population share vector
#' custom_popshare_vector <- seq(from = 1/13, to = 1, length.out = 13)
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                              weight = pip_gd$P,
#'                              complete = FALSE,
#'                              popshare = custom_popshare_vector)
#'
#' # Example 4: Using a different Lorenz curve (e.g., Lorenz Beta)
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                              weight = pip_gd$P,
#'                              complete = FALSE,
#'                              lorenz = "lb")
#'
#' # Example 5: Detailed output with complete = TRUE
#' pipgd_quantile_welfare_share(welfare = pip_gd$L,
#'                              weight = pip_gd$P,
#'                              complete = TRUE)
#'
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
      params <- pipgd_select_lorenz(welfare    = welfare,
                                    weight     =  weight,
                                    complete   = TRUE)
    } else {
      params <- pipgd_select_lorenz(welfare    = params$data$welfare,
                                    weight     = params$data$weight,
                                    complete   = TRUE)
    }

    if (is.null(lorenz)) {
      lorenz <- params$selected_lorenz$for_dist
    } else {
      match.arg(lorenz, c("lq", "lb"))
    }

    # get shares ------------------------
    shr <- pipgd_welfare_share_at(params   = params,
                                  complete = FALSE,
                                  n        = n)
    shr <- c(shr$dist_stats$welfare_share_at[1],
             diff(shr$dist_stats$welfare_share_at))

    #   ____________________________________________________________
    #   Return                                                ####
    if (isFALSE(complete)) {
      params <- vector("list")
    }

    params$dist_stats$popshare               <- popshare
    params$dist_stats$quantile_welfare_share <- shr

    params

}


#' Get quantile at specified shared of population - grouped data
#'
#' `pipgd_quantile` returns the quantile (i.e., monetary value) that corresponds
#' to the share of the population that lives below that threshold.
#'
#' This is basically the inverse of estimating the poverty rate (headcount or
#' population share) below the poverty line. In this case, you provide the
#' headcount and `pipgd_quantile` returns the "poverty line".
#'
#' The quantiles are calculated as function of the mean of the distribution
#' times an `x` factor. Basically, the quantile is `x` times the mean. By
#' default, the mean is equal to 1, which implies that, if no mean value is
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
#'
#' @return Returns a nested list containing distribution statistics:
#' `$dist_stats$pop_share` is a numeric vector containing the share of the
#' population.
#' `$dist_stats$quantile` is a numeric vector showing the
#' corresponding quantile.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P)
#'
#' # Example 2: Specifying a custom number of quantiles
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P,
#'                n       = 5)
#'
#' # Example 3: Using a custom population share vector
#' custom_popshare_vector <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P,
#'                popshare = custom_popshare_vector)
#'
#'
#' # Example 4: Specifying a different Lorenz curve ('lb', Lorenz beta)
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P,
#'                lorenz  = "lb")
#'
#' # Example 5: Detailed output with complete = TRUE and different mean factor
#' pipgd_quantile(welfare = pip_gd$L,
#'                weight  = pip_gd$P,
#'                mean = 1.5,
#'                complete = TRUE)

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
    } else {
      params <- pipgd_select_lorenz(welfare    = params$data$welfare,
                                    weight     = params$data$weight,
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


#' Compute Gini coefficient
#'
#' Gini is computed using either the beta or quadratic Lorenz
#' functions.
#'
#' @inheritParams pipgd_pov_headcount_nv
#'
#' @return Returns a nested list containing distribution statistics:
#' `$dist_stats$gini` is a numeric vector containing the gini coefficient.
#' `$dist_stats$lorenz` is a numeric vector showing the lorenz curve used.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic Calculation of Gini Coefficient
#' pipgd_gini(welfare = pip_gd$L,
#'            weight = pip_gd$P)
#'
#' # Example 2: Gini Coefficient with a Specific Lorenz Curve (e.g. Lorenz beta)
#' pipgd_gini(welfare = pip_gd$L,
#'            weight = pip_gd$P,
#'            lorenz = "lb")
#'
#' # Example 3: Detailed Output of Gini Calculation
#' pipgd_gini(welfare = pip_gd$L,
#'            weight = pip_gd$P,
#'            complete = TRUE)
pipgd_gini <- function(
  params     = NULL,
  welfare    = NULL,
  weight     = NULL,
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
      complete = TRUE
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
        C         = params$gd_params$lb$reg_results$coef[["C"]]
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

  params
}


#' Compute MLD
#'
#' MLD (Mean Logarithimic Deviation) is computed using either the beta or
#' quadratic Lorenz functions.
#'
#' @inheritParams pipgd_gini
#'
#' @return Returns a nested list containing distribution statistics:
#' `$dist_stats$mld` is a numeric vector containing the mld calculation.
#' `$dist_stats$lorenz` is a numeric vector showing the lorenz curve used.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic MLD Calculation
#' pipgd_mld(welfare = pip_gd$L,
#'           weight = pip_gd$P)
#'
#' # Example 2: MLD with a Specific Lorenz Curve
#' pipgd_mld(welfare = pip_gd$L,
#'           weight = pip_gd$P,
#'           lorenz = "lb")
#'
#' # Example 3: Detailed Output of MLD Calculation
#' pipgd_mld(welfare = pip_gd$L,
#'           weight = pip_gd$P,
#'           complete = TRUE)
#'
pipgd_mld <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
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
      complete = TRUE
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

  # Compute mld
  mld_ <- paste0("wbpip::gd_compute_mld_", lorenz) |>
    parse(text = _)

  mld <- eval(mld_)(
    A         = params$gd_params[[lorenz]]$reg_results$coef[["A"]],
    B         = params$gd_params[[lorenz]]$reg_results$coef[["B"]],
    C         = params$gd_params[[lorenz]]$reg_results$coef[["C"]]
  )

  attributes(mld) <- NULL

  #   ____________________________________________________
  #   Return                                           ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$mld    <- mld
  params$dist_stats$lorenz <- lorenz

  params


}


#' Compute polarization index
#'
#' This function computes polarization index of the distribution (for grouped data)
#'
#'
#' @inheritParams pipgd_gini
#' @param gini numeric scalar of gini index, from `pipgd_gini()` or user supplied
#' @param mean numeric scalar of distribution mean. Default is 1
#'
#' @return Returns a nested list containing:
#' `$dist_stats$polarization` a numeric vector containing the index of polarization of the distribution;
#' `$dist_stats$lorenz` a character vector specifying which Lorenz curve is used.
#' If `complete = TRUE`, it returns a `pipgd_params` object with additional
#' details and intermediate calculations.
#'
#' @export
#'
#' @examples
#' # Example with welfare and weight vectors
#' pipgd_polarization(welfare = pip_gd$L,
#'                    weight  = pip_gd$P)
#'
#' # Example with list of params
#' # Params from `pipgd_select_lorenz()`
#' params = pipgd_select_lorenz(welfare  = pip_gd$L,
#'                              weight   = pip_gd$P,
#'                              complete = TRUE)
#' pipgd_polarization(params = params)
#'
#' # Example with a specific Lorenz
#' pipgd_polarization(welfare = pip_gd$L,
#'                    weight  = pip_gd$P,
#'                    lorenz  = "lb")
#'
#' # Example with complete output
#' pipgd_polarization(welfare  = pip_gd$L,
#'                    weight   = pip_gd$P,
#'                    complete = TRUE)
#'


pipgd_polarization <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    gini       = NULL,
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
      complete = TRUE
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
  #   Set p0 and compute dcm
  #   _________________________________________________________________

  # p0 - always 0.5
  p0   = 0.5
  mean = mean

  # gini

  if (is.null(gini)) {
    gini <- pipgd_gini(welfare = params$data$welfare,
                       weight  = params$data$weight,
                       lorenz  = lorenz)$dist_stats$gini
  } else {
    gini <- gini
  }

  dcm = (1 - gini)*mean

  # Compute polarization index
  polarization_ <- paste0("wbpip:::gd_compute_polarization_", lorenz) |>
    parse(text = _)

  polarization <- eval(polarization_)(
    mean      = mean,
    p0        = p0,
    dcm       = dcm,
    A         = params$gd_params[[lorenz]]$reg_results$coef[["A"]],
    B         = params$gd_params[[lorenz]]$reg_results$coef[["B"]],
    C         = params$gd_params[[lorenz]]$reg_results$coef[["C"]]
  )

  attributes(polarization) <- NULL

  #   ____________________________________________________
  #   Return                                           ####


  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$dist_stats$polarization  <- polarization
  params$dist_stats$lorenz        <- lorenz

  params

}












