# This file contains all the functions related to distributional measures.
#  on grouped data


#' Welfare share by quantile in group data
#'
#' `pipgd_welfare_share_at` returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 as default), to estimate the corresponding
#' share of welfare in each.
#'
#' @inheritParams pipgd_select_lorenz
#' @param pipster_object pipster object created using [create_pipster_object]
#' @param lorenz character or NULL. Lorenz curve selected. It could be "lq" for
#'   Lorenz Quadratic or "lb" for Lorenz Beta
#' @param popshare numeric: vector of share of population. Default is `seq(from
#'   = 1/n, to = 1, by = 1/n)`, which is determined by argument n
#' @param n numeric scalar for the number of quantiles to be used in `popshare`
#' @param ... additional arguments for [pipgd_select_lorenz]
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
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    complete       = getOption("pipster.return_complete"),
    lorenz         = NULL,
    n              = NULL,
    popshare       = NULL
) {

  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)
  po <- is_valid_inputs_dist(pl)

  # __________________________________________________________________________
  # params--------------------------------------------------------------------
  if (!po) {
    pipster_object <- validate_params(pipster_object = pipster_object,
                                      welfare        = welfare,
                                      weight         = weight,
                                      lorenz         = lorenz,
                                      n              = n,
                                      popshare       = popshare)

  }
  params   <- pipster_object$params
  popshare <- pipster_object$args$popshare
  results  <- pipster_object$results

  # ____________________________________________________________________________
  # Lorenz ---------------------------------------------------------------------
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  # ____________________________________________________________________________
  # wbpip ----------------------------------------------------------------------
  qfun <- paste0("wbpip::value_at_", lorenz) |>
    parse(text = _)

  value_at_vc <- Vectorize(eval(qfun),
                           vectorize.args = "x",
                           SIMPLIFY       = TRUE)

  value_at <- value_at_vc(x = popshare,
                          params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                          params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                          params$gd_params[[lorenz]]$reg_results$coef[["C"]])

  # Return----------------------------------------------------------------------
  #_____________________________________________________________________________
  if (isFALSE(complete)) results <- list()
  results$dist_stats$popshare          <- popshare
  results$dist_stats$welfare_share_at  <- value_at
  results$dist_stats$lorenz            <- lorenz

  if (isFALSE(complete)) {
    return(results)
  }
  pipster_object$results <- results

  pipster_object

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
#' @param ... additional arguments for [pipgd_select_lorenz]
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
  function(pipster_object = NULL,
           welfare        = NULL,
           weight         = NULL,
           complete       = getOption("pipster.return_complete"),
           lorenz         = NULL,
           n              = 10,
           popshare       = seq(from = 1/n, to = 1, by = 1/n)) {

    # Defenses--------------------------------------------------------------------
    #_____________________________________________________________________________
    pl <- as.list(environment())
    check_pipgd_params(pl)
    po <- is_valid_inputs_dist(pl)

    # __________________________________________________________________________
    # params--------------------------------------------------------------------
    if (!po ||
        is.null(pipster_object$results$dist_stats$welfare_share_at)) {
      pipster_object <- pipgd_welfare_share_at(pipster_object = pipster_object,
                                               welfare        = welfare,
                                               weight         = weight,
                                               lorenz         = lorenz,
                                               n              = n,
                                               popshare       = popshare,
                                               complete       = TRUE)

    }
    results <- pipster_object$results
    shr     <- results$dist_stats$welfare_share_at
    shr     <- c(shr[1],
                 diff(shr))

    # Return----------------------------------------------------------------------
    #_____________________________________________________________________________
    if (isFALSE(complete)) {
      results <- list()
      results$dist_stats$popshare      <- popshare
      results$dist_stats$welfare_share <- shr
      results$dist_stats$lorenz        <- lorenz
      return(results)
    }

    results$dist_stats$popshare      <- popshare
    results$dist_stats$welfare_share <- shr
    results$dist_stats$lorenz        <- lorenz
    pipster_object$results           <- results

    pipster_object

}


#' Get quantile at specified shared of population - grouped data
#'
#' `pipgd_quantile` returns the quantile (i.e. welfare value) that corresponds
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
  function(pipster_object = NULL,
           welfare        = NULL,
           weight         = NULL,
           n              = 10,
           popshare       = seq(from = 1/n, to = 1, by = 1/n),
           mean           = 1,
           complete       = getOption("pipster.return_complete"),
           lorenz         = NULL) {

    # Defenses--------------------------------------------------------------------
    #_____________________________________________________________________________
    pl <- as.list(environment())
    check_pipgd_params(pl)
    po <- is_valid_inputs_dist(pl, mean = TRUE)

    # __________________________________________________________________________
    # params--------------------------------------------------------------------
    if (!po) {
      pipster_object <- validate_params(pipster_object = pipster_object,
                                        welfare        = welfare,
                                        weight         = weight,
                                        lorenz         = lorenz,
                                        mean           = mean,
                                        n              = n,
                                        popshare       = popshare)

    }
    params  <- pipster_object$params
    results <- pipster_object$results
    mean    <- pipster_object$args$mean


    if (is.null(lorenz)) {
      lorenz <- params$selected_lorenz$for_dist
    } else {
      match.arg(lorenz, c("lq", "lb"))
    }

    qfun <- paste0("wbpip::derive_", lorenz) |>
      parse(text = _)

    qt <-  eval(qfun)(x = popshare,
                      params$gd_params[[lorenz]]$reg_results$coef[["A"]],
                      params$gd_params[[lorenz]]$reg_results$coef[["B"]],
                      params$gd_params[[lorenz]]$reg_results$coef[["C"]])
    qt <- qt*mean

    # Return----------------------------------------------------------------------
    #_____________________________________________________________________________
    if (isFALSE(complete)) {
      results <- list()
      results$dist_stats$popshare <- popshare
      results$dist_stats$quantile <- qt
      results$dist_stats$lorenz   <- lorenz
      return(results)
    }

    results$dist_stats$popshare <- popshare
    results$dist_stats$quantile <- qt
    results$dist_stats$lorenz   <- lorenz
    pipster_object$results      <- results

    pipster_object
}

#' Compute Gini coefficient
#'
#' Gini is computed using either the beta or quadratic Lorenz
#' functions.
#'
#' @inheritParams pipgd_pov_headcount_nv
#' @param ... additional arguments for [pipgd_select_lorenz]
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
  pipster_object = NULL,
  welfare    = NULL,
  weight     = NULL,
  complete   = getOption("pipster.return_complete"),
  lorenz     = NULL
){

  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)
  po <- is_valid_inputs_dist(pl)

  # __________________________________________________________________________
  # params--------------------------------------------------------------------
  if (!po) {
    pipster_object <- validate_params(pipster_object = pipster_object,
                                      welfare        = welfare,
                                      weight         = weight,
                                      lorenz         = lorenz)

  }
  params <- pipster_object$params
  results <- pipster_object$results
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
        key_values = c(
              e         = params$gd_params$lq$key_values$e,
              m         = params$gd_params$lq$key_values$m,
              n         = params$gd_params$lq$key_values$n,
              r         = params$gd_params$lq$key_values$r
        )
      )
  }

  attributes(gini) <- NULL

  # Return----------------------------------------------------------------------
  #_____________________________________________________________________________
  if (isFALSE(complete)) {
    results <- list()
    results$dist_stats$gini   <- gini
    results$dist_stats$lorenz <- lorenz
    return(results)
  }
  results$dist_stats$gini   <- gini
  results$dist_stats$lorenz <- lorenz
  pipster_object$results    <- results

  pipster_object
}




#' Compute MLD
#'
#' MLD (Mean Logarithimic Deviation) is computed using either the beta or
#' quadratic Lorenz functions.
#'
#' @inheritParams pipgd_gini
#' @param ... additional arguments for [pipgd_select_lorenz]
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
pipgd_mld <- function(
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    complete       = getOption("pipster.return_complete"),
    lorenz         = NULL
){

  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)
  po <- is_valid_inputs_dist(pl)

  # __________________________________________________________________________
  # params--------------------------------------------------------------------
  if (!po) {
    pipster_object <- validate_params(pipster_object = pipster_object,
                                      welfare        = welfare,
                                      weight         = weight,
                                      lorenz         = lorenz)#,
                                      # n              = n,
                                      # popshare       = popshare)

  }
  params  <- pipster_object$params
  results <- pipster_object$results
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

  # Return----------------------------------------------------------------------
  #_____________________________________________________________________________
  if (isFALSE(complete)) {
    results <- list()
    results$dist_stats$mld    <- mld
    results$dist_stats$lorenz <- lorenz
    return(results)
  }

  results$dist_stats$mld    <- mld
  results$dist_stats$lorenz <- lorenz
  pipster_object$results    <- results

  pipster_object

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
    pipster_object = NULL,
    welfare        = NULL,
    weight         = NULL,
    mean           = NULL,
    complete       = getOption("pipster.return_complete"),
    lorenz         = NULL
){
  # Defenses--------------------------------------------------------------------
  #_____________________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)
  po <- is_valid_inputs_dist(pl, mean = TRUE, gini = TRUE)

  # __________________________________________________________________________
  # params--------------------------------------------------------------------
  if (!po ||
      is.null(pipster_object$results$dist_stats$gini)) {
    pipster_object <- validate_params(pipster_object = pipster_object,
                                      welfare        = welfare,
                                      weight         = weight,
                                      lorenz         = lorenz,
                                      mean           = mean)
    pipster_object <- pipgd_gini(pipster_object = pipster_object,
                                 lorenz         = lorenz,
                                 complete       = TRUE)

  }
  params  <- pipster_object$params
  results <- pipster_object$results
  # ____________________________________________________________________________
  # Arguments-------------------------------------------------------------------
  gini <- results$dist_stats$gini
  p0   <- 0.5 # constant
  mean <- pipster_object$args$mean
  dcm  <- (1 - gini)*mean

  # ____________________________________________________________________________
  # Lorenz----------------------------------------------------------------------
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  # ____________________________________________________________________________
  # Polarization----------------------------------------------------------------
  polarization_ <- paste0("wbpip:::gd_compute_polarization_",
                          lorenz) |>
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

  # ____________________________________________________________________________
  # Return----------------------------------------------------------------------
  if (isFALSE(complete)) {
    results <- list()
    results$dist_stats$gini          <- gini
    results$dist_stats$polarization  <- polarization
    results$dist_stats$lorenz        <- lorenz
    return(results)
  }
  results$dist_stats$gini          <- gini
  results$dist_stats$polarization  <- polarization
  results$dist_stats$lorenz        <- lorenz
  pipster_object$results           <- results

  pipster_object

}








#' Check that inputs for gd dist functions are valid
#'
#' Inputs are considered "valid" if the arguments used to create
#' the pipster object - and stored in `pipster_object$args` - is the
#' same as the argument values being used in the function being called.
#' However, if the function being called has all arguments
#' (except `pipster_object`) being NULL, the `pipster_object$args` will
#' automatically be used. If supplied arguments are non-NULL and different from
#' the `pipster_object$args` then the pipster object is re-estimated internally
#' using teh newly supplied arguments.
#'
#' @param pl
#' @param mean logical: if TRUE then will also check the `mean` argument.
#' Default is FALSE because not every dist function requires mean.
#' @param gini logical: if TRUE then will also check the `gini` argument.
#' Default is FALSE because not every dist function requires gini
#'
#' @return logical: TRUE if valid, FALSE if invalid. pipster_object must
#' be re-estimated if FALSE
#' @keywords internal
is_valid_inputs_dist <- function(pl, mean = FALSE, gini = FALSE) {
  # check that all of `pl`
  # are the same as the arguments in
  # pipster_object$args
  if (is.null(pl$pipster_object)) {
    return(FALSE)
  }

  if (is.null(c(pl$mean,
                pl$n,
                pl$gini,
                pl$popshare,
                pl$lorenz))) {
    return(TRUE)
  }
  # checks
  c_n       <- identical(pl$pipster_object$args$n,
                         pl$n)
  c_popsh   <- identical(pl$pipster_object$args$popshare,
                         pl$popshare)
  c_lorenz  <- identical(pl$pipster_object$args$lorenz,
                         pl$lorenz)
  c_prod    <- c_n*c_popsh*c_lorenz
  # gini
  if (isTRUE(gini)) {
    c_gini <- !is.null(pl$pipster_object$results$dist_stats$gini)
    c_prod <- c_prod*c_gini
  }
  # mean
  if (isTRUE(mean)) {
    c_mean <- identical(pl$pipster_object$args$mean,
                        pl$mean)
    c_prod <- c_prod*c_mean
  }

  if (c_prod == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


