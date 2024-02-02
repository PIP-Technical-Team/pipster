# This file contains all the functions related to distributional measures
#  on microdata (md)



#' Get quantile at specified shared of population - micro data
#'
#' `pipmd_quantile` returns the quantile (i.e., monetary value) that corresponds
#' to share of the population that lives below that threshold.
#'
#' This is basically the inverse of estimating the poverty rate (headcount or
#' population share) below the poverty line. In this case, you provide the
#' headcount and `pipmd_quantile` returns the "poverty line".
#'
#' The quantiles are calculated as function of the mean of the distribution
#' times an `x` factor. Basically, the quantile is `x` times the mean. By
#' default, the mean is equal to 1, which implies that, if no mean value if
#' provided, the return value is equal to `x`.
#'
#' @param welfare welfare vector
#' @param weight population weight vector
#' @param n numeric: number of equi-spaced quantiles
#' @param popshare numeric atomic vector: the quantiles to return. Will only be
#' used if `n = NULL`
#' @param format character: "dt", "list", "atomic", giving the format of the
#' output
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `quantile` and `value`.  Check `format` argument to change the output format.
#'
#' @export
#'
#'
#' @examples
#' # Example 1: Calculating quintiles.
#' pipmd_quantile(welfare = pip_md_s$welfare,
#'                weight  = pip_md_s$weight,
#'                n       = 5,
#'                format  = "list")
#'
#' # Example 2: Calculating deciles with data.table format.
#' pipmd_quantile(welfare = pip_md_s$welfare,
#'                weight  = pip_md_s$weight,
#'                n       = 10,
#'                format  = "dt")
#'
#' # Example 3: Calculating quantiles at specific population shares and format atomic.
#' specific_popshares <- seq(from = 0, to = 1, length.out = 100)
#' pipmd_quantile(welfare = pip_md_s$welfare,
#'                weight  = pip_md_s$weight,
#'                popshare = specific_popshares,
#'                format  = "atomic")
#'
pipmd_quantile <- function(
  welfare    ,
  weight     = rep(1, length = length(welfare)),
  n          = 10,
  popshare   = seq(from = 1/n, to = 1, by = 1/n),
  format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- wbpip::md_quantile_values(
    welfare    = welfare,
    weight     = weight,
    n          = n,
    popshare   = popshare,
    format     = format
  )

  # ____________________________________________________________________________
  # Format and Return ----------------------------------------------------------
  return(output)


}





#' Welfare share by quantile in micro data
#'
#' `pipmd_welfare_share_at` returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 be default), to estimate the corresponding
#' share of welfare in each.
#'
#' @inheritParams pipmd_quantile
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `quantile` and `share_at`.  Check `format` argument to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default quantiles (10)
#' pipmd_welfare_share_at(welfare = pip_md_s$welfare,
#'                        weight = pip_md_s$weight)
#'
#' # Example 2: Specifying a different number of quantiles
#' pipmd_welfare_share_at(welfare = pip_md_s$welfare,
#'                        weight = pip_md_s$weight,
#'                        n = 5,  # For quintiles
#'                        format = "list")
#'
#' # Example 3: Using specific population shares
#' specific_popshares <- seq(from = 0.1, to = 1, by = 0.1)  # Deciles
#' pipmd_welfare_share_at(welfare = pip_md_s$welfare,
#'                        weight = pip_md_s$weight,
#'                        popshare = specific_popshares,
#'                        format = "dt")
#'
#' # Example 4: Returning atomic format
#' pipmd_welfare_share_at(welfare = pip_md_s$welfare,
#'                        weight = pip_md_s$weight,
#'                        n = 4,  # For quartiles
#'                        format = "atomic")
#'
pipmd_welfare_share_at <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    n          = 10,
    popshare   = seq(from = 1/n, to = 1, by = 1/n),
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  output <- wbpip::md_welfare_share_at(
    welfare    = welfare,
    weight     = weight,
    n          = n,
    popshare   = popshare,
    format     = format
  )

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  return(output)

}






#' Quantile welfare share
#'
#' `pipmd_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that `pipmd_welfare_share_at` get the share of
#' welfare held by a particular share of the population, which is in a sense
#' the cumulative share. Instead, `pipmd_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' @inheritParams pipmd_quantile
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `quantile` and `share_at`.  Check `format` argument to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default quantiles (10)
#' pipmd_quantile_welfare_share(welfare = pip_md_s$welfare,
#'                              weight = pip_md_s$weight)
#'
#' # Example 2: Specifying a different number of quantiles
#' pipmd_quantile_welfare_share(welfare = pip_md_s$welfare,
#'                              weight = pip_md_s$weight,
#'                              n = 5,  # For quintiles
#'                              format = "list")
#'
#' # Example 3: Using specific population shares
#' specific_popshares <- seq(from = 0.1, to = 1, by = 0.1)  # Deciles
#' pipmd_quantile_welfare_share(welfare = pip_md_s$welfare,
#'                              weight = pip_md_s$weight,
#'                              popshare = specific_popshares,
#'                              format = "dt")
#' rm(specific_popshares)
#'
#' # Example 4: Returning atomic format
#' pipmd_quantile_welfare_share(welfare = pip_md_s$welfare,
#'                              weight = pip_md_s$weight,
#'                              n = 4,  # For quartiles
#'                              format = "atomic")
#'
pipmd_quantile_welfare_share <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    n          = 10,
    popshare   = seq(from = 1/n, to = 1, by = 1/n),
    format     = c("dt", "list", "atomic"))
  {
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()


  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  output <- wbpip::md_quantile_welfare_share(
    welfare    = welfare,
    weight     = weight,
    n          = n,
    popshare   = popshare,
    format     = format
  )

  # ____________________________________________________________________________
  # Return ---------------------------------------------------------------------
  return(output)

}




#' Gini coefficient
#'
#' Compute the Gini coefficient for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `pipmd_gini()` computes the Gini coefficient for the distribution.
#'
#' @inheritParams pipmd_quantile
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `indicator` and `value`. Check `format` argument to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic Gini coefficient calculation with default settings
#' pipmd_gini(welfare = pip_md_s$welfare,
#'            weight = pip_md_s$weight)
#'
#' # Example 2: Returning Gini coefficient in data.table format
#' pipmd_gini(welfare = pip_md_s$welfare,
#'            weight = pip_md_s$weight,
#'            format = "dt")
#'
#' # Example 3: Returning Gini coefficient as an atomic value
#' pipmd_gini(welfare = pip_md_s$welfare,
#'            weight = pip_md_s$weight,
#'            format = "atomic")
#'
#' # Example 4: Using equal weights (weight vector is NULL)
#' pipmd_gini(welfare = pip_md_s$welfare,
#'            format = "list")
#'
pipmd_gini <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    format  = c("dt", "list", "atomic"))
  {
  # _____________________________________
  # Arguments ---------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()

  # ______________________________________
  # Calculations -------------------------
  gn <- wbpip::md_compute_gini(
    welfare = welfare,
    weight  = weight
  )
  names(gn) <- "gini"

  # ________________________________________
  # Format & Return ------------------------
  return_format_md_dist(gn, name = "gini", format)

}




#' Wolfson polarization index
#'
#' Compute the Wolfson polarization index for microdata.
#'
#' Given a vector of income or consumption values and their respective weights
#' `pipmd_polarization()` computes the Wolfson polarization index.
#'
#' @inheritParams pipmd_quantile
#' @param gini numeric: gini coefficient. If NULL (default) then uses
#' [pipmd_gini] to calculate the gini.
#' @param mean numeric: weighted welfare mean. Default is NULL.
#' @param median numeric: weighted welfare mean. Default is NULL.
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `indicator` and `value`. Check `format` argument to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic polarization calculation with default settings
#' pipmd_polarization(welfare = pip_md_s$welfare,
#'                    weight = pip_md_s$weight)
#'
#' # Example 2: Specifying Gini coefficient, mean, and median
#' custom_gini <- pipmd_gini(welfare = pip_md_s$welfare,
#'                           weight = pip_md_s$weight,
#'                           format = "atomic")
#' custom_mean <- mean(pip_md_s$welfare)
#' custom_median <- median(pip_md_s$welfare)
#' pipmd_polarization(welfare = pip_md_s$welfare,
#'                    weight = pip_md_s$weight,
#'                    gini = custom_gini,
#'                    mean = custom_mean,
#'                    median = custom_median,
#'                    format = "list")
#' rm(custom_gini, custom_mean, custom_median)
#'
#' # Example 3: Returning polarization index in data.table format
#' pipmd_polarization(welfare = pip_md_s$welfare,
#'                    weight = pip_md_s$weight,
#'                    format = "dt")
#'
#' # Example 4: Using equal weights (weight vector is NULL)
#' pipmd_polarization(welfare = pip_md_s$welfare,
#'                    format = "atomic")
#'
pipmd_polarization <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    gini    = NULL,
    mean    = NULL,
    median  = NULL,
    format  = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()

  if (is.null(gini)) {
    gini <- pipmd_gini(
      welfare = welfare,
      weight  = weight,
      format  = "atomic"
    )
  }
  if (is.null(mean)) {
    mean <- fmean(x = welfare, w = weight)
  }
  if (is.null(median)) {
    median <- fmedian(
      x     = welfare,
      w     = weight)
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  p <- wbpip::md_compute_polarization(
    welfare = welfare,
    weight  = weight,
    gini    = gini,
    mean    = mean,
    median  = median
  )
  names(p) <- "polarization"

  # ____________________________________________________________________________
  # Format & Return ------------------------------------------------------------
  return_format_md_dist(p, name = "polarization", format)

}



#' Mean Log Deviation
#'
#' Given a vector of weights and welfare, this functions computes the
#' Mean Log Deviation (MLD).
#'
#'
#' @inheritParams pipmd_polarization
#'
#' @return Returns a `data.table` and `data.frame` object with two variables:
#' `indicator` and `value`. Check `format` argument to change the output format.
#'
#' @export
#'
#' @examples
#' # Example 1: Basic usage with default settings
#' pipmd_mld(welfare = pip_md_s$welfare,
#'           weight = pip_md_s$weight)
#'
#' # Example 2: Specifying mean and returning as a list
#' custom_mean <- mean(pip_md_s$welfare)
#' pipmd_mld(welfare = pip_md_s$welfare,
#'           weight = pip_md_s$weight,
#'           mean = custom_mean,
#'           format = "list")
#'
#' # Example 3: Returning MLD as an atomic value
#' pipmd_mld(welfare = pip_md_s$welfare,
#'           weight = pip_md_s$weight,
#'           format = "atomic")
#'
#' # Example 4: Using equal weights (weight vector is NULL)
#' pipmd_mld(welfare = pip_md_s$welfare,
#'           format = "list")
#'
pipmd_mld <- function(
    welfare    ,
    weight     = rep(1, length = length(welfare)),
    mean    = NULL,
    format  = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  format <- match.arg(format)

  # defenses ---------
  check_pipmd_dist()

  if (is.null(mean)) {
    mean <- fmean(x = welfare,w = weight)
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  p <- wbpip::md_compute_mld(
    welfare = welfare,
    weight  = weight,
    mean    = mean
  )
  names(p) <- "mld"

  # ____________________________________________________________________________
  # Format & Return ------------------------------------------------------------
  return_format_md_dist(p, name = "mld", format)

}














