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
#' @return quantiles: see `format`
#' @export
#'
#' @examples
#' pipmd_quantile(
#'   welfare = pip_md_s$welfare
#'   weight  = pip_md_s$weight,
#'   n       = 5
#' )
pipmd_quantile <- function(
  welfare    = NULL,
  weight     = NULL,
  n          = 10,
  popshare   = seq(from = 1/n, to = 1, by = 1/n),
  format     = c("dt", "list", "atomic")
){

  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(weight)) {
    weight <- rep(1, length = length(welfare))
    cli::cli_alert_warning(
      text = "No weight vector specified, each observation assigned equal weight"
    )
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  format <- match.arg(format)


  # ____________________________________________________________________________
  # Specify Quantiles ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  q <- fquantile(
    x     = welfare,
    w     = weight,
    probs = popshare
  )

  # ____________________________________________________________________________
  # Format and Return ----------------------------------------------------------
  if (format == "atomic") {
    return(q)
  } else if (format == "dt") {
    q <- data.table::data.table(
      quantile = paste0("q_", names(q)),
      values   = q |> as.numeric()
    )
    return(q)
  } else if (format == "list") {
    return(
      as.list(q)
    )
  }


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
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' pipmd_welfare_share_at(welfare = pip_md_s$welfare,
#'                         weight = pip_md_s$weight)
pipmd_welfare_share_at <- function(
    welfare    = NULL,
    weight     = NULL,
    n          = 10,
    popshare   = seq(from = 1/n, to = 1, by = 1/n),
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(weight)) {
    weight <- rep(1, length = length(welfare))
    cli::cli_alert_warning(
      text = "No weight vector specified, each observation assigned equal weight"
    )
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  format <- match.arg(format)

  # ____________________________________________________________________________
  # Specify Quantiles ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }
  weight  <- weight[order(welfare)]
  welfare <- welfare[order(welfare)]
  q       <- pipmd_quantile(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    popshare = popshare,
    format   = "list"
  )
  total_weight <- sum(weight)
  output <- lapply(
    q,
    function(x){
      share <- weight[welfare <= x]
      share <- sum(share)/total_weight
      return(share)
    }
  )

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(output)
  } else if (format == "atomic") {
    return(
      output |> unlist()
    )
  } else if (format == "dt") {
    output <- data.table::data.table(
      quantile   = paste0("q_", names(output)),
      share_at   = output |> as.numeric()
    )
    return(output)
  }

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
#' @return list with vector of share of welfare by quantiles
#' @export
#'
#' @examples
#' pipmd_quantile_welfare_share(welfare = pip_md_s$welfare,
#'                              weight = pip_md_s$weight)
pipmd_quantile_welfare_share <- function(
    welfare    = NULL,
    weight     = NULL,
    n          = 10,
    popshare   = seq(from = 1/n, to = 1, by = 1/n),
    format     = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(weight)) {
    weight <- rep(1, length = length(welfare))
    cli::cli_alert_warning(
      text = "No weight vector specified, each observation assigned equal weight"
    )
  }
  if (is.null(n) & is.null(popshare)) {
    cli::cli_abort("Either `n` or `popshare` must be non-NULL")
  }
  format <- match.arg(format)


  # ____________________________________________________________________________
  # Specify Quantiles ----------------------------------------------------------
  if (!is.null(n)) {
    popshare <- seq(from = 1/n, to = 1, by = 1/n)
  }
  weight  <- weight[order(welfare)]
  welfare <- welfare[order(welfare)]
  output  <- pipmd_quantile(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    popshare = popshare,
    format   = "atomic"
  )

  # ____________________________________________________________________________
  # Format & Return -------------------------------------------------------------
  if (format == "list") {
    return(output |> as.list())
  } else if (format == "atomic") {
    return(output)
  } else if (format == "dt") {
    output <- data.table::data.table(
      quantile   = paste0("q_", names(output)),
      share_at   = output |> as.numeric()
    )
    return(output)
  }

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
#' @return gini
#' @export
#'
#' @examples
#' pipmd_gini(welfare = pip_md_s$welfare,
#'            weight = pip_md_s$weight)
pipmd_gini <- function(
    welfare = NULL,
    weight  = NULL,
    format  = c("dt", "list", "atomic")
){
  # ____________________________________________________________________________
  # Arguments ------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (is.null(welfare)) {
    cli::cli_abort("Welfare vector cannot be NULL")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
  }
  if (is.null(weight)) {
    weight <- rep(1, length = length(welfare))
    cli::cli_alert_warning(
      text = "No weight vector specified, each observation assigned equal weight"
    )
  }

  # ____________________________________________________________________________
  # Calculations ---------------------------------------------------------------
  gn <- wbpip::md_compute_gini(
    welfare = welfare,
    weight  = weight
  )
  names(gn) <- "gini"

  # ____________________________________________________________________________
  # Format & Return ------------------------------------------------------------
  if (format == "list") {
    return(gn |> as.list())
  } else if (format == "atomic") {
    return(gn)
  } else if (format == "dt") {
    gn <- data.table::data.table(
      indicator = "gini",
      value     = gn
    )
    return(gn)
  }
}




















#
#
# md_compute_quantiles # to compute specified quantiles
#


#
#
#
# # welfare at xth percentile
# return(list(
#   mean = mean, # no vectorization
#   median = median,
#   gini = gini,
#   polarization = polarization,
#   mld = mld,
#   quantiles = quantiles[["quantiles"]]
# ))


