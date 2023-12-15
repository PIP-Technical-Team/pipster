#' Check validity of Lorenz Curve
#'
#' @inheritParams pipgd_validate_lorenz
#' @param params list of parameters from `pipgd_validate_lorenz()`
#' @param complete logical: If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#' @param mean numeric: welfare mean of distribution.
#' @param povline numeric: value of poverty line. Default is the `mean` value
#' @param popshare numeric: range (0,1). Share of population. Provide share of
#'   population instead of poverty line
#'
#'
#' @return list of distributional validity of each Lorenz model
#' @export
#'
#' @examples
#' # Using Lorenz parameters from pipgd_validate_lorenz
#'  res <-
#' pipgd_params(welfare = pip_gd$L,
#'              weight = pip_gd$P) |>
#'   pipgd_validate_lorenz() |>
#'   pipgd_select_lorenz()
#'
#' # Using welfare and population vecotrs
#' res2 <- pipgd_select_lorenz(welfare = pip_gd$L,
#'                             weight = pip_gd$P)
#' identical(res, res2)
pipgd_validate_lorenz <-
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           complete   = getOption("pipster.return_complete")
  ) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)


  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- pipgd_params(welfare = welfare,
                           weight  = weight)
  }

  if (!is.null(popshare)) {
    povline_lq <- mean * wbpip::derive_lq(popshare,
                                   params$gd_params$lq$reg_results$coef[["A"]],
                                   params$gd_params$lq$reg_results$coef[["B"]],
                                   params$gd_params$lq$reg_results$coef[["C"]])

    povline_lb <- mean * wbpip::derive_lb(popshare,
                                   params$gd_params$lb$reg_results$coef[["A"]],
                                   params$gd_params$lb$reg_results$coef[["B"]],
                                   params$gd_params$lb$reg_results$coef[["C"]])

  } else {
    povline_lb <- povline_lq <- povline

  }

  # Validity or LQ
  validity_lq <- wbpip:::check_curve_validity_lq(
    params$gd_params$lq$reg_results$coef[["A"]],
    params$gd_params$lq$reg_results$coef[["B"]],
    params$gd_params$lq$reg_results$coef[["C"]],
    params$gd_params$lq$key_values$e,
    params$gd_params$lq$key_values$m,
    params$gd_params$lq$key_values$n,
    params$gd_params$lq$key_values$r^2)

  headcount_lq <- wbpip:::gd_compute_headcount_lq(mean,
                                          povline_lq,
                                          params$gd_params$lq$reg_results$coef[["B"]],
                                          params$gd_params$lq$key_values$m,
                                          params$gd_params$lq$key_values$n,
                                          params$gd_params$lq$key_values$r)

  validity_lq$headcount <- headcount_lq

  # Validity of LB
  # Compute poverty stats
  headcount_lb <- wbpip:::gd_compute_headcount_lb(mean,
                                          povline_lb,
                                          params$gd_params$lb$reg_results$coef[["A"]],
                                          params$gd_params$lb$reg_results$coef[["B"]],
                                          params$gd_params$lb$reg_results$coef[["C"]])

  # Check validity
  validity_lb <-
    wbpip:::check_curve_validity_lb(headcount = headcount_lb,
                            params$gd_params$lb$reg_results$coef[["A"]],
                            params$gd_params$lb$reg_results$coef[["B"]],
                            params$gd_params$lb$reg_results$coef[["C"]])

  validity_lb$headcount <- headcount_lb

  if ( povline_lb != mean*times_mean) {
    times_mean <- povline_lb/mean
  }

  norm_lb_label <- paste0("Normality with a mean of ", mean,
                          " and a poverty line of ", povline_lb,
                          ";", times_mean, " times the mean.")

  attr(validity_lb$is_normal, "label") <- norm_lb_label


  #   __________________________________________________________________
  #   Return                                                          ####

  if (isFALSE(complete))
    params <- vector("list")

  params$gd_params$lq$validity <- validity_lq
  params$gd_params$lb$validity <- validity_lb

  return(params)

}


#' Get selected Lorenz curve for distributional stats
#'
#' @inheritParams pipgd_params
#' @inheritParams pipgd_validate_lorenz
#' @param params list of parameters from `get_gd_lorenz_validity()`
#'
#' @return list of values with best lorenz fit for distributional Stats
#' @export
#'
#' @examples
#' # Using Lorenz parameters from get_gd_lorenz_params
#' withr::local_options(pipster.return_complete  = TRUE)
#' params <- get_gd_lorenz_params(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
#'
#' params <- get_gd_lorenz_validity(
#'   params = params,
#'   complete = TRUE)
#' pipgd_select_lorenz(params = params)
#'
#' # Using Lorenz parameters from get_gd_lorenz_validity
#' params <- get_gd_lorenz_validity(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P,
#'   complete = TRUE)
#' pipgd_select_lorenz(params = params)
#'
#' # Using original vectors
#'
#' pipgd_select_lorenz(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
pipgd_select_lorenz <-
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           complete   = getOption("pipster.return_complete")) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- pipgd_validate_lorenz(welfare = welfare,
                                    weight = weight,
                                    complete   = TRUE,
                                    mean       = mean,
                                    times_mean = times_mean,
                                    povline    = povline,
                                    popshare   = popshare)
  }

  ## Selected Lorenz for  Distribution-------
  lq <- append(params$gd_params$lq$validity,
               params$gd_params$lq$reg_results["sse"])

  lb <- append(params$gd_params$lb$validity,
               params$gd_params$lb$reg_results["sse"])

  use_lq_for_dist <-
    wbpip:::use_lq_for_distributional(lq,lb)

  ## Selected Lorenz for Poverty -----------

  fit_lb <- wbpip:::gd_compute_fit_lb(params$data$welfare,
                              params$data$weight,
                              params$gd_params$lb$validity$headcount,
                              params$gd_params$lb$reg_results$coef[["A"]],
                              params$gd_params$lb$reg_results$coef[["B"]],
                              params$gd_params$lb$reg_results$coef[["C"]])

  fit_lq <- wbpip:::gd_compute_fit_lq(params$data$welfare,
                              params$data$weight,
                              params$gd_params$lq$validity$headcount,
                              params$gd_params$lb$reg_results$coef[["A"]],
                              params$gd_params$lb$reg_results$coef[["B"]],
                              params$gd_params$lb$reg_results$coef[["C"]])

  lq <- append(lq,
               fit_lq["ssez"])
  lb <- append(lb,
               fit_lb["ssez"])


  use_lq_for_pov <- wbpip:::use_lq_for_poverty(lq, lb)

  l_res <- list(for_dist = ifelse(use_lq_for_dist, "lq", "lb"),
                for_pov  = ifelse(use_lq_for_pov, "lq", "lb"),
                use_lq_for_dist = use_lq_for_dist,
                use_lq_for_pov  = use_lq_for_pov)

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$selected_lorenz <- l_res
  return(params)

}

# to develop
pipgd_lorenz_curve <- function(){
  return(TRUE)
}
