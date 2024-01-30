#' Check validity of Lorenz Curve
#'
#' @inheritParams pipgd_params
#' @param params list of parameters from `pipgd_params()`
#' @param complete logical: If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#' @param mean numeric: welfare mean of distribution.
#' @param povline numeric: value of poverty line. Default is the `mean` value
#' @param popshare numeric: range (0,1). Share of population. Provide share of
#'   population instead of poverty line
#' @param times_mean numeric factor that multiplies the mean to create a
#'   relative poverty line. Default is 1
#'
#'
#' @return Returns a nested list of distributional validity of each Lorenz model
#' accessible at `$gd_params$lq$validity$is_normal` for the Lorenz beta and
#' `$gd_params$lq$validity$is_normal` for the Lorenz quadratic.
#'
#' @export
#'
#' @examples
#' # Example 1: Validate Lorenz Curves using pre-calculated parameters.
#' parameters <- pipgd_params(welfare = pip_gd$L, weight = pip_gd$P)
#' pipgd_validate_lorenz(params = parameters)
#'
#' # Example 2: Directly using welfare and weight vectors.
#' pipgd_validate_lorenz(welfare = pip_gd$L,
#'                       weight = pip_gd$P)
#'
#' # Example 3: Specifying mean and poverty line
#' pipgd_validate_lorenz(welfare = pip_gd$L,
#'                       weight = pip_gd$P,
#'                       mean = mean(pip_gd$X),
#'                       povline = 50)
#'
#' # Example 4: Using a custom population share
#' pipgd_validate_lorenz(welfare = pip_gd$L,
#'                       weight = pip_gd$P,
#'                       popshare = 0.5)
#'
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
  if (!is.null(welfare) & !is.null(weight)) {
    params <- pipgd_params(welfare = welfare,
                           weight  = weight)
  } else if (is.null(params$gd_params$lq$reg_results$coef)) {
    stop(
      "Either `welfare` and `weights` should be specified or `params` should be output from `pipster::pipgd_params()`"
    )
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

  # Validity of LQ
  validity_lq <- wbpip::check_curve_validity_lq(
    params$gd_params$lq$reg_results$coef[["A"]],
    params$gd_params$lq$reg_results$coef[["B"]],
    params$gd_params$lq$reg_results$coef[["C"]],
    params$gd_params$lq$key_values$e,
    params$gd_params$lq$key_values$m,
    params$gd_params$lq$key_values$n,
    params$gd_params$lq$key_values$r^2)

  headcount_lq <- wbpip::gd_compute_headcount_lq(mean,
                                          povline_lq,
                                          params$gd_params$lq$reg_results$coef[["B"]],
                                          params$gd_params$lq$key_values$m,
                                          params$gd_params$lq$key_values$n,
                                          params$gd_params$lq$key_values$r)

  validity_lq$headcount <- headcount_lq

  # Validity of LB
  # Compute poverty stats
  headcount_lb <- wbpip::gd_compute_headcount_lb(mean,
                                          povline_lb,
                                          params$gd_params$lb$reg_results$coef[["A"]],
                                          params$gd_params$lb$reg_results$coef[["B"]],
                                          params$gd_params$lb$reg_results$coef[["C"]])

  # Check validity
  validity_lb <-
    wbpip::check_curve_validity_lb(headcount = headcount_lb,
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
#' @param params list of parameters from `pipgd_validate_lorenz()`
#' @param mean numeric scalar of distribution mean. Default is 1
#'
#' @return Returns a list of values with the best lorenz fit given the
#' distributional Stats.
#'
#' @export
#'
#' @examples
#' # Example 1: Directly using welfare and weight vectors.
#' pipgd_select_lorenz(welfare = pip_gd$L,
#'                     weight = pip_gd$P)
#'
#' # Example 2: Specifying mean and poverty line.
#' custom_mean <- sum(pip_gd$W * pip_gd$X) / sum(pip_gd$W)
#' pipgd_select_lorenz(welfare = pip_gd$L,
#'                     weight = pip_gd$P,
#'                     mean = custom_mean,
#'                     povline = 1.25)
#'
#'
#' # Example 3.1: Using parameters from pipgd_validate_lorenz()
#' validated_parameters <- pipgd_validate_lorenz(welfare = pip_gd$L,
#'                                               weight = pip_gd$P,
#'                                               complete = TRUE)
#' pipgd_select_lorenz(params = validated_parameters)
#'
#'
#' # Example 3.2: Piping from from pipgd_params |> pipgd_validate_lorenz()
#' pipgd_params(welfare = pip_gd$L,
#'              weight = pip_gd$P) |>
#' pipgd_validate_lorenz(complete = TRUE)|>
#' pipgd_select_lorenz()
#'
#' # Example 4: Detailed output with complete = TRUE
#' pipgd_select_lorenz(welfare = pip_gd$L,
#'                     weight = pip_gd$P,
#'                     complete = TRUE)
#'
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
    params <- pipgd_validate_lorenz(welfare    = welfare,
                                    weight     = weight,
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
    wbpip::use_lq_for_distributional(lq,lb)

  ## Selected Lorenz for Poverty -----------

  fit_lb <- wbpip::gd_compute_fit_lb(params$data$welfare,
                              params$data$weight,
                              params$gd_params$lb$validity$headcount,
                              params$gd_params$lb$reg_results$coef[["A"]],
                              params$gd_params$lb$reg_results$coef[["B"]],
                              params$gd_params$lb$reg_results$coef[["C"]])

  fit_lq <- wbpip::gd_compute_fit_lq(params$data$welfare,
                              params$data$weight,
                              params$gd_params$lq$validity$headcount,
                              params$gd_params$lb$reg_results$coef[["A"]],
                              params$gd_params$lb$reg_results$coef[["B"]],
                              params$gd_params$lb$reg_results$coef[["C"]])

  lq <- append(lq,
               fit_lq["ssez"])
  lb <- append(lb,
               fit_lb["ssez"])


  use_lq_for_pov <- wbpip::use_lq_for_poverty(lq, lb)

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





#' Lorenz curve
#'
#' Returns the Lorenz curve. User provides the cumulative welfare and
#' cumulative weight, as well as the number of points on the lorenz curve required.
#' By default, the best fitting Lorenz parameterization (quadratic or beta) is
#' selected.
#'
#' @inheritParams pipgd_pov_headcount_nv
#' @param n_bins atomic double vector of length 1: number of points on the
#' lorenz curve
#'
#' @return Returns a list which contains:
#'  * numeric lorenz curve,
#'  * corresponding points on x-axis,
#'  * whether lq or lb parameterization, and
#'  * if `complete=TRUE`, also returns all params.
#'
#' @export
#'
#' @examples
#' # Example 1: Generating a Lorenz Curve with default settings
#' pipgd_lorenz_curve(welfare = pip_gd$L,
#'                    weight = pip_gd$P)
#'
#' # Example 2: Specifying the number of bins for the Lorenz Curve
#' pipgd_lorenz_curve(welfare = pip_gd$L,
#'                    weight = pip_gd$P,
#'                    n_bins = 50)
#'
#' # Example 3: Using pre-calculated parameters
#' validated_parameters <- pipgd_validate_lorenz(welfare = pip_gd$L,
#'                                               weight = pip_gd$P)
#' pipgd_lorenz_curve(params = validated_parameters)
#'
#'
#' # Example 4: Generating Lorenz Curve with a specific Lorenz model(e.g. Lorenz beta)
#' pipgd_lorenz_curve(params = validated_parameters,
#'                    lorenz = "lb")
#'
#'
pipgd_lorenz_curve <- function(
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
    lorenz     = NULL,
    n_bins     = 100
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
  #   Lorenz Calculations
  #   _________________________________________________________________

  x_vec <- seq(from = 0, to = 1, length.out = n_bins)

  if (lorenz == "lb") {


    lc <- wbpip:::value_at_lb(
      x = x_vec,
      A = params$gd_params$lb$reg_results$coef[["A"]],
      B = params$gd_params$lb$reg_results$coef[["B"]],
      C = params$gd_params$lb$reg_results$coef[["C"]]
    )

  } else if (lorenz == "lq") {

    lc <- sapply(
      X   = x_vec,
      FUN = function(x1){
        wbpip:::value_at_lq(
          x = x1,
          A = params$gd_params$lq$reg_results$coef[["A"]],
          B = params$gd_params$lq$reg_results$coef[["B"]],
          C = params$gd_params$lq$reg_results$coef[["C"]]
        )

      }
    )

  }

  attributes(lc) <- NULL

  #   _________________________________________________________________
  #   Return
  #   _________________________________________________________________
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$lorenz_curve$output <- lc
  params$lorenz_curve$points <- x_vec
  params$lorenz_curve$lorenz <- lorenz

  return(
    params
  )

}


# # Example 1:
# # !!!! This example is not working
# # Using Lorenz parameters from pipgd_validate_lorenz
#
#  res <-
# pipgd_params(welfare = pip_gd$L,
#              weight = pip_gd$P) |>
#   pipgd_validate_lorenz() |>
#   pipgd_select_lorenz()
#
# # Using welfare and population vectors
# res2 <- pipgd_select_lorenz(welfare = pip_gd$L,
#                             weight = pip_gd$P)
# identical(res, res2)








