#' Get Lorenz Parameters
#'
#' Get Parameters and key values derived from the quadratic and Beta Lorenz
#' parametrization. `welfare` and `population` must be vectors of a group data
#' dataset
#'
#' @param welfare numeric: cumulative sahre of welfare (income/consumption)
#' @param population numeric: cumulative share of the population
#'
#' @return list with Group data parameters parameters
#' @export
#'
#' @examples
#' # Get Lorenz parameters
#' res <- pipgd_params(
#'   welfare = pip_gd$L,
#'   population = pip_gd$P)
#' str(res)
pipgd_params <- function(welfare,
                         population) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  # create results list
  l_res <- vector(mode = "list", length = 2)
  names(l_res) <- c("gd_params", "data")


  # Apply Lorenz quadratic fit ----------------------------------------------

  ## STEP 1: Prep data to fit functional form-------------
  functional_form_lq <-
    wbpip:::create_functional_form_lq(welfare    = welfare,
                                     population = population)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- wbpip:::regres(functional_form_lq, is_lq = TRUE)
  names(reg_results_lq$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lq$reg_results <- reg_results_lq


  ## STEP 3: get key values
  # Compute key numbers from Lorenz quadratic form
  kv <- wbpip::gd_lq_key_values(reg_results_lq$coef[["A"]],
                         reg_results_lq$coef[["B"]],
                         reg_results_lq$coef[["C"]])

  l_res$gd_params$lq$key_values <- kv


  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  functional_form_lb <-
    wbpip:::create_functional_form_lb(welfare    = welfare,
                              population = population)

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- wbpip:::regres(functional_form_lb, is_lq = FALSE)
  names(reg_results_lb$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lb$reg_results <- reg_results_lb

  l_res$gd_params$lb$key_values <- NA

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  l_res$data$welfare    <- welfare
  l_res$data$population <- population

  return(l_res)

}




#' Check parameters of get_gd functions
#'
#' @param lp list of parameters
#'
#' @return invisible TRUE
#' @keywords internal
check_pipgd_params <- function(lp) {


  #   ____________________________________________________________________________
  #   Computations                                                            ####

  nlp <- names(lp)

  ## welfare -----------


  ## welfare and params -----------
  if ( all(c("params", "welfare") %in% nlp)) {
    if (!is.null(lp$params) &&
        (!is.null(lp$welfare)  || !is.null(lp$population))) {
      cli_abort("You must specify either {.field params} or
                {.field welfare} and {.field population}")
    }
  }


  ## povline and popshare ----------
  if ( all(c("povline", "popshare") %in% nlp)) {
    if (!is.na(lp$povline) && !is.null(lp$popshare)) {
      cli_abort("You must specify either {.field povline} or
                {.field popshare}")
    }
  }


  # "Either `params` or `welfare` and `population` should be spefied" =
  #   (is.null(params) && !is.null(welfare) && !is.null(population)) ||
  #   (!is.null(params) && is.null(welfare) && is.null(population))
  #
  # "`params` should be a list from `get_gd_lorenz_params()`" =
  #   is.list(params) || is.null(params)
  #
  # "`complete` must be logical" =
  #   is.logical(complete)

  ## lorenz -----------
  if ( all(c("lorenz") %in% nlp)) {

    if (!is.null(lp$lorenz) && !lp$lorenz %in% c("lq", "lb")) {

      cli_abort("{.field lorenz} must be either 'lq' or 'lb', or
                {.code NULL} to let the algorithm select")
    }
  }


  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(TRUE))

}