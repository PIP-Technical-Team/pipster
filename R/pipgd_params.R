#' Get Group Data Parameters
#'
#' Get Parameters and key values derived from the quadratic and Beta Lorenz
#' parametrization. `welfare` and `population` must be vectors of a group data
#' dataset
#'
#' @param welfare numeric vector of cumulative share of welfare (income/consumption)
#' @param weight numeric vector of cumulative share of the population
#' @param mean numeric scalar of distribution mean. Default is NULL
#' @param population numeric scalar with actual size of population. Default is NULL
#'
#' @return list with Group data parameters parameters
#' @export
#' @references
#' Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://ageconsearch.umn.edu/record/94862/)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' Krause, M. 2013. "[Corrigendum to Elliptical Lorenz
#' curves](https://doi.org/10.1016/j.jeconom.2013.01.001)". *Journal of
#' Econometrics 174* (1): 44.
#'
#' Villasenor, J., B. C. Arnold. 1989. "[Elliptical Lorenz
#' curves](https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338)".
#' *Journal of Econometrics 40* (2): 327-338.
#'
#' @examples
#' # Get Lorenz parameters
#' res <- pipgd_params(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
#' str(res)
pipgd_params <- function(welfare,
                         weight,
                         mean = NULL,
                         population = NULL) {

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
                                     population = weight)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- wbpip:::regres(functional_form_lq, is_lq = TRUE)
  names(reg_results_lq$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lq$reg_results <- reg_results_lq


  ## STEP 3: get key values
  # Compute key numbers from Lorenz quadratic form
  kv <- wbpip:::gd_lq_key_values(reg_results_lq$coef[["A"]],
                         reg_results_lq$coef[["B"]],
                         reg_results_lq$coef[["C"]])

  l_res$gd_params$lq$key_values <- kv


  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  functional_form_lb <-
    wbpip:::create_functional_form_lb(welfare    = welfare,
                                      population = weight)

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- wbpip:::regres(functional_form_lb, is_lq = FALSE)
  names(reg_results_lb$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lb$reg_results <- reg_results_lb

  l_res$gd_params$lb$key_values <- NA

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  l_res$data$welfare    <- welfare
  l_res$data$weight     <- weight
  l_res$data$mean       <- mean
  l_res$data$population <- population
  class(l_res) <- "pipgd_params"
  l_res
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

  ## params --------------------
  if ("params" %in% nlp) {
    if (!is.null(lp$params) && !inherits(lp$params, "pipgd_params")) {
      cli::cli_abort(c("argument {.field params} must be of
                       class {.code pipgd_params}.",
                       "It should be created using {.fun pipgd_params}"))
    }
  }

  ## welfare -----------


  ## welfare and params -----------
  if ( all(c("params", "welfare") %in% nlp)) {
    if (!is.null(lp$params) &&
        (!is.null(lp$welfare)  || !is.null(lp$population))) {
      cli::cli_abort("You must specify either {.field params} or
                {.field welfare} and {.field population}")
    }
  }


  ## povline and popshare ----------
  if ( all(c("povline", "popshare") %in% nlp)) {
    if (!is.na(lp$povline) && !is.null(lp$popshare)) {
      cli::cli_abort("You must specify either {.field povline} or
                {.field popshare}")
    }
  }


  # "Either `params` or `welfare` and `population` should be spefied" =
  #   (is.null(params) && !is.null(welfare) && !is.null(population)) ||
  #   (!is.null(params) && is.null(welfare) && is.null(population))
  #
  # "`params` should be a list from `pipgd_validate_lorenz()`" =
  #   is.list(params) || is.null(params)
  #
  # "`complete` must be logical" =
  #   is.logical(complete)

  ## lorenz -----------
  if ( all(c("lorenz") %in% nlp)) {

    if (!is.null(lp$lorenz) && !lp$lorenz %in% c("lq", "lb")) {

      cli::cli_abort("{.field lorenz} must be either 'lq' or 'lb', or
                {.code NULL} to let the algorithm select")
    }
  }


  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(TRUE))

}
