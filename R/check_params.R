#' Check parameters of pipgd functions
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

  if ("popshare" %in% nlp) {
    if (any(lp$popshare <= 0)) {
      cli::cli_abort("All values in {.arg popshare} must be positve")
    }
  }

  if ("popshare" %in% nlp) {
    if (any(lp$popshare > 1)) {
      cli::cli_abort("No values in {.arg popshare} can be >1")
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


#' Check parameters of pipmd functions
#'
#'
#' @return invisible TRUE
#' @keywords internal
check_pipmd_params <- function() {
  lp <- parent.frame()  |>
    as.list()

  with(lp, {
    if (is.na(welfare) |> any()) {
      cli::cli_abort("No elements in welfare vector can be NA")
    }
    if (!is.numeric(welfare)) {
      cli::cli_abort("welfare must be numeric")
    }

    if (length(weight) > 1 & any(is.na(weight))) {
      cli::cli_abort("No elements in weight vector can be NA - make NULL to use equal weighting")
    }

    if (is.null(povline) || !is.numeric(povline)) {
      cli::cli_abort(
        text = "A numeric poverty line must be specified"
      )
    }

    if (povline < min(welfare) || povline > max(welfare)) {
      cli::cli_alert_info(
        text = "Note: specified poverty line is not within the welfare range"
      )
    }
  })

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(TRUE))

}



