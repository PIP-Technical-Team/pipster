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
  if ("pipster_object" %in% nlp) {
    if (!is.null(lp$pipster_object) && !inherits(lp$pipster_object, "pipster")) {
      cli::cli_abort(c("argument {.field pipster_object} must be of
                       class {.code pipster}.",
                       "It should be created using {.fun create_pipster_object}"))
    }
  }

  if (!"pipster_object" %in% nlp) {
    ## povline and povshare ----------
    if ( all(c("povline", "povshare") %in% nlp)) {
      if (!is.na(lp$povline) && !is.null(lp$povshare)) {
        cli::cli_abort("You must specify either {.field povline} or
                {.field povshare}")
      }
    }
  }

  ## welfare and params -----------
  if ( all(c("pipster_object", "welfare") %in% nlp)) {
    if (!is.null(lp$pipster_object) &&
        (!is.null(lp$welfare)  || !is.null(lp$weight))) {
      cli::cli_abort("You must specify either {.field pipster_object} or
                {.field welfare} and {.field weight}")
    }
  }

  if ("popshare" %in% nlp) {
    if (any(lp$popshare <= 0)) {
      cli::cli_abort("All values in {.arg popshare} must be positve")
    }
  }
  if ("povshare" %in% nlp) {
    if (any(lp$povshare <= 0)) {
      cli::cli_abort("All values in {.arg povshare} must be positve")
    }
  }

  if ("popshare" %in% nlp) {
    if (any(lp$popshare > 1)) {
      cli::cli_abort("No values in {.arg popshare} can be >1")
    }
  }
  if ("povshare" %in% nlp) {
    if (any(lp$povshare > 1)) {
      cli::cli_abort("No values in {.arg povshare} can be >1")
    }
  }

  # if (all(c("n", "pipster_object", "popshare") %in% nlp)) {
  #   if (is.null(c(lp$n, lp$pipster_object, lp$popshare))) {
  #     cli::cli_abort("You must either in {.arg povshare} can be >1")
  #   }
  # }

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
check_pipmd_pov <- function() {
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



check_pipmd_dist <- function() {
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
    if (is.null(weight)) {
      weight <- rep(1, length = length(welfare))
      cli::cli_alert_warning(
        text = "No weight vector specified, each observation assigned equal weight"
      )
    }

    if (exists("n", inherits = FALSE) &
        exists("popshare", inherits = FALSE)) {
      if (is.null(n) & is.null(popshare)) {
        cli::cli_abort("Either {.arg n} or {.arg popshare} must be defined")
      }
    }
  })
  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(TRUE))
}
