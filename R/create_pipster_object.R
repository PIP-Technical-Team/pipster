
#' Create Pipster Object
#'
#' The first step in using the `pipster` package is to create a pipster object.
#' It creates the appropriate classes for welfare and weights, reformats the
#' data if necessary, and, for grouped data, it estimates and selects the lorenz
#' curves to be used for the estimation of poverty and distributional measures.
#'
#' @param welfare numeric: welfare vector
#' @param weight numeric: weight vector
#' @param imputation_id numeric: vector of ids for multiply imputed data.
#' Default is NULL
#'
#' @return list: pipster object containing welfare and weights,
#' params if grouped data, imputation_id if imputed data
#' @export
#'
#' @examples
#' p <- create_pipster_object(welfare = pip_gd$L,
#'                            weight  = pip_gd$P)
#' p
create_pipster_object <-
  function(welfare,
           weight         = rep(1, length(welfare)),
           mean           = 1,
           times_mean     = 1,
           n              = 10,
           povshare       = NULL,
           popshare       = seq(from = 1/n, to = 1, by = 1/n),
           povline        = ifelse(is.null(povshare),
                                   mean*times_mean,
                                   NA_real_),
           lorenz         = NULL,
           #complete       = getOption("pipster.return_complete"),
           imputation_id  = NULL) {

  #_____________________________________________________________________________
  # Arguments-------------------------------------------------------------------
  if (is.na(welfare) |> any()) {
    cli::cli_abort("No elements in welfare vector can be NA")
  }
  if (!is.numeric(welfare)) {
    cli::cli_abort("welfare must be numeric")
  }
  if (length(weight) > 1 & any(is.na(weight))) {
    cli::cli_abort("No elements in weight vector can be NA -
                   leave argument empty to give equal weighting")
  }

  #_____________________________________________________________________________
  # Class-----------------------------------------------------------------------
  tp <- identify_pip_type(welfare       = welfare,
                          weight        = weight,
                          imputation_id = imputation_id)
  cl <- substr(tp, start = 1, stop = 2)

  #_____________________________________________________________________________
  # Convert format--------------------------------------------------------------
  weight  <- weight[order(welfare)]
  welfare <- welfare[order(welfare)]
  if (tp %in% c("gd_2", "gd_3", "gd_5")) {
    welfare_original <- welfare
    weight_original  <- weight
  }
  switch(
    tp,
    "md"   = {
      imputation_id <- rep(1, length(welfare))
    },
    "id"   = {
      cl <- "md"
    },
    "gd_1" = {
      dt <- wbpip::gd_clean_data(dt          = data.frame(welfare = welfare,
                                                          weight  = weight),
                                 welfare     = "welfare",
                                 population  = "weight",
                                 gd_type     = 1,
                                 quiet       = TRUE)
      welfare <- dt$welfare
      weight  <- dt$weight
    },
    "gd_2" = {
      dt <- wbpip::gd_clean_data(dt          = data.frame(welfare = welfare,
                                                          weight  = weight),
                                 welfare     = "welfare",
                                 population  = "weight",
                                 gd_type     = 2,
                                 quiet       = TRUE)
      welfare <- dt$welfare
      weight  <- dt$weight
    },
    "gd_3" = {
      cli::cli_abort(
        paste0("Group data of type `gd_3` not supported.")
      )
    },
    "gd_5" = {
      dt <- wbpip::gd_clean_data(dt          = data.frame(welfare = welfare,
                                                          weight  = weight),
                                 welfare     = "welfare",
                                 population  = "weight",
                                 gd_type     = 5,
                                 quiet       = TRUE)
      welfare <- dt$welfare
      weight  <- dt$weight
    }
  )

  #_____________________________________________________________________________
  # Params----------------------------------------------------------------------
  if (cl == "gd") {
    params <- pipgd_select_lorenz(welfare    = welfare,
                                  weight     = weight,
                                  mean       = mean,
                                  times_mean = times_mean,
                                  povshare   = povshare,
                                  povline    = povline,
                                  complete   = TRUE)
  }

  #_____________________________________________________________________________
  # Store args------------------------------------------------------------------
  args <- list(mean       = mean,
               times_mean = times_mean,
               povshare   = povshare,
               n          = n,
               popshare   = popshare,
               povline    = povline,
               lorenz     = lorenz)#,
               #complete   = complete)

  #_____________________________________________________________________________
  # Return----------------------------------------------------------------------
  class_func <- paste0("new_pipster_", cl) |>
    parse(text = _)
  ret <- list(
    welfare = eval(class_func)(welfare),
    weight  = eval(class_func)(weight),
    args    = args
  )
  if (cl == "gd") {
    ret$params <- params
  } else {
    ret$imputation_id <- imputation_id
  }
  class(ret) <- "pipster"

  ret

}




