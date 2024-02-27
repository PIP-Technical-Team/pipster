#' Create Pipster Object
#'
#' The first step in using the `pipster` package is to create a pipster object.
#' It creates the appropriate classes for welfare and weights, reformats the
#' data if necessary, and, for grouped data, it estimates and selects the lorenz
#' curves to be used for the estimation of poverty and distributional measures.
#' Note: Group Data must always come with a weight variable specified, or they
#' will be classified as micro data.
#'
#' @param welfare numeric: welfare vector (monetary or shares, also cumulative)
#' @param weight numeric: weight vector (count or shares, also cumulative)
#' @param imputation_id numeric: vector of ids for multiply imputed data.
#' Default is NULL
#'
#' @return list: pipster object containing welfare and weights for micro data,
#' params if grouped data, imputation_id if imputed data.
#' @export
#'
#' @examples
#'
#' # Example 1 - Group data
#' p_gd <- create_pipster_object(welfare = pip_gd$L,
#'                               weight  = pip_gd$P)
#' p_gd
#'
#' # Example 2 - Micro data
#' p_md <- create_pipster_object(welfare = pip_md$welfare,
#'                               weight  = pip_md$weight)
#' p_md
#'
#' # Example 3 - Micro data - NULL weight
#'
#' p_md_no_w <- create_pipster_object(welfare = pip_md$welfare)
#'
#' p_md_no_w
create_pipster_object <-
  function(welfare,
           weight        = NULL,
           imputation_id = NULL) {

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
  # treat NULL weights -----------

  if (is.null(weight)) {
    weight        <- rep(1, length(welfare))
  }

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
      if (flast(welfare) == 100) {
        welfare <- welfare/100
      }
    },
    "gd_2" = {
      welfare <- fcumsum(welfare)/fsum(welfare)
      weight  <- fcumsum(weight)/fsum(weight)
    },
    "gd_3" = {
      welfare <- fcumsum(welfare)/fsum(welfare)
    },
    "gd_5" = {
      welfare <- fcumsum(welfare)/fsum(welfare)
      weight  <- fcumsum(weight)/fsum(weight)
    }
  )

  #_____________________________________________________________________________
  # Params----------------------------------------------------------------------
  if (cl == "gd") {
    params <- pipgd_select_lorenz(welfare  = welfare,
                                  weight   = weight,
                                  complete = TRUE)
  }

  #_____________________________________________________________________________
  # Return----------------------------------------------------------------------
  class_func <- paste0("new_pipster_", cl) |>
    parse(text = _)
  ret <- list(
    welfare = eval(class_func)(welfare),
    weight  = eval(class_func)(weight)
  )
  if (cl == "gd") {
    ret$params <- params
  } else {
    ret$imputation_id <- imputation_id
  }
  class(ret) <- "pipster"

  ret

}



