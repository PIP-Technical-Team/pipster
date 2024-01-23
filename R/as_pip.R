#' Convert to PIP format and add class
#'
#' Convert welfare, weight and (optionally) imputed id vectors to PIP format
#' from a data.frame
#'
#'
#' @param dt data.frame with welfare data
#' @param welfare_var character: variable name of welfare vector in dt
#' @param weight_var character: variable name of weight vector in dt
#' @param imputation_id_var character: variable name of imputation ID vector in
#'   dt
#' @param pip_type character: One of "md", "id", "gd_*". Generally this comes
#'   from the output of [identify_pip_type()]
#' @inheritParams identify_pip_type
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # Example 1: Basic usage with md data
#' as_pip(dt = pip_md,
#'        welfare_var = "welfare",
#'        weight_var = "weight")
#'
#' # Example 2: Including imputation_id_var
#' as_pip(dt = pip_id,
#'        welfare_var = "welfare",
#'        weight_var = "weight",
#'        imputation_id_var = "imputation_id")
#'
#' # Example 3: Basic usage with gd data and explicit pip_type
#' as_pip(dt = pip_gd,
#'        welfare_var = "X",
#'        weight_var = "W",
#'        pip_type = "gd")
#'
as_pip <- function(
    dt,
    welfare_var,
    weight_var,
    imputation_id_var   = NULL,
    pip_type            = NULL,
    groupdata_threshold = getOption("pipster.gd_threshold"),
    verbose             = getOption("pipster.verbose")
    ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  welfare        <- dt[[welfare_var]]
  weight         <- dt[[weight_var]]


  if (is.null(imputation_id_var)) {
    imputation_id <-  NULL
  } else {
    imputation_id  <- dt[[imputation_id_var]]
  }


  if (is.null(pip_type)) {
    pip_type <- identify_pip_type(welfare       = welfare,
                                  weight        = weight,
                                  imputation_id = imputation_id,
                                  verbose       = verbose)
    if (verbose) {
      cli::cli_alert("PIP type identified: {.val {pip_type}}")
    }
  } else {
    identify_pip_type_check()
  }
  pip_type <- match.arg(pip_type, c("md", "id", "gd_1", "gd_2", "gd_3", "gd_4", "gd_5"))

  convert_to_pip_format_check()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # set up   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # imputation_id
  if (!is.null(imputation_id)) {
    # convert to factor if it is character
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # transformations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## microdata --------
  tb <- if (pip_type %in% c("md", "id")) {
    convert_to_pip_md(dt,
                            welfare_var,
                            imputation_id_var,
                            verbose)
    # add pip class


  } else {
    tp <- gsub("(gd_)([1-5])", "\\2", pip_type) |>
      as.numeric()

    wbpip:::gd_clean_data(dt = dt,
                                welfare = welfare_var,
                                population = weight_var,
                                gd_type = tp)

  }

  tb <- add_pip_class(x = tb, pip_type)

}


#' add PIP class
#'
#' @param x data frame
#' @inheritParams as_pip
#'
#' @return data frame with pip class
#' @keywords internal
add_pip_class <- function(x, pip_type) {
  stopifnot(is.data.frame(x))

  if (!(inherits(x, "data.table"))) {
    x <- data.table::as.data.table(x)
  }

  tp <- substr(pip_type, 1, 2)
  if (tp == "id") {
    tp <- c("id", "md")
  }
  new_class <- paste0("pip", tp)

  data.table::setattr(x, "class", c(new_class, class(x)))
  return(invisible(data.table::copy(x)))
}


convert_to_pip_format_check <- function() {
  l <- as.list(parent.frame())
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defensive setup   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (l$pip_type == "gd_1") {

  } else if (l$pip_type == "gd_2") {

  } else if (l$pip_type == "gd_3") {

  } else if (l$pip_type == "gd_4") {

  } else if (l$pip_type == "gd_5") {

  } else if (l$pip_type == "md") {

  } else if (l$pip_type == "id") {

  } else {
    cli::cli_abort("{.var pip_type} {.field {pip_type}} is not a valid value")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Defenses --------
  stopifnot( exprs = {

  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Warnings --------


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))


}



#' Convert vectors to microdata data PIP format
#'
#' @inheritParams as_pip

#' @return dataframe
#' @keywords internal
convert_to_pip_md <- function(
    dt,
    welfare_var,
    imputation_id_var,
    verbose       = getOption("pipster.verbose")
    ) {

  if (!is.null(imputation_id_var)) {
    tb <-
      dt |>
      roworderv(cols = c(imputation_id_var, welfare_var))

  } else {
    tb <-
      dt |>
      roworderv(cols = welfare_var)
  }

  return(tb)

}
