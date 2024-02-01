# Collections of functions that are used across the package

#' Return data according to format
#'
#' @param ld list of data
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#'   single numeric vector, whose names are corresponding selected Lorenz for
#'   each value.  Default is "dt"
#' @param var character: name of variable to be returned.
#'
#' @return data.table, list, or atomic vector
#' @keywords internal
return_format <-
  function(ld,
           var,
           povline = NULL,
           complete = FALSE,
           format = c("dt", "list", "atomic")) {

    format <- match.arg(format)

    inv_reduce <- function(x,f) {
      Reduce(f,x)
    }

    #   ____________________________________________________
    #   Early returns                                   ####
    # if (FALSE) {
    #   return()
    # }

    #   ____________________________________________________
    #   Computations                                     ####
    if (format == "list") {
      # return(append(ld, list(povline = povline)))
      names(ld) <- paste0("pl", povline)

      return(ld)
    }

    if (complete == TRUE) {
      cli::cli_abort("{.field complete} is only available with {.field format} = 'list'")
    }

    dt <- ld |>
      inv_reduce(c) |>
      inv_reduce(c)

    pg <- dt[names(dt) == var] |>
      unlist()
    sl <- dt[names(dt) == "lorenz"] |>
      unlist()

    if (format == "dt") {
      dt <- data.table::data.table(povline   = povline,
                       V1        = pg,
                       lorenz    = sl)
      data.table::setnames(dt, "V1", var)
      return(dt)
    }

    if (format == "atomic") {
      names(pg) <- sl
      return(pg)
    }


  }





#' Return data according to format - microdata
#'
#' @inheritParams return_format
#'
#' @return determined by `format`
return_format_md <- function(
    ld,
    var,
    povline,
    complete = FALSE,
    format   = c("dt", "list", "atomic")
){

  format <- match.arg(format)

  inv_reduce <- function(x,f) {
    Reduce(f,x)
  }

  # ____________________________________________________________________________
  # Early Returns --------------------------------------------------------------
  # if (FALSE) {
  #   return()
  # }

  # ____________________________________________________________________________
  # Computations ---------------------------------------------------------------
  if (format == "list") {
    names(ld) <- paste0("pl", povline)

    return(ld)
  } else{

    if (complete == TRUE) {
      cli::cli_abort("{.field complete} is only available with {.field format} = 'list'")
    }

    dt <- ld |>
      inv_reduce(c) |>
      inv_reduce(c)

    names(dt) <- paste0("pl", povline)

    if (format == "atomic") {
      return(dt)
    } else if (format == "dt") {

      dt <- data.table::data.table(
        povline = povline,
        V1      = dt |> unname()
      )
      data.table::setnames(
        dt,
        old = "V1",
        new = var
      )
      return(dt)
    }

  }

}









