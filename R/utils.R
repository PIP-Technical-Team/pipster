# Collections of functions that are used across the package

#' Return data according to format
#'
#' @param ld list of data
#' @param format character: either "dt" for data.table, "list" or "atomic" for a
#'   single numeric vector, whose names are corresponding selected Lorenz for
#'   each value.  Default is "dt"
#' @param var character: name of variable to be returned.
#' @param povline numeric: poverty line
#' @param complete logical: if `format = "list"` then `complete = TRUE` gives complete
#' information output.
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
      attr(pg,"povline") <- povline
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


#' Return data according to format - microdata - pov
#'
#' @inheritParams return_format
#'
#' @return determined by `format`
return_format_md_pov <- function(ld,
                                  var,
                                  povline,
                                  complete = FALSE,
                                  format = c("dt", "list", "atomic")) {
  format <- match.arg(format)

  # complete non-lists = error
  if (complete == TRUE && format != "list") {
    cli::cli_abort("{.field complete} is only available with {.field format} = 'list'")
  }

  # atomic format
  if (format == "atomic") {

    atomic_vector <- sapply(ld, function(item) item$pov_stats[[var]], simplify = "vector", USE.NAMES = TRUE)

    names(atomic_vector) <- sapply(ld, function(item) paste0("pl", round(item$pov_stats$povline)))
    return(atomic_vector)
  }


  # dt format
  if (format == 'dt') {

    dt_list <- lapply(ld, function(item) {
      povline_value <- item$pov_stats$povline
      stat_value <- item$pov_stats[[var]]
      dt <- data.table(povline = povline_value, stat_value = stat_value)
      setnames(dt, "stat_value", var)
      return(dt)
    })

    combined_dt <- rbindlist(dt_list)
    return(combined_dt)
  }

  # list format
  if (format == 'list' && complete == FALSE ) {

    # remove ld$pov_stats$fgt_data but keep rest of ld$pov_stats
    modified_ld <- lapply(ld, function(item) {
      pov_stats <- item$pov_stats
      pov_stats$fgt_data <- NULL
      return(pov_stats)
    })

    names(modified_ld) <- paste0("pl", sapply(modified_ld, function(item) round(item$povline)))

    return(modified_ld)

  } else {

    names(ld) <- paste0("pl", sapply(ld, function(item) round(item$pov_stats$povline)))

    return(ld)
  }

}



#' return md dist data format
#'
#' @param p object from md_dist functions
#' @param name character: name of the indicator
#' @inheritParams pipmd_quantile
#'
#' @return depending on format.
#' @keywords internal
return_format_md_dist <- function(p, name, format = "atomic") {
  if (format == "list") {
    return(p |> as.list())
  } else if (format == "atomic") {
    return(p)
  } else if (format == "dt") {
    p <- data.table::data.table(
      indicator = name,
      value     = p
    )
    return(p)
  }
}
