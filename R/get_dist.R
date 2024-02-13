# Quantile------------------------------------


#' Quantile welfare values
#'
#' Gives the `n` quantile welfare values for the given welfare and weight vectors.
#' This is a generic function.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipgd_quantile
#'
#' @return list
#' @export
get_quantile <- function(pipster_object,
                         n        = 10,
                         popshare = seq(from = 1/n,
                                        to   = 1,
                                        by   = 1/n),
                         ... ) {
  welfare <- pipster_object$welfare
  UseMethod("get_quantile", object = welfare)
}


#' Quantile welfare values
#'
#' Gives the `n` quantile welfare values for the given welfare and weight vectors.
#' This is for `pipster_gd` grouped data objects. This is a wrapper
#' over [pipgd_quantile]. Requires a pipster object of class `pipster_gd`
#' (grouped data object).
#'
#' @inheritParams get_quantile
#' @param ... additional arguments passed to [pipgd_quantile]
#'
#' @return list
#' @export
get_quantile.pipster_gd <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  # Computations
  res <- pipgd_quantile(pipster_object = pipster_object,
                        n              = n,
                        popshare       = popshare,
                        ...)

  # Format
  res_list        <- res$dist_stats$quantile |>
    as.list()
  names(res_list) <- paste0(res$dist_stats$popshare*100,
                            "%")

  res_list

}


#' Quantile welfare values
#'
#' Gives the `n` quantile welfare values for the given welfare and weight vectors.
#' This is for `pipster_gd` grouped data objects. This is a wrapper
#' over [pipmd_quantile], which should be viewed for more detail.
#'
#' @inheritParams get_quantile
#' @param ... additional arguments passed to [pipmd_quantile]
#'
#' @return list
#' @export
get_quantile.pipster_md <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  pipmd_quantile(pipster_object = pipster_object,
                 n        = n,
                 popshare = popshare,
                 format   = "list",
                 ...)
}


#' Quantile welfare values
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_quantile
#' @param ... additional arguments
#'
#' @return list
#' @export
get_quantile.default <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  cli::cli_abort("No default exist. Please check object class.")
}


# Welfare share at------------------------------------


#' Welfare share by quantile
#'
#' Generic function. Returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 be default), to estimate the corresponding
#' share of welfare in each.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipgd_welfare_share_at
#'
#' @return list
#' @export
get_welfare_share_at <- function(pipster_object,
                                 n        = 10,
                                 popshare = seq(from = 1/n,
                                                to   = 1,
                                                by   = 1/n),
                                 ... ) {

  welfare <- pipster_object$welfare

  UseMethod("get_welfare_share_at", object = welfare)
}



#' Welfare share by quantile in group data
#'
#' Returns the share of welfare held by the specified
#' share of the population in the parameter `popshare`. Alternatively, you can
#' select the number of quantiles (10 be default), to estimate the corresponding
#' share of welfare in each.
#'
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_welfare_share_at], which should be viewed
#' for more detail.
#'
#' @inheritParams get_welfare_share_at
#' @param ... additional arguments passed to [pipgd_welfare_share_at]
#'
#' @return list
#' @export
get_welfare_share_at.pipster_gd <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  # Computations
  res <- pipgd_welfare_share_at(pipster_object = pipster_object,
                                n              = n,
                                popshare       = popshare,
                                ...)

  # Format
  res_list        <- res$dist_stats$welfare_share_at |>
    as.list()
  names(res_list) <- paste0(res$dist_stats$popshare*100,
                            "%")

  res_list

}


#' Quantile welfare values
#'
#' Gives the `n` quantile welfare values for the given welfare and weight vectors.
#'
#' Requires a pipster object of class `pipster_md` (grouped data object).
#' This is a wrapper over [pipmd_welfare_share_at], which should be viewed
#' for more detail.
#'
#' @inheritParams get_welfare_share_at
#' @param ... additional arguments passed to [pipmd_welfare_share_at]
#'
#' @return list
#' @export
get_welfare_share_at.pipster_md <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  pipmd_welfare_share_at(pipster_object = pipster_object,
                         n        = n,
                         popshare = popshare,
                         format   = "list",
                         ...)
}


#' Quantile welfare values
#'
#' This default S3 method returns an error when called.
#'
#' @inheritParams get_welfare_share_at
#' @param ... additional arguments
#'
#' @return list
#' @export
get_welfare_share_at.default <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

  cli::cli_abort("No default exist. Please check object class.")
}




# Quantile welfare share ------------------------------------


#' Quantile welfare share
#'
#' This is a generic function.
#' `get_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that [get_welfare_share_at] gets the share of
#' welfare held by a particular share of the population, which is essentially
#' the cumulative share. Instead, `get_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipgd_quantile_welfare_share
#'
#' @return list
#' @export
get_quantile_welfare_share <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ... ) {

  welfare <- pipster_object$welfare

  UseMethod("get_quantile_welfare_share",
            object = welfare)
}



#' Quantile welfare share in group data
#'
#' This is a generic function.
#' `get_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that [get_welfare_share_at] gets the share of
#' welfare held by a particular share of the population, which is essentially
#' the cumulative share. Instead, `get_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_quantile_welfare_share], which should be viewed
#' for more detail.
#'
#' @inheritParams get_quantile_welfare_share
#' @param ... additional arguments passed to [pipgd_quantile_welfare_share]
#'
#' @return list
#' @export
get_quantile_welfare_share.pipster_gd <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

    # Computations
    res <-
      pipgd_quantile_welfare_share(pipster_object = pipster_object,
                                   n              = n,
                                   popshare       = popshare,
                                   ...)

    # Format
    res_list        <- res$dist_stats$quantile_welfare_share |>
      as.list()
    names(res_list) <- paste0(res$dist_stats$popshare*100,
                              "%")

    res_list

  }


#' Quantile welfare share in micro data
#'
#' This is a generic function.
#' `get_quantile_welfare_share` returns the share of welfare held by a
#' particular quantile. Notice that [get_welfare_share_at] gets the share of
#' welfare held by a particular share of the population, which is essentially
#' the cumulative share. Instead, `get_quantile_welfare_share` returns
#' the proportion of welfare that only the specified quantile holds.
#'
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_quantile_welfare_share], which should be viewed
#' for more detail.
#'
#' @inheritParams get_quantile_welfare_share
#' @param ... additional arguments passed to [pipmd_quantile_welfare_share]
#'
#' @return list
#' @export
get_quantile_welfare_share.pipster_md <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

    pipmd_quantile_welfare_share(pipster_object = pipster_object,
                                 n        = n,
                                 popshare = popshare,
                                 format   = "list",
                                  ...)
  }


#' Quantile welfare values
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_quantile_welfare_share
#' @param ... additional arguments
#'
#' @return list
#' @export
get_quantile_welfare_share.default <-
  function(pipster_object,
           n        = 10,
           popshare = seq(from = 1/n,
                          to   = 1,
                          by   = 1/n),
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }


# gini ------------------------------------


#' Gini coefficient
#'
#' This is a generic function calculating the gini coefficient.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipgd_gini
#'
#' @return list
#' @export
get_gini <-
  function(pipster_object,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_gini",
              object = welfare)
  }



#' Gini coefficient on group data
#'
#' This is a method for computing the gini coefficient on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_gini], which should be viewed
#' for more detail.
#'
#' @inheritParams get_gini
#' @param ... additional arguments passed to [pipgd_gini]
#'
#' @return list
#' @export
get_gini.pipster_gd <-
  function(pipster_object,
           ...) {

    # Computations
    res <-
      pipgd_gini(pipster_object = pipster_object,
                 ...)

    # Format
    res <- res$dist_stats

    res

  }


#' Gini coefficient on micro data
#'
#' This is a method for computing the gini coefficient on micro data.
#' Requires a pipster object of class `pipster_md` (grouped micro object).
#' This is a wrapper over [pipmd_gini], which should be viewed
#' for more detail.
#'
#' @inheritParams get_gini
#' @param ... additional arguments passed to [pipmd_gini]
#'
#' @return list
#' @export
get_gini.pipster_md <-
  function(pipster_object,
           ...) {

    pipmd_gini(pipster_object = pipster_object,
               format   = "list",
                                 ...)
  }


#' Gini coefficient S3 default
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_gini
#' @param ... additional arguments
#'
#' @return list
#' @export
get_gini.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }




# Polarization ------------------------------------


#' Wolfson polarization index
#'
#' This is a generic function to compute the Wolfson polarization index.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_polarization
#'
#' @return list
#' @export
get_polarization <-
  function(pipster_object,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_polarization",
              object = welfare)
  }



#' Wolfson polarization index
#'
#' This is a method to compute the Wolfson polarization index on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over pipgd_polarization, which should be viewed
#' for more detail.
#'
#' @inheritParams get_polarization
#' @param ... additional arguments passed to pipgd_polarization
#'
#' @return list
#' @export
get_polarization.pipster_gd <-
  function(pipster_object,
           ...) {
    return("to be implemented")
    # Computations
    #res <-
    #  pipgd_polarization(pipster_object = pipster_object,
    #             ...)

    # Format
    res <- res$dist_stats

    res

  }


#' Wolfson polarization index
#'
#' This is a method to compute the Wolfson polarization index on micro data.
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_polarization], which should be viewed
#' for more detail.
#'
#' @inheritParams get_polarization
#' @param ... additional arguments passed to [pipmd_polarization]
#'
#' @return list
#' @export
get_polarization.pipster_md <-
  function(pipster_object,
           ...) {

    pipmd_polarization(pipster_object = pipster_object,
               format   = "list",
               ...)
  }


#' Gini coefficient S3 default
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_polarization
#' @param ... additional arguments
#'
#' @return list
#' @export
get_polarization.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }




# MLD ------------------------------------


#' Mean Log Deviation
#'
#' This is a generic function to compute the
#' Mean Log Deviation (MLD).
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_mld
#'
#' @return list
#' @export
get_mld <-
  function(pipster_object,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_mld",
              object = welfare)
  }



#'  Mean Log Deviation (MLD) for group data
#'
#' This is a method to compute the MLD on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_mld], which should be viewed
#' for more detail.
#'
#' @inheritParams get_mld
#' @param ... additional arguments passed to pipgd_mld
#'
#' @return list
#' @export
get_mld.pipster_gd <-
  function(pipster_object,
           ...) {

    # Computations
    res <-
      pipgd_mld(pipster_object = pipster_object,
                 ...)

    # Format
    res <- res$dist_stats

    res

  }


#' Wolfson polarization index
#'
#' This is a method to compute the Wolfson polarization index on micro data.
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_polarization], which should be viewed
#' for more detail.
#'
#' @inheritParams get_mld
#' @param ... additional arguments passed to [pipmd_polarization]
#'
#' @return list
#' @export
get_mld.pipster_md <-
  function(pipster_object,
           ...) {

    pipmd_mld(pipster_object = pipster_object,
              format         = "list",
              ...)
  }


#' Gini coefficient S3 default
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_mld
#' @param ... additional arguments
#'
#' @return list
#' @export
get_mld.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }























