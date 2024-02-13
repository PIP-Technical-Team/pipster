

# Headcount ------------------------------------


#' Calculate poverty headcount
#'
#' This is a generic function to compute the
#' poverty headcount ratio - i.e. FGT 0.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_pov_headcount
#'
#' @return list
#' @export
get_pov_headcount <-
  function(pipster_object,
           povline,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_pov_headcount",
              object = welfare)
  }



#' Calculate poverty headcount for group data
#'
#' This is a method to compute poverty headcount on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_pov_headcount], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_headcount
#' @param ... additional arguments passed to [pipgd_pov_headcount]
#'
#' @return list
#' @export
get_pov_headcount.pipster_gd <-
  function(pipster_object,
           povline,
           ...) {

    # Computations
    res <-
      pipgd_pov_headcount(pipster_object = pipster_object,
                          povline        = povline,
                          ...)

    # Format
    res

  }



#' Calculate poverty headcount for micro data
#'
#' This is a method to compute poverty headcount on micro data.
#' Requires a pipster object of class `pipster_md` (grouped data object).
#' This is a wrapper over [pipmd_pov_headcount], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_headcount
#' @param ... additional arguments passed to [pipmd_pov_headcount]
#'
#' @return list
#' @export
get_pov_headcount.pipster_md <-
  function(pipster_object,
           povline,
           ...) {

    pipmd_pov_headcount(pipster_object = pipster_object,
                        povline        = povline,
                        format         = "list",
                        ...)
  }


#' Calculate poverty headcount default method
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_pov_headcount
#' @param ... additional arguments
#'
#' @return list
#' @export
get_pov_headcount.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }

























# Poverty Gap ------------------------------------


#' Calculate poverty gap
#'
#' This is a generic function to compute the
#' poverty gap - i.e. FGT 1.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_pov_gap
#'
#' @return list
#' @export
get_pov_gap <-
  function(pipster_object,
           povline,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_pov_gap",
              object = welfare)
  }



#' Calculate poverty gap for group data
#'
#' This is a method to compute poverty gap on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_pov_gap], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_gap
#' @param ... additional arguments passed to [pipgd_pov_gap]
#'
#' @return list
#' @export
get_pov_gap.pipster_gd <-
  function(pipster_object,
           povline,
           ...) {

    # Computations
    res <-
      pipgd_pov_gap(pipster_object = pipster_object,
                    povline        = povline,
                    ...)

    # Format
    res

  }



#' Calculate poverty gap for micro data
#'
#' This is a method to compute poverty gap on micro data.
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_pov_gap], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_gap
#' @param ... additional arguments passed to [pipmd_pov_gap]
#'
#' @return list
#' @export
get_pov_gap.pipster_md <-
  function(pipster_object,
           povline,
           ...) {

    pipmd_pov_gap(pipster_object = pipster_object,
                  povline        = povline,
                  format         = "list",
                  ...)
  }


#' Calculate poverty gap default method
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_pov_gap
#' @param ... additional arguments
#'
#' @return list
#' @export
get_pov_gap.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }




# Poverty Severity ------------------------------------


#' Calculate poverty severity
#'
#' This is a generic function to compute the
#' poverty severity - i.e. FGT 2.
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_pov_severity
#'
#' @return list
#' @export
get_pov_severity <-
  function(pipster_object,
           povline,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_pov_severity",
              object = welfare)
  }



#' Calculate poverty severity for group data
#'
#' This is a method to compute poverty severity on group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_pov_severity], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_severity
#' @param ... additional arguments passed to [pipgd_pov_severity]
#'
#' @return list
#' @export
get_pov_severity.pipster_gd <-
  function(pipster_object,
           povline,
           ...) {

    # Computations
    res <-
      pipgd_pov_severity(pipster_object = pipster_object,
                         povline        = povline,
                         ...)

    # Format
    res

  }



#' Calculate poverty severity for micro data
#'
#' This is a method to compute poverty severity on micro data.
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_pov_severity], which should be viewed
#' for more detail.
#'
#' @inheritParams get_pov_severity
#' @param ... additional arguments passed to [pipmd_pov_severity]
#'
#' @return list
#' @export
get_pov_severity.pipster_md <-
  function(pipster_object,
           povline,
           ...) {

    pipmd_pov_gap(pipster_object = pipster_object,
                  povline        = povline,
                  format         = "list",
                  ...)
  }


#' Calculate poverty severity default method
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_pov_severity
#' @param ... additional arguments
#'
#' @return list
#' @export
get_pov_severity.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }




# Watts ------------------------------------


#' Calculate Watts poverty index
#'
#' This is a generic function to compute the
#' Watts poverty index
#'
#' @param pipster_object pipster object created using [create_pipster_object]
#' @inheritParams pipmd_watts
#'
#' @return list
#' @export
get_watts <-
  function(pipster_object,
           povline,
           ... ) {

    welfare <- pipster_object$welfare

    UseMethod("get_watts",
              object = welfare)
  }



#' Calculate Watts poverty index for group data
#'
#' This is a method to compute Watts poverty indexon group data.
#' Requires a pipster object of class `pipster_gd` (grouped data object).
#' This is a wrapper over [pipgd_watts], which should be viewed
#' for more detail.
#'
#' @inheritParams get_watts
#' @param ... additional arguments passed to [pipgd_watts]
#'
#' @return list
#' @export
get_watts.pipster_gd <-
  function(pipster_object,
           povline,
           ...) {

    # Computations
    res <-
      pipgd_watts(pipster_object = pipster_object,
                         povline        = povline,
                         ...)

    # Format
    res

  }



#' Calculate Watts poverty index for micro data
#'
#' This is a method to compute Watts poverty index on micro data.
#' Requires a pipster object of class `pipster_md` (micro data object).
#' This is a wrapper over [pipmd_watts], which should be viewed
#' for more detail.
#'
#' @inheritParams get_watts
#' @param ... additional arguments passed to [pipmd_watts]
#'
#' @return list
#' @export
get_watts.pipster_md <-
  function(pipster_object,
           povline,
           ...) {

    pipmd_watts(pipster_object = pipster_object,
                povline        = povline,
                format         = "list",
                ...)
  }


#' Calculate Watts poverty index default method
#'
#' This default S3 method returns an error when called
#'
#' @inheritParams get_watts
#' @param ... additional arguments
#'
#' @return list
#' @export
get_watts.default <-
  function(pipster_object,
           ...) {

    cli::cli_abort("No default exist. Please check object class.")
  }


