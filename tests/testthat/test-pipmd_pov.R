# Testing functions related to poverty measures (for micro data)
welfare_s = pip_md_s$welfare
weight_s  = pip_md_s$weight

# Testing poverty headcount function (non vectorized) ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_pov_headcount_nv arguments work as expected", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  pipmd_pov_headcount_nv(welfare = welfare_test,
                         weight  = weight_s) |>
    expect_error()

  pipmd_pov_headcount_nv(welfare = NULL,
                         weight  = weight_s) |>
    expect_error()

  pipmd_pov_headcount_nv(welfare = welfare_s,
                         weight  = weight_test) |>
    expect_error()

  pipmd_pov_headcount_nv(welfare = welfare_s,
                         weight  = weight_s,
                         povline = "non numeric povline") |>
    expect_error()

  pipmd_pov_headcount_nv(welfare = welfare_s,
                         weight  = weight_s,
                         povline = NULL) |>
    expect_error()

  pipmd_pov_headcount_nv(welfare = welfare_s,
                         weight  = weight_s,
                         povline = -0.5) |>
    expect_message()

  pipmd_pov_headcount_nv(welfare = welfare_s,
                         weight  = weight_s,
                         povline = 19) |>
    expect_message()

})

# Computations ---------------------------------------------------------------
test_that("pipmd_pov_headcount_nv outputs work as expected", {

  res <- pipmd_pov_headcount_nv(welfare = welfare_s,
                                weight  = weight_s,
                                povline = 1)
  res_benchmark <- list(pov_headcount   = 0.126034968864758)

  class(res) |>
    expect_equal("list")

  length(res) |>
    expect_equal(1)

  names(res) |>
    expect_equal("pov_headcount")

  res |>
    expect_equal(res_benchmark)

})

# Testing poverty headcount function (vectorized) ####

# When format is "list" --------------------------------------------------------------------------------

test_that("pipmd_pov_headcount work as expected -when format is list", {

  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test  <- weight_s
  weight_test[1]  = NA

  povline      <- c(0.5, 1, 2)
  format       <-  "list"

  # Arguments
  pipmd_pov_headcount(welfare        = welfare_test,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = NULL,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_test,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  # pipmd_pov_headcount(welfare        = welfare_s,
  #                     povline        = povline,
  #                     format         = format) |>
  #   expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = "non numeric povline",
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = NULL,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = -0.5,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = 19,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_no_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <- pipmd_pov_headcount(welfare = welfare_s,
                             weight  = weight_s,
                             povline = povline,
                             format  = format)
  res_benchmark <- list(pl0.5 = list(pov_headcount = 0.0192906691433047),
                        pl1 = list(pov_headcount = 0.126034968864758),
                        pl2 = list(pov_headcount = 0.423769472762453))

  class(res) |>
    expect_equal("list")

  names(res) |>
    expect_equal(c("pl0.5", "pl1", "pl2"))

  length(res) |>
    expect_equal(length(povline))

  res |>
    expect_equal(res_benchmark)

})

# When format is "dt" -------------------------------------------------
test_that("pipmd_pov_headcount work as expected -when format is dt", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  povline <- c(0.5, 1, 2)
  format = "dt"

  # Arguments
  pipmd_pov_headcount(welfare        = welfare_test,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = NULL,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_test,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  # pipmd_pov_headcount(welfare        = welfare_s,
  #                     povline        = povline,
  #                     format         = format) |>
  #   expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = "non numeric povline",
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = NULL,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = -0.5,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = 19,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_no_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <- pipmd_pov_headcount(welfare = welfare_s,
                             weight  = weight_s,
                             povline = povline,
                             format  = format)

  res_benchmark <- structure(
    list(povline = c(0.5, 1, 2),
         pov_headcount = c(0.0192906691433047,
                           0.126034968864758,
                           0.423769472762453)),
    row.names = c(NA, -3L),
    class = c("data.table",
              "data.frame"))

  class(res) |>
    expect_equal(c("data.table",
                   "data.frame"))

  names(res) |>
    expect_equal(c("povline",
                   "pov_headcount"))

  nrow(res) |>
    expect_equal(length(povline))

  res |>
    expect_equal(res_benchmark)

})

# When format is "atomic" -------------------------------------------------
test_that("pipmd_pov_headcount work as expected -when format is atomic", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test  <- weight_s
  weight_test[1]  = NA

  povline      <- c(0.5, 1, 2)
  format          = "atomic"

  # Arguments
  pipmd_pov_headcount(welfare        = welfare_test,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = NULL,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_test,
                      povline        = povline,
                      format         = format) |>
    expect_error()

  # pipmd_pov_headcount(welfare        = welfare_s,
  #                     povline        = povline,
  #                     format         = format) |>
  #   expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = "non numeric povline",
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = NULL,
                      format         = format) |>
    expect_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = -0.5,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = 19,
                      format         = format) |>
    expect_message()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = format) |>
    expect_no_error()

  pipmd_pov_headcount(welfare        = welfare_s,
                      weight         = weight_s,
                      povline        = povline,
                      format         = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <- pipmd_pov_headcount(welfare = welfare_s,
                             weight  = weight_s,
                             povline = povline,
                             format  = format)
  res_benchmark <- c(pl0.5 = 0.0192906691433047,
                     pl1   = 0.126034968864758,
                     pl2   = 0.423769472762453)

  class(res) |>
    expect_equal("numeric")

  names(res) |>
    expect_equal(c("pl0.5",
                   "pl1",
                   "pl2" ))

  length(res) |>
    expect_equal(length(povline))

  res |>
    expect_equal(res_benchmark)

})

# Testing poverty gap function (non vectorized) ####
# Arguments --------------------------------------------------------
test_that("pipmd_pov_gap_nv arguments work as expected", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test  <- weight_s
  weight_test[1]  = NA

  pipmd_pov_gap_nv(welfare = welfare_test,
                   weight  = weight_s) |>
    expect_error()

  pipmd_pov_gap_nv(welfare = NULL,
                   weight  = weight_s) |>
    expect_error()

  pipmd_pov_gap_nv(welfare = welfare_s,
                   weight  = weight_test) |>
    expect_error()

  pipmd_pov_gap_nv(welfare = welfare_s) |>
    expect_message()

  pipmd_pov_gap_nv(welfare = welfare_s,
                   weight  = weight_s,
                   povline = NULL) |>
    expect_error()

  pipmd_pov_gap_nv(welfare = welfare_s,
                   weight  = weight_s,
                   povline = 0.2) |>
    expect_message()

  pipmd_pov_gap_nv(welfare = welfare_s,
                   weight  = weight_s,
                   povline = 19) |>
    expect_message()

  pipmd_pov_gap_nv(welfare = welfare_s,
                   weight  = weight_s,
                   povline = 0.5) |>
    expect_no_error()

})

# Outputs --------------------------------------------------------
test_that("pipmd_pov_gap_nv outputs work as expected", {
  res <- pipmd_pov_gap_nv(welfare = welfare_s,
                          weight  = weight_s,
                          povline = 1)

  res_benchmark <- list(pov_gap   = 0.0376375358962338)

  class(res) |>
    expect_equal("list")

  names(res) |>
    expect_equal("pov_gap")

  res |>
    expect_equal(res_benchmark)

})

# Testing poverty gap function (vectorized) ####
# Inputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_gap arguments work as expected", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test  <- weight_s
  weight_test[1]  = NA

  povline      <- c(0.5, 1, 2)
  format          = "list"

  pipmd_pov_gap(welfare = welfare_test,
                weight  = weight_s,
                povline = povline,
                format  = format) |>
    expect_error()

  pipmd_pov_gap(welfare = NULL,
                weight  = weight_s,
                povline = povline,
                format  = format) |>
    expect_error()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_test,
                povline = povline,
                format  = format) |>
    expect_error()

  pipmd_pov_gap(welfare = welfare_s,
                povline = povline,
                format  = format) |>
    expect_message()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = "non numeric povline",
                format  = format) |>
    expect_error()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = NULL,
                format  = format) |>
    expect_error()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = -0.5,
                format  = format) |>
    expect_message()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = 19,
                format  = format) |>
    expect_message()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = povline,
                format  = format) |>
    expect_no_error()

  pipmd_pov_gap(welfare = welfare_s,
                weight  = weight_s,
                povline = povline,
                format  = "neither atomic, dt or list") |>
    expect_error()
})

# Outputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_gap work as expected", {

   povline <- c(0.5, 1, 2)

  res_list <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format = "list")
  res_dt   <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format = "dt")
  res_atom <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format = "atomic")

  res_bm_list   <- list(pl0.5 = list(pov_gap = 0.00252705288414119),
                        pl1   = list(pov_gap = 0.0376375358962338),
                        pl2   = list(pov_gap = 0.154012543240549))
  res_bm_atomic <- c(pl0.5 = 0.00252705288414119,
                     pl1 = 0.0376375358962338,
                     pl2 = 0.154012543240549)
  res_bm_dt     <- structure(
    list(povline = c(0.5, 1, 2),
         pov_gap = c(0.00252705288414119,
                     0.0376375358962338,
                     0.154012543240549)),
    row.names = c(NA, -3L),
    class = c("data.table",
              "data.frame"))

  # When format is "list"
  class(res_list) |>
    expect_equal("list")

  names(res_list) |>
    expect_equal(c("pl0.5",
                   "pl1",
                   "pl2"))

  names(res_atom) |>
    expect_equal(names(res_list))

  names(res_list$pl0.5) |>
    expect_equal("pov_gap")

  names(res_list$pl1) |>
    expect_equal(names(res_list$pl0.5))

  names(res_list$pl2) |>
    expect_equal(names(res_list$pl0.5))

  length(res_list) |>
    expect_equal(length(povline))

  # When format is dt
  class(res_dt) |>
   expect_equal(c("data.table", "data.frame"))

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))

  res_list |>
    expect_equal(res_bm_list)

  res_atom |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

})

# Testing poverty severity function (non vectorized) ####
# Arguments ----------------------------------------------------------------------------------
test_that("pipmd_pov_severity_nv arguments work as expected", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  pipmd_pov_severity_nv(welfare = welfare_test,
                        weight  = weight_s) |>
    expect_error()

  pipmd_pov_severity_nv(welfare = NULL,
                        weight  = weight_s) |>
    expect_error()

  pipmd_pov_severity_nv(welfare = welfare_s,
                        weight  = weight_test) |>
    expect_error()

  pipmd_pov_severity_nv(welfare = welfare_s) |>
    expect_message()

  pipmd_pov_severity_nv(welfare = welfare_s,
                        weight  = weight_s,
                        povline = "non numeric povline") |>
    expect_error()

  pipmd_pov_severity_nv(welfare = welfare_s,
                        weight  = weight_s,
                        povline = NULL) |>
    expect_error()

  pipmd_pov_severity_nv(welfare = welfare_s,
                        weight  = weight_s,
                        povline = -0.5) |>
    expect_message()

  pipmd_pov_severity_nv(welfare = welfare_s,
                        weight  = weight_s,
                        povline = 19) |>
    expect_message()

})

# Output ------------------------------------------------------------------
test_that("pipmd_pov_severity_nv outputs work as expected", {

  res <- pipmd_pov_severity_nv(welfare = welfare_s,
                               weight  = weight_s)

  res_benchmark <- list(pov_severity   = 0.0141310112148631)

  class(res) |>
    expect_equal("list")

  length(res) |>
    expect_equal(1)

  names(res) |>
    expect_equal("pov_severity")

  res |>
    expect_equal(res_benchmark)

})

# Testing poverty severity function (vectorized) ####
# Arguments -----------------------------------------------------------------------
test_that("pipmd_pov_severity arguments work as expected", {

  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  povline <- c(0.5, 1, 2)
  format = "list"

  pipmd_pov_severity(welfare = welfare_test,
                     weight = weight_s,
                     povline = povline,
                     format = format) |>
    expect_error()

  pipmd_pov_severity(welfare = NULL,
                     weight = weight_s,
                     povline = povline,
                     format = format) |>
    expect_error()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_test,
                     povline = povline,
                     format = format) |>
    expect_error()

  pipmd_pov_severity(welfare = welfare_s,
                     povline = povline,
                     format = format) |>
    expect_message()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = "non numeric povline",
                     format = format) |>
    expect_error()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = NULL,
                     format = format) |>
    expect_error()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = -0.5,
                     format = format) |>
    expect_message()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = 19,
                     format = format) |>
    expect_message()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = povline,
                     format = format) |>
    expect_no_error()

  pipmd_pov_severity(welfare = welfare_s,
                     weight = weight_s,
                     povline = povline,
                     format = "neither atomic, dt or list") |>
    expect_error()
})

# Outputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_severity work as expected", {
  povline       <- c(0.5, 1, 2)

  res_list      <- pipmd_pov_severity(welfare = welfare_s,
                                      weight = weight_s,
                                      povline = povline,
                                      format = "list")
  res_dt        <- pipmd_pov_severity(welfare = welfare_s,
                                      weight = weight_s,
                                      povline = povline,
                                      format = "dt")
  res_atom      <- pipmd_pov_severity(welfare = welfare_s,
                                      weight = weight_s,
                                      povline = povline,
                                      format = "atomic")

  res_bm_list   <- list(pl0.5 = list(pov_severity = 0.000561012489886177),
                        pl1   = list(pov_severity = 0.0141310112148631),
                        pl2   = list(pov_severity = 0.0773230776014347))
  res_bm_atomic <- c(pl0.5 = 0.000561012489886177,
                     pl1   = 0.0141310112148631,
                     pl2   = 0.0773230776014347)
  res_bm_dt     <- structure(
    list(povline      = c(0.5, 1, 2),
         pov_severity = c(0.000561012489886177,
                          0.0141310112148631,
                          0.0773230776014347)),
    row.names         = c(NA,
                          -3L),
    class             = c("data.table",
                          "data.frame"))

  # When format is "list"
  class(res_list) |>
    expect_equal("list")

  names(res_list) |>
    expect_equal(c("pl0.5", "pl1", "pl2"))

  names(res_atom) |>
    expect_equal(names(res_list))

  names(res_list$pl0.5) |>
    expect_equal("pov_severity")

  names(res_list$pl1) |>
    expect_equal(names(res_list$pl0.5))

  names(res_list$pl2) |>
    expect_equal(names(res_list$pl0.5))

  length(res_list) |>
    expect_equal(length(povline))

  # When format is dt
  class(res_dt) |>
   expect_equal(c("data.table", "data.frame"))
  names(res_dt) |>
    expect_equal(c("povline", "pov_severity"))

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))

  res_list |>
    expect_equal(res_bm_list)

  res_atom |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

})

# Testing watts index function (non vectorized) ####
# Arguments ----------------------------------------------------------------------------------
test_that("pipmd_watts_nv arguments work as expected", {
  welfare_test <- welfare_s
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  pipmd_watts_nv(welfare = welfare_test, weight = weight_s) |>
    expect_error()

  pipmd_watts_nv(welfare = NULL, weight = weight_s) |>
    expect_error()

  pipmd_watts_nv(welfare = welfare_s, weight = weight_test) |>
    expect_error()

  pipmd_watts_nv(welfare = welfare_s) |>
    expect_message()

  pipmd_watts_nv(welfare = welfare_s, weight = weight_s, povline = "non numeric povline") |>
    expect_error()

  pipmd_watts_nv(welfare = welfare_s, weight = weight_s, povline = NULL) |>
    expect_error()

  pipmd_watts_nv(welfare = welfare_s, weight = weight_s, povline = -0.5) |>
    expect_message()

  pipmd_watts_nv(welfare = welfare_s, weight = weight_s, povline = 19) |>
    expect_message()

})

# Outputs -------------------------------------------------------------------------------------------
test_that("pipmd_watts_nv outputs work as expected", {
  res <- pipmd_watts_nv(welfare = welfare_s, weight = weight_s)
  res_benchmark <- list(watts = 0.048153814961799)

  class(res) |>
    expect_equal("list")

  length(res) |>
    expect_equal(1)

  names(res) |>
    expect_equal("watts")

  res |>
    expect_equal(res_benchmark)

})




# Testing watts index function (vectorized) ####
test_that("pipmd_watts work as expected", {

  povline <- c(0.5, 1, 2)

  res_list <- pipmd_watts(welfare = welfare_s,
                          weight  = weight_s,
                          povline = povline,
                          format  = "list")
  res_dt <- pipmd_watts(welfare   = welfare_s,
                        weight    = weight_s,
                        povline   = povline,
                        format    = "dt")
  res_atom <- pipmd_watts(welfare = welfare_s,
                          weight  = weight_s,
                          povline = povline,
                          format  = "atomic")

  res_bm_list <- list(pl0.5 = round(0.002894164, 7),
                      pl1   = round(0.04815381, 7),
                      pl2   = round(0.2235731, 7))

  res_bm_atomic <- c(pl0.5 = 0.002894164,
                     pl1 = 0.04815381,
                     pl2 = 0.2235731)
  res_bm_dt <- structure(
    list(povline = c(0.5, 1, 2),
         watts = c(0.002894164,
                   0.04815381,
                   0.2235731)),
    row.names = c(NA, -3L),
    class = c("data.table", "data.frame"))

  lapply(res_list, function(x){round(x$watts, 7)}) |>
    expect_equal(res_bm_list)

  round(res_atom, 7) |>
    expect_equal(round(res_bm_atomic, 7))

  round(res_dt,7) |>
    expect_equal(round(res_bm_dt, 7))


  # When format is "list"
  class(res_list) |>
    expect_equal("list")

  names(res_list) |>
    expect_equal(c("pl0.5", "pl1", "pl2"))

  names(res_atom) |>
    expect_equal(names(res_list))

  names(res_list$pl0.5) |>
    expect_equal("watts")

  names(res_list$pl1) |>
    expect_equal(names(res_list$pl0.5))

  names(res_list$pl2) |>
    expect_equal(names(res_list$pl0.5))

  length(res_list) |>
    expect_equal(length(povline))

  # When format is dt
  #class(res_dt) |>
  #  expect_equal("data.table")
  names(res_dt) |>
    expect_equal(c("povline", "watts"))

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))

})












