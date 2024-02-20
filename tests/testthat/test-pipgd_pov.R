#_______________________________________________________________________________
# Define Objects----------------------------------------------------------------
#_______________________________________________________________________________
welfare <- pip_gd$L
weight  <- pip_gd$P
params  <- pipgd_select_lorenz(welfare  = welfare,
                               weight   = weight,
                               complete = TRUE)


#_______________________________________________________________________________
# HEADCOUNT --------------------------------------------------------------------
#_______________________________________________________________________________
test_that("pipgd_pov_headcount_nv works", {

  res_with_params         <- pipgd_pov_headcount_nv(params  = params,
                                                    welfare = NULL,
                                                    weight  = NULL)
  res_with_welfare_weight <- pipgd_pov_headcount_nv(welfare = welfare,
                                                    weight  = weight)
  res_complete            <- pipgd_pov_headcount_nv(welfare = welfare,
                                                    weight  = weight,
                                                    complete = TRUE)
  #__________________________________
  # Type
  expect_equal(class(res_with_params),
               "list")
  expect_equal(class(res_with_welfare_weight),
               "list")
  expect_equal(class(res_complete),
               "pipgd_params")

  #__________________________________
  # Errors
  expect_error(
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight  = weight,
                           lorenz  = "neither NULL, lb or lq")
  )
  expect_error(
    pipgd_pov_headcount_nv(params = params,
                           lorenz = "neither NULL, lb or lq")
  )
  expect_error(
    pipgd_pov_headcount_nv(params  = NULL,
                           welfare = NULL,
                           weight  = weight)
  )
  expect_error(
    pipgd_pov_headcount_nv(params  = NULL,
                           welfare = welfare,
                           weight  = NULL)
  )
  #__________________________________
  # Names & Items
  expect_equal(
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight = weight) |>
      names(),
    "pov_stats"
  )

  expect_equal(
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight = weight)$pov_stats |>
      names(),
    c("headcount", "lorenz")
    )
  #__________________________________
  # Output

  expect_equal(res_with_params,
               res_with_welfare_weight)
  expect_equal(
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight  = weight,
                           lorenz  = "lq")$pov_stats$headcount,
    params$gd_params$lq$validity$headcount)

  expect_equal(pipgd_pov_headcount_nv(params = params,
                                      lorenz = NULL)$pov_stats$headcount,
               params$gd_params[[params$selected_lorenz$for_pov]]$validity$headcount)

})

# Test pipgd_pov_headcount (vectorized function)
test_that("pipgd_pov_headcount works as expected", {

  povline = c(.5, 1, 2, 3)

  #  Inputs & Format ------------------------------------------------------------------------
  expect_equal(
    class(pipgd_pov_headcount(params = params,
                              format = "list")),
    "list")
  expect_equal(
    class(pipgd_pov_headcount(params = params,
                              format = "atomic")),
    "numeric")
  expect_error(
    pipgd_pov_headcount(params = params,
                        format = "atomic",
                        complete = TRUE))
  expect_error(
    pipgd_pov_headcount(params = params,
                        format = "dt",
                        complete = TRUE))
  expect_equal(
    class(pipgd_pov_headcount(params = params,
                              format = "dt")),
    c("data.table",
      "data.frame"))

  expect_equal(
    pipgd_pov_headcount(params  = params,
                        povline = povline,
                        format  = "dt"),
    pipgd_pov_headcount(welfare = welfare,
                        weight  = weight,
                        povline = povline,
                        format  = "dt")
    )
  expect_equal(
    pipgd_pov_headcount(params  = params,
                        povline = povline,
                        format  = "atomic"),
    pipgd_pov_headcount(welfare = welfare,
                        weight  = weight,
                        povline = povline,
                        format  = "atomic"))
  expect_equal(
    pipgd_pov_headcount(params  = params,
                        povline = povline,
                        format  = "list"),
    pipgd_pov_headcount(welfare = welfare,
                        weight  = weight,
                        povline = povline,
                        format  = "list"))

  # Computations -------------------------------------------------------------------
  res_atomic <- pipgd_pov_headcount(welfare = welfare,
                                    weight  = weight,
                                    povline = c(.5, 1, 2, 3),
                                    format  = "atomic")
  expect_equal(
    res_atomic[[1]],
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight  = weight,
                           povline = 0.5)$pov_stats$headcount)

  res_list <- pipgd_pov_headcount(welfare     = welfare,
                                  weight      = weight,
                                  povline     = c(.5, 1, 2, 3),
                                  format      = "list")
  expect_equal(
    res_list$pl0.5$pov_stats$headcount,
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight  = weight,
                           povline = 0.5)$pov_stats$headcount)

  res_dt <- pipgd_pov_headcount(welfare = welfare,
                                weight  = weight,
                                povline = c(.5, 1, 2, 3),
                                format  = "dt")
  expect_equal(
    res_dt$headcount[1],
    pipgd_pov_headcount_nv(welfare = welfare,
                           weight  = weight,
                           povline = 0.5)$pov_stats$headcount)
})

# Test poverty gap functions ####
# Test pipgd_pov_gap_nv (non vectorized function)

#_______________________________________________________________________________
# POV GAP ----------------------------------------------------------------------
#_______________________________________________________________________________


test_that("pipgd_pov_gap_nv works as expected", {

  # Inputs & Errors -------------------------------------------------------
  expect_error(
    pipgd_pov_gap_nv(params  = NULL,
                     welfare = NULL,
                     weight  = NULL))
  expect_error(
    pipgd_pov_gap_nv(params  = NULL,
                     welfare = NULL,
                     weight  = weight))
  expect_error(
    pipgd_pov_gap_nv(params  = NULL,
                     welfare = NULL,
                     weight  = weight))

  expect_error(
    pipgd_pov_gap_nv(params  = NULL,
                     welfare = welfare,
                     weight  = NULL))

  expect_error(
    pipgd_pov_gap_nv(params  = params,
                     lorenz  = "Neither NULL, lq or lb"))

  expect_error(
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     lorenz  = "Neither NULL, lq or lb"))

  expect_no_error(
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     popshare = 0.6))

  expect_error(
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     popshare = 0.6,
                     povline = 0.8))


  # Computation of povline ---------------------------------------
  out_lq <- pipgd_pov_gap_nv(welfare = welfare,
                          weight = weight,
                          popshare = 0.6,
                          lorenz = "lq")

  pov_gap_lq_bm <- 0.18981108366762

  out_lq$pov_stats$pov_gap |>
      expect_equal(pov_gap_lq_bm)

  pov_gap_lb_bm <- 0.196668019926771

  out_lb <- pipgd_pov_gap_nv(welfare = welfare,
                             weight = weight,
                             popshare = 0.6,
                             lorenz = "lb")

  #out_lb$pov_stats$pov_gap |>
  #  expect_equal(pov_gap_lb_bm)


  # Output -------------------------------------------------------
  res_params_complete <- pipgd_pov_gap_nv(params   = params,
                                          complete = TRUE)
  res_params          <- pipgd_pov_gap_nv(params   = params)

  expect_equal(
    pipgd_pov_gap_nv(welfare        = welfare,
                     weight         = weight,
                     lorenz         = NULL)$pov_stats$lorenz,
    pipgd_pov_headcount_nv(welfare  = welfare,
                           weight   = weight,
                           complete = TRUE)$selected_lorenz$for_pov)

  expect_equal(
    pipgd_pov_gap_nv(params         = params,
                     lorenz         = NULL)$pov_stats$lorenz,
    pipgd_pov_headcount_nv(params   = params,
                           complete = TRUE)$selected_lorenz$for_pov)

  expect_equal(
    pipgd_pov_gap_nv(params = params,
                     lorenz = "lb")$pov_stats$lorenz,
    "lb")

  expect_equal(
    pipgd_pov_gap_nv(params = params,
                     lorenz = "lq")$pov_stats$lorenz,
    "lq")

  expect_equal(class(res_params),
               "list")

  expect_equal(class(res_params_complete),
               "pipgd_params")

  expect_equal(
    res_params |>
      names(),
    "pov_stats")

  expect_equal(
    res_params$pov_stats |>
      names(),
    c("pov_gap", "lorenz"))

  names(res_params_complete) |>
    expect_equal(c("gd_params",
                   "data",
                   "selected_lorenz",
                   "pov_stats"))

  names(res_params_complete$gd_params) |>
    expect_equal(c("lq",
                   "lb"))

  names(res_params_complete$gd_params$lq) |>
    expect_equal(names(res_params_complete$gd_params$lb))

  names(res_params_complete$gd_params$lb) |>
    expect_equal(c("reg_results",
                   "key_values",
                   "validity"))

  names(res_params_complete$gd_params$lb$reg_results) |>
    expect_equal(c("ymean",
                   "sst",
                   "coef",
                   "sse",
                   "r2",
                   "mse",
                   "se"))

  names(res_params_complete$gd_params$lb$reg_results$coef) |>
    expect_equal(c("A",
                   "B",
                   "C"))

  names(res_params_complete$gd_params$lb$validity) |>
    expect_equal(c("is_valid",
                   "is_normal",
                   "headcount"))

  names(res_params_complete$gd_params$lq$key_values) |>
    expect_equal(c("e",
                   "m",
                   "n",
                   "r",
                   "s1",
                   "s2"))

  names(res_params_complete$data) |>
      expect_equal(c("welfare",
                     "weight"))

  names(res_params_complete$selected_lorenz) |>
      expect_equal(c("for_dist",
                     "for_pov",
                     "use_lq_for_dist",
                     "use_lq_for_pov"))

  names(res_params_complete$pov_stats) |>
      expect_equal(c("headcount",
                     "lorenz",
                     "pov_gap"))

})

# Test pipgd_pov_gap
test_that("pipgd_pov_gap works as expected", {

  # Inputs ---------------------------------------------------------------------
  pipgd_pov_gap(params  = NULL,
                welfare = NULL,
                weight  = NULL) |>
    expect_error()

  pipgd_pov_gap(params  = NULL,
                welfare = welfare,
                weight  = NULL) |>
    expect_error()

  pipgd_pov_gap(params  = NULL,
                welfare = NULL,
                weight  = weight) |>
    expect_error()

  # Computations ---------------------------------------------------------------
  res_povgap_atomic <- pipgd_pov_gap(welfare = welfare,
                                     weight  = weight,
                                     povline = c(.5, 1, 2, 3),
                                     format  = "atomic")

  expect_equal(
    res_povgap_atomic[[1]],
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     povline = 0.5)$pov_stats$pov_gap)


  expect_equal(pipgd_pov_gap(welfare = welfare,
                             weight  = weight,
                             povline = c(.5, 1, 2, 3),
                             format  = "atomic"),
               pipgd_pov_gap(params = params,
                             povline = c(.5, 1, 2, 3),
                             format  = "atomic"))


  res_povgap_list <- pipgd_pov_gap(welfare = welfare,
                                   weight  = weight,
                                   povline = c(.5, 1, 2, 3),
                                   format  = "list")
  expect_equal(
    res_povgap_list$pl0.5$pov_stats$pov_gap,
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     povline = 0.5)$pov_stats$pov_gap)

  res_povgap_dt <- pipgd_pov_gap(welfare = welfare,
                                 weight  = weight,
                                 povline = c(.5, 1, 2, 3),
                                 format  = "dt")
  expect_equal(
    res_povgap_dt$pov_gap[1],
    pipgd_pov_gap_nv(welfare = welfare,
                     weight  = weight,
                     povline = 0.5)$pov_stats$pov_gap)

  # Output format --------------------------------------------------------------
  class(pipgd_pov_gap(welfare = welfare,
                      weight  = weight,
                      format  = "list")) |>
    expect_equal("list")

  class(pipgd_pov_gap(welfare = welfare,
                      weight  = weight,
                      format  = "atomic")) |>
    expect_equal("numeric")

  class(pipgd_pov_gap(welfare = welfare,
                      weight  = weight,
                      format  = "dt")) |>
    expect_equal(c("data.table",
                   "data.frame"))

})

#_______________________________________________________________________________
# POV SEVERITY -----------------------------------------------------------------
#_______________________________________________________________________________
# Test poverty severity functions ####
# pipgd_pov_severity_nv (non vectorized)
test_that("pipgd_pov_severity_nv() -params works", {

  res_ps_params         <- pipgd_pov_severity_nv(params  = params)
  res_ps_welfare_weight <- pipgd_pov_severity_nv(welfare = welfare,
                                                 weight  = weight)

  expect_equal(res_ps_params,
               res_ps_welfare_weight)

  expect_error(
    pipgd_pov_severity_nv(welfare = welfare,
                          weight = NULL)
  )

  expect_error(
    pipgd_pov_severity_nv(welfare = NULL,
                          weight = weight)
  )

  expect_no_error(
    pipgd_pov_severity_nv(params = params,
                          welfare = NULL,
                          weight = NULL))
})


test_that("pipgd_pov_severity_nv() -mean works", {

  res_mean_1 <- pipgd_pov_severity_nv(
    welfare = welfare,
    weight  = weight,
    mean    = 1
  )
  res_mean_none <- pipgd_pov_severity_nv(
    welfare = welfare,
    weight  = weight
  )

  pipgd_pov_severity_nv(
    welfare = welfare,
    weight  = weight,
    mean    = NULL) |>
      expect_error()

  expect_equal(
    res_mean_1, res_mean_none
  )

})

test_that("pipgd_pov_severity_nv() -lorenz works", {

  res_lnull <- pipgd_pov_severity_nv(welfare = welfare,
                                     weight = weight,
                                     lorenz = NULL)
  res_lnull$pov_severity

  # Invalid Lorenz
  expect_error(
    pipgd_pov_severity_nv(welfare = welfare,
                          weight  = weight,
                          lorenz  = "neither NULL, lb or lq"))
  expect_error(
    pipgd_pov_severity_nv(welfare = welfare,
                          weight  = weight,
                          lorenz  = 5))

  # Lorenz                        = "lb"
  pipgd_pov_severity_nv(welfare   = welfare,
                        weight    = weight,
                        lorenz    = "lb")$pov_stats$lorenz |>
    expect_equal("lb")

  # Lorenz                        = "lq"
  pipgd_pov_severity_nv(welfare   = welfare,
                        weight    = weight,
                        lorenz    = "lq")$pov_stats$lorenz  |>
    expect_equal("lq")

  # Lorenz                        = NULL
  pipgd_pov_severity_nv(welfare   = welfare,
                        weight    = weight,
                        lorenz    = NULL)$pov_stats$lorenz  |>
    expect_equal(params$selected_lorenz$for_pov)

})

test_that("pipgd_pov_severity_nv() -pov_gap works", {
  # pov gap must be either NULL or the result of pipster:::pipgd_pov_gap_nv
  pov_gap <- 0.205233231505016
  pipgd_pov_severity_nv(welfare = welfare,
                        weight  = weight,
                        pov_gap = pov_gap) |>
    expect_error()
  pov_gap <- list(pov_stats = list("not_correct" = 1:5))
  pipgd_pov_severity_nv(welfare = welfare,
                        weight  = weight,
                        pov_gap = pov_gap) |>
    expect_error()

  pov_gap <- pipgd_pov_gap_nv(
    welfare  = welfare,
    weight   = weight,
    complete = TRUE
  )

  expect_equal(
    pipgd_pov_severity_nv(pov_gap = pov_gap,
                          welfare = welfare,
                          weight = weight),

    pipgd_pov_severity_nv(welfare = welfare,
                          weight = weight)
    )

})

test_that("pipgd_pov_severity_nv() -complete works", {
  res_complete     <- pipgd_pov_severity_nv(welfare  = welfare,
                                            weight   = weight,
                                            complete = TRUE)
  res_not_complete <- pipgd_pov_severity_nv(welfare  = welfare,
                                            weight   = weight)

  # Checking popshare and povline arguments

test_that("pipgd_pov_severity_nv() -popshare works", {

    popshare = 0.4
    pipgd_pov_severity_nv(welfare = welfare,
                          weight  = weight,
                          popshare = popshare) |>
      expect_no_error()

  })


  # Output--------------------------------------
  expect_equal(
    c(res_not_complete$pov_stats$pov_severity,
      res_not_complete$pov_stats$lorenz),
    c(res_complete$pov_stats$pov_severity,
      res_complete$pov_stats$lorenz)
  )
  # Names & Structure --------------------------
  names(res_complete) |>
    expect_equal(c("gd_params",
                   "data",
                   "selected_lorenz",
                   "pov_stats"))

  names(res_complete$gd_params) |>
    expect_equal(c("lq",
                   "lb"))

  names(res_complete$gd_params$lq) |>
    expect_equal(names(res_complete$gd_params$lb))

  names(res_complete$gd_params$lb) |>
    expect_equal(c("reg_results",
                   "key_values",
                   "validity"))

  names(res_complete$gd_params$lb$reg_results) |>
    expect_equal(c("ymean",
                   "sst",
                   "coef",
                   "sse",
                   "r2",
                   "mse",
                   "se"))

  names(res_complete$gd_params$lb$reg_results$coef) |>
    expect_equal(c("A",
                   "B",
                   "C"))

  names(res_complete$gd_params$lb$validity) |>
    expect_equal(c("is_valid",
                   "is_normal",
                   "headcount"))

  names(res_complete$gd_params$lq$key_values) |>
    expect_equal(c("e",
                   "m",
                   "n",
                   "r",
                   "s1",
                   "s2"))

  names(res_complete$data) |>
      expect_equal(c("welfare",
                     "weight"))

  names(res_complete$selected_lorenz) |>
      expect_equal(c("for_dist",
                     "for_pov",
                     "use_lq_for_dist",
                     "use_lq_for_pov"))

  names(res_complete$pov_stats) |>
      expect_equal(c("headcount",
                     "lorenz",
                     "pov_gap",
                     "pov_severity"))

  # Test output class
  class(res_complete) |>
    expect_equal("pipgd_params")

  class(res_not_complete) |>
    expect_equal("list")

})

test_that("pipgd_pov_severity_nv() -pov_severity works", {
  params_pov_gap <- pipgd_pov_gap_nv(welfare      = welfare,
                                     weight       = weight,
                                     complete     = TRUE)
  # Test pov_severity is returned from wbpip:::gd_compute_pov_severity_lb (when lorenz='lb')
  res            <- pipgd_pov_severity_nv(welfare = welfare,
                                          weight  = weight,
                                          lorenz  = "lb")$pov_stats$pov_severity
  res_benchmark  <- 0.0902182821029137

  res |>
    expect_equal(res_benchmark)

  # Test pov_severity is returned from wbpip::wbpip:::gd_compute_pov_severity_lb (when lorenz='lq')
  expect_no_error(
    pipgd_pov_severity_nv(welfare = welfare,
                          weight  = weight,
                          lorenz  = "lq")$pov_stats$pov_severity
  )

})

# Test pipgd_pov_severity (vectorized)

test_that("pipgd_pov_severity inputs works as expected", {

  povline = c(.5, 1, 2, 3)

  # Test params, welfare and weights arguments work as expected
  expect_equal(
    pipgd_pov_severity(welfare = welfare,
                       weight  = weight,
                       povline = povline),
    pipgd_pov_severity(params  = params,
                       povline = povline))

  pipgd_pov_severity(welfare = welfare,
                     weight  = NULL,
                     povline = povline) |>
    expect_error()

  pipgd_pov_severity(welfare = NULL,
                     weight  = weight,
                     povline = povline) |>
    expect_error()

  # Test format argument works as expected
  pipgd_pov_severity(welfare = welfare,
                     weight  = weight,
                     povline = povline,
                     format  = "Neither dt, list or atomic") |>
    expect_error()

  # Test lorenz argument works as expected
  pipgd_pov_severity(welfare = welfare,
                     weight  = weight,
                     povline = povline,
                     lorenz  = "Neither NULL, lb or lq") |>
    expect_error()

  # Test mean argument works as expected
  expect_equal(
    pipgd_pov_severity(welfare = welfare,
                       weight  = weight,
                       povline = povline,
                       mean    = 1),
    pipgd_pov_severity(welfare = welfare,
                       weight  = weight,
                       povline = povline))

  pipgd_pov_severity(welfare   = welfare,
                     weight    = weight,
                     povline   = povline,
                     mean      = NULL) |>
    expect_error()

  # Test popshare argument works
  pipgd_pov_severity(welfare   = welfare,
                     weight    = weight,
                     popshare = 0.4) |>
    expect_no_error()


  # Test pov_gap argument works as expected
  # NOTE:
  expect_error(
    pipgd_pov_severity(welfare  = welfare,
                       weight   = weight,
                       pov_gap  = 0.2052332))
  expect_equal(pipgd_pov_severity(welfare = welfare,
                                  weight  = weight,
                                  pov_gap = pipgd_pov_gap_nv(welfare  = welfare,
                                                             weight   = weight,
                                                             complete = T)),
               pipgd_pov_severity(welfare = welfare,
                                  weight  = weight))

  # Test complete argument works as expected
  expect_equal(
    pipgd_pov_severity(welfare  = welfare,
                       weight   = weight,
                       format   = "list")$pl1$pov_stats$pov_severity,

    pipgd_pov_severity(welfare  = welfare,
                       weight   = weight,
                       format   = "list",
                       complete = TRUE)$pl1$pov_stats$pov_severity
  )
})

test_that("pipgd_pov_severity -outputs",{

  povline = c(.5, 1, 2, 3)

  res_atomic_v <- pipgd_pov_severity(welfare = welfare,
                                     weight  = weight,
                                     povline = povline,
                                     format  = "atomic")
  res_dt_v     <- pipgd_pov_severity(welfare = welfare,
                                     weight  = weight,
                                     povline = povline,
                                     format  = "dt")
  res_list_v   <- pipgd_pov_severity(welfare = welfare,
                                     weight  = weight,
                                     povline = povline,
                                     format  = "list")

  class(res_atomic_v) |>
    expect_equal("numeric")

  class(res_list_v) |>
    expect_equal("list")

  # Check names in output list - !! the checks below will fail because the function at the current version
  # does not produce correct outputs names !!

  names(res_list_v) |>
    expect_equal(c("pl0.5",
                   "pl1",
                   "pl2",
                   "pl3"))

  names(res_list_v$pl0.5) |>
    expect_equal(names(res_list_v$pl1))

  names(res_list_v$pl0.5) |>
    expect_equal(names(res_list_v$pl2))

  names(res_list_v$pl0.5) |>
    expect_equal(names(res_list_v$pl3))

  names(res_list_v$pl0.5) |>
    expect_equal("pov_stats")

  names(res_list_v$pl0.5$pov_stats) |>
    expect_equal(c("pov_severity",
                   "lorenz"))

  class(pipgd_pov_severity(welfare = welfare,
                           weight  = weight,
                           povline = povline,
                           format  = "dt")) |>
   expect_equal(c("data.table",
                  "data.frame"))

  # Check that vectorization works
  res_povsev_nv <- pipgd_pov_severity_nv(welfare = welfare,
                                         weight  = weight,
                                         povline = 0.5)$pov_stats$pov_severity

  res_atomic_v[[1]] |>
    expect_equal(res_povsev_nv)

  res_dt_v$pov_severity[1] |>
    expect_equal(res_povsev_nv)

  # The check below will fail because, currently, when format = "list" the output names are incorrect
   res_list_v$pl0.5$pov_stats$pov_severity |>
    expect_equal(res_povsev_nv)
})

#_______________________________________________________________________________
# WATTS ------------------------------------------------------------------------
#_______________________________________________________________________________

# Test pipgd_watts_nv() function (non vectorized) -INPUTS
params <- pipgd_pov_gap_nv(welfare = welfare, weight = weight, complete = TRUE)

test_that("pipgd_watts_nv inputs work as expected", {

  res        <- pipgd_watts_nv(welfare = welfare,
                               weight  = weight)
  res_params <- pipgd_watts_nv(params  = params)



  # Check the output is the same when providing welfare and wieght or params
  expect_equal(res, res_params)

  # Check either params or (welfare and weights) are provided
  pipgd_watts_nv(params     = NULL,
                 welfare    = NULL,
                 weight     = NULL) |>
    expect_error()
  pipgd_watts_nv(params     = NULL,
                 welfare    = welfare,
                 weight     = NULL) |>
    expect_error()
  pipgd_watts_nv(params     = NULL,
                 welfare    = NULL,
                 weight     = weight) |>
    expect_error()

  # Check invalid mean, times_mean give errors
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 mean       = "invalid mean") |>
    expect_error()
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 times_mean = "invalid times_mean") |>
    expect_error()

  # Check popshare argument works as expected
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 popshare   = "invalid popshare") |>
    expect_error()
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 popshare   = 0.3) |>
    expect_no_error()

  # Check either popshare or povline are provided
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 popshare   = 0.4, povline = 0.5) |>
    expect_error()

  # Check that Lorenz argument works as expected
  pipgd_watts_nv(welfare    = welfare,
                 weight     = weight,
                 lorenz     = "Neither NULL, lq or lb") |>
    expect_error()

})

# Test pipgd_watts_nv() function (non vectorized) -OUTPUTS
test_that("pipgd_watts_nv outputs work as expected", {

  params              <- pipgd_pov_gap_nv(welfare  = welfare,
                                          weight   = weight,
                                          complete = TRUE)
  res                 <- pipgd_watts_nv(welfare = welfare,
                                        weight  = weight)
  res_params          <- pipgd_watts_nv(params = params)

  res_complete        <- pipgd_watts_nv(welfare  = welfare,
                                        weight   = weight,
                                        complete = TRUE)
  res_params_complete <- pipgd_watts_nv(params   = params,
                                        complete = TRUE)

  # Output class
  class(res) |>
    expect_equal(class(res_params))

  # To check !
  class(res) |>
    expect_equal("list")

  class(res_complete) |>
    expect_equal(class(res_params_complete))

  class(res_params_complete) |>
    expect_equal("pipgd_params")

  # Names in output list when complete is FALSE
  names(res) |>
    expect_equal("pov_stats")

  names(res) |>
    expect_equal(names(res_params))

  names(res$pov_stats) |>
    expect_equal(c("watts",
                   "lorenz"))

  # Names in output list when complete is TRUE
  names(res_complete) |>
    expect_equal(c("gd_params",
                   "data",
                   "selected_lorenz",
                   "pov_stats"))

  names(res_complete$gd_params) |>
    expect_equal(c("lq",
                   "lb"))

  names(res_complete$gd_params$lq) |>
    expect_equal(names(res_complete$gd_params$lb))

  names(res_complete$gd_params$lq) |>
    expect_equal(c("reg_results",
                   "key_values",
                   "validity"))

  names(res_complete$gd_params$lq$reg_results) |>
    expect_equal(c("ymean",
                   "sst",
                   "coef",
                   "sse",
                   "r2",
                   "mse",
                   "se"))

  names(res_complete$gd_params$lq$key_values) |>
    expect_equal(c("e",
                   "m",
                   "n",
                   "r",
                   "s1",
                   "s2"))

  names(res_complete$gd_params$lq$validity) |>
    expect_equal(c("is_normal",
                   "is_valid",
                   "headcount"))

  names(res_complete$gd_params$lb$key_values) |>
    expect_equal(NULL)

  names(res_complete$data) |>
    expect_equal(c("welfare",
                   "weight"))

  names(res_complete$selected_lorenz) |>
    expect_equal(c( "for_dist",
                    "for_pov",
                    "use_lq_for_dist",
                    "use_lq_for_pov" ))

   names(res_complete$pov_stats) |>
    expect_equal(c("headcount",
                   "lorenz",
                   "watts"))

})

# Test pipgd_watts_nv() function (non vectorized) -CALCULATION OF WATTS
test_that("pipgd_watts_nv watts output is as expected", {

  params           <- pipgd_pov_gap_nv(welfare  = welfare,
                                       weight   = weight,
                                       complete = TRUE)
  res_with_lb      <- pipgd_watts_nv(welfare    = welfare,
                                     weight     = weight,
                                     lorenz     = "lb")
  res_with_lq      <- pipgd_watts_nv(welfare    = welfare,
                                     weight     = weight,
                                     lorenz     = "lq")

  res_lb_benchmark <- list(pov_stats = list(watts = 0.277580116426276,
                                            lorenz = "lb"))
  res_lq_benchmark <- list(pov_stats = list(watts = 0.60555598,
                                            lorenz = "lq"))

  res_with_lb |>
    expect_equal(res_lb_benchmark)

  res_with_lq |>
    expect_equal(res_lq_benchmark)
})


test_that("pipgd_watts inputs works as expected", {

  povline = c(.5, 1, 2, 3)

  # Test params, welfare and weights arguments work as expected
  pipgd_watts(welfare = welfare,
              weight  = weight,
              povline = povline) |>
    expect_equal(pipgd_watts(params  = params,
                             povline = povline))

  pipgd_watts(welfare = welfare,
              weight  = NULL,
              povline  = povline) |>
    expect_error()

  pipgd_watts(welfare = NULL,
              weight  = weight,
              povline = povline) |>
    expect_error()

  # Test format argument works as expected
  pipgd_watts(welfare = welfare,
              weight  = weight,
              povline = povline,
              format  = "Neither dt, list or atomic") |>
    expect_error()

  # Test lorenz argument works as expected
  pipgd_watts(welfare = welfare,
              weight  = weight,
              povline = povline,
              lorenz  = "Neither NULL, lb or lq") |>
    expect_error()

  # Test mean argument works as expected
  pipgd_watts(welfare = welfare,
              weight  = weight,
              povline = povline,
              mean    = 1) |>
    expect_equal(pipgd_watts(welfare = welfare,
                             weight  = weight,
                             povline = povline))

  pipgd_watts(welfare = welfare,
              weight  = weight,
              povline = povline,
              mean    = NULL) |>
    expect_error()

  # Test popshare argument works as expected
  pipgd_watts(welfare  = welfare,
              weight   = weight,
              popshare = 0.5) |>
    expect_no_error()

})

# Test pipgd_watts() function (vectorized) -OUTPUTS
test_that("pipgd_watts -outputs",{
  povline = c(.5, 1, 2, 3)

  res_atomic <- pipgd_watts(welfare = welfare,
                            weight  = weight,
                            povline = povline,
                            format  = "atomic")
  res_dt     <- pipgd_watts(welfare = welfare,
                            weight  = weight,
                            povline = povline,
                            format  = "dt")
  res_list   <- pipgd_watts(welfare = welfare,
                            weight  = weight,
                            povline = povline,
                            format  = "list")

  class(res_atomic) |>
    expect_equal("numeric")

  class(res_list) |>
    expect_equal("list")

  class(res_dt) |>
   expect_equal(c("data.table",
                  "data.frame"))

  # Check names in output list
  names(res_list) |>
    expect_equal(c("pl0.5",
                   "pl1",
                   "pl2",
                   "pl3"))

  names(res_list$pl0.5) |>
    expect_equal(names(res_list$pl1))

  names(res_list$pl0.5) |>
    expect_equal(names(res_list$pl2))

  names(res_list$pl0.5) |>
    expect_equal(names(res_list$pl3))

  names(res_list$pl0.5) |>
    expect_equal("pov_stats")

  names(res_list$pl0.5$pov_stats) |>
    expect_equal(c("watts",
                   "lorenz"))

  # Check that vectorization works
  res_watts_nv <- pipgd_watts_nv(welfare = welfare,
                                 weight  = weight,
                                 povline = 0.5)$pov_stats$watts

  res_atomic[[1]] |>
    expect_equal(res_watts_nv)

  res_dt$watts[1] |>
    expect_equal(res_watts_nv)

  # The check below will fail also because, currently, when format = "list" the output names are incorrect
  res_list$pl0.5$pov_stats$watts |>
    expect_equal(res_watts_nv)
})




