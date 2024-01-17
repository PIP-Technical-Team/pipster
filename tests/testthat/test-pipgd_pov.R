# Test poverty headcount functions ####
# Test pipgd_pov_headcount_nv (non vectorized function) 

welfare <- pip_gd$L
weight  <- pip_gd$P
params <- pipgd_select_lorenz(welfare = welfare, weight = weight, complete = TRUE)


test_that("pipgd_pov_headcount_nv works", {

  res_with_params <- pipgd_pov_headcount_nv(params = params, welfare = NULL, weight = NULL)
  res_with_welfare_weight <- pipgd_pov_headcount_nv(welfare = welfare, weight = weight)
  expect_equal(res_with_params, res_with_welfare_weight)
  expect_equal(class(res_with_params), "list")
  expect_equal(class(res_with_welfare_weight), "list")
  
  expect_error(pipgd_pov_headcount_nv(welfare = welfare, weight = weight, lorenz = "neither NULL, lb or lq"))
  expect_error(pipgd_pov_headcount_nv(params = params, lorenz = "neither NULL, lb or lq"))
  expect_error(pipgd_pov_headcount_nv(params = NULL, welfare = NULL, weight = weight))
  expect_error(pipgd_pov_headcount_nv(params = NULL, welfare = welfare, weight = NULL))

  pipgd_pov_headcount_nv(welfare = welfare, weight = weight) |>
    names() |>
    expect_equal("pov_stats")

  pipgd_pov_headcount_nv(welfare = welfare, weight = weight)$pov_stats |>
    names() |>
    expect_equal(c("headcount", "lorenz"))

  pipgd_pov_headcount_nv(welfare = welfare, weight = weight, lorenz = "lq")$pov_stats$headcount |>
    expect_equal(params$gd_params$lq$validity$headcount)
  
  pipgd_pov_headcount_nv(params = params, lorenz = NULL)$pov_stats$headcount |>
    expect_equal(params$gd_params[[params$selected_lorenz$for_pov]]$validity$headcount)
})

# Test pipgd_pov_headcount (vectorized function) 
test_that("pipgd_pov_headcount works as expected", {
  
  #  Inputs & Format ------------------------------------------------------------------------
  expect_equal(class(pipgd_pov_headcount(params = params, format = "list")), "list")
  expect_equal(class(pipgd_pov_headcount(params = params, format = "atomic")), "numeric")
  expect_error(pipgd_pov_headcount(params = params, format = "atomic", complete = TRUE))
  expect_error(pipgd_pov_headcount(params = params, format = "dt", complete = TRUE))
  #expect_equal(class(pipgd_pov_headcount(params = params, format = "dt")), "data.table data.frame")

  expect_equal(pipgd_pov_headcount(params = params, povline = c(.5, 1, 2, 3), format = "dt"), pipgd_pov_headcount(welfare = welfare, weight=weight, format = "dt"))
  expect_equal(pipgd_pov_headcount(params = params, povline = c(.5, 1, 2, 3), format = "atomic"), pipgd_pov_headcount(welfare = welfare, weight=weight, format = "atomic"))
  expect_equal(pipgd_pov_headcount(params = params, povline = c(.5, 1, 2, 3), format = "list"), pipgd_pov_headcount(welfare = welfare, weight=weight, format = "list"))
  
  # Computations -------------------------------------------------------------------
  res_atomic <- pipgd_pov_headcount(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "atomic")
  expect_equal(res_atomic[1], pipgd_pov_headcount_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$headcount)
  expect_equal(res_atomic[2], pipgd_pov_headcount_nv(welfare=welfare, weight=weight, povline = 1)$pov_stats$headcount)

  res_list <- pipgd_pov_headcount(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "list")
  expect_equal(res_list$pl0.5$pov_stats$headcount, pipgd_pov_headcount_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$headcount)

  res_dt <- pipgd_pov_headcount(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "dt")
  expect_equal(res_dt$headcount[1], pipgd_pov_headcount_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$headcount)
})

# Test poverty gap functions ####
# Test pipgd_pov_gap_nv (non vectorized function)

test_that("pipgd_pov_gap_nv works as expected", {
  
  # Inputs -------------------------------------------------------
  pipgd_pov_gap_nv(params = NULL, welfare = NULL, weight = NULL) |>
    expect_error()
  
  pipgd_pov_gap_nv(params = NULL, welfare = NULL, weight = weight) |>
    expect_error()
  
  pipgd_pov_gap_nv(params = NULL, welfare = welfare, weight = NULL) |>
    expect_error()
  
  pipgd_pov_gap_nv(params = params, lorenz = "Neither NULL, lq or lb") |>
    expect_error()
  
  pipgd_pov_gap_nv(welfare = welfare, weight = weight, lorenz = "Neither NULL, lq or lb") |>
    expect_error()

  # Output -------------------------------------------------------
  res_params_complete <- pipgd_pov_gap_nv(params = params, complete = TRUE)
  res_params <- pipgd_pov_gap_nv(params = params, complete = TRUE)
  
  pipgd_pov_gap_nv(welfare = welfare, weight = weight, lorenz = NULL)$pov_stats$lorenz |>
    expect_equal(pipgd_pov_headcount_nv(welfare = welfare, weight = weight, complete = TRUE)$selected_lorenz$for_pov)

  pipgd_pov_gap_nv(params = params, lorenz = NULL)$pov_stats$lorenz |>
    expect_equal(pipgd_pov_headcount_nv(params = params, complete = TRUE)$selected_lorenz$for_pov)

  pipgd_pov_gap_nv(params = params, lorenz = "lb")$pov_stats$lorenz |>
    expect_equal("lb")

  pipgd_pov_gap_nv(params = params, lorenz = "lq")$pov_stats$lorenz |>
    expect_equal("lq")

  # QUESTION: is it an error or is it correct that when complete is FALSE the output is of class list 
  # and not of class pipgd_params? (i am aware this is because in the function definition we specify:
  #  if (isFALSE(complete)) {
  #   params <- vector("list")
  #    })
  # but I am unsure on why we want to specify it
  
  # If this is not what we want, I would add expect_equal(class(res_params), class(res_params_complete))
  
  expect_equal(class(res_params), "list")
  expect_equal(class(res_params_complete), "pipgd_params")
  
  names(res_params) |>
    expect_equal("pov_stats")

  names(res_params$pov_stats) |>
    expect_equal(c("pov_gap", "lorenz"))

  names(res_params_complete) |>
    expect_equal(c("gd_params", "data", "selected_lorenz", "pov_stats"))
  
  names(res_params_complete$gd_params) |>
    expect_equal(c("lq", "lb"))
  
  names(res_params_complete$gd_params$lq) |>
    expect_equal(names(res_params_complete$gd$params$lb))
  
  names(res_params_complete$gd_params$lb) |>
    expect_equal(c("reg_results", "key_values", "validity"))

  names(res_params_complete$gd_params$lb$reg_results) |>
    expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

  names(res_params_complete$gd_params$lb$reg_results$coef) |>
    expect_equal(c("A", "B", "C"))
  
  names(res_params_complete$gd_params$lb$validity) |>
    expect_equal(c("is_valid", "is_normal", "headcount"))
  
  names(res_params_complete$gd_params$lq$key_values)|>
    expect_equal(c("e", "m", "n", "r", "s1","s2"))
  
  names(res_params_complete$gd$params$data) |>
      expect_equal(c("welfare", "weight"))
    
  names(res_params_complete$gd$params$selected_lorenz) |>
      expect_equal(c("for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov"))
  
  names(res_params_complete$gd$params$pov_stats) |>
      expect_equal(c("headcount", "lorenz", "pov_gap"))

})

# Test pipgd_pov_gap
test_that("pipgd_pov_gap works as expected", {

  # Inputs ---------------------------------------------------------------------
  pipgd_pov_gap(params = NULL, welfare = NULL, weight = NULL) |>
    expect_error()
  pipgd_pov_gap(params = NULL, welfare = welfare, weight = NULL) |>
    expect_error()
  pipgd_pov_gap(params = NULL, welfare = NULL, weight = weight) |>
    expect_error()
  
  # Computations ---------------------------------------------------------------
  res_povgap_atomic <- pipgd_pov_gap(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "atomic")
  expect_equal(res_povgap_atomic[1], pipgd_pov_gap_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$pov_gap)

  res_povgap_list <- pipgd_pov_gap(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "list")
  expect_equal(res_povgap_list$pl0.5$pov_stats$pov_gap, pipgd_pov_gap_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$pov_gap)

  res_povgap_dt <- pipgd_pov_gap(welfare = welfare, weight=weight, povline = c(.5, 1, 2, 3), format = "dt")
  expect_equal(res_povgap_dt$pov_gap[1], pipgd_pov_gap_nv(welfare=welfare, weight=weight, povline = 0.5)$pov_stats$pov_gap)

  # Output format --------------------------------------------------------------
  class(pipgd_pov_gap(welfare = welfare, weight = weight, format = "list")) |>
    expect_equal(class"list")
  class(pipgd_pov_gap(welfare = welfare, weight = weight, format = "atomic")) |>
    expect_equal(class"numeric")
  #class(pipgd_pov_gap(welfare = welfare, weight = weight, format = "dt")) |>
  #  expect_equal(class"list")
  
})


# Test poverty severity functions ####
# pipgd_pov_severity_nv (non vectorized)
test_that("pipgd_pov_severity_nv() -params works", {
  res_ps_params <- pipgd_pov_severity_nv(params = params)
  
  res_ps_welfare_weight <- pipgd_pov_severity_nv(welfare = welfare, weight  = weight)

  expect_equal(res_ps_params, res_ps_welfare_weight)
  expect_no_error(pipgd_pov_severity_nv(params = params, welfare = NULL, weight = NULL))
})


test_that("pipgd_pov_severity_nv() -mean works", {

  res_mean_1 <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare,
    mean    = 1
  )
  res_mean_none <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare
  )
  res_mean_null <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare,
    mean    = NULL
  )

  expect_equal(
    ps1, psnone
  )
  expect_equal(
    ps1, psnull
  )
})

test_that("pipgd_pov_severity_nv() -lorenz works", {
  res_lnull <- pipgd_pov_severity_nv(params = params, lorenz = NULL)
  res_lnull$pov_severity

})
test_that("pipgd_pov_severity_nv() -pov_gap works", {

})
test_that("pipgd_pov_severity_nv() -complete works", {

})

# Test pipgd_pov_severity (vectorized)
 


