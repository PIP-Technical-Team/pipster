
# Test pipgd_validate_lorenz function ####
test_that("pipgd_validate_lorenz inputs and outputs", {
  
  params <- pipgd_params(welfare = pip_gd$L, weight = pip_gd$P)
  res_with_params <- pipgd_params(welfare = pip_gd$L, weight = pip_gd$P) |>
    pipgd_validate_lorenz()
  res_with_welfare_weight <- pipgd_validate_lorenz(welfare = pip_gd$L, weight = pip_gd$P)

  # Inputs --------------------------------------------------------------------------------
  # test for expecting stop when neither (welfare and wieght) nor params (as output of pipster::pipgd_params) are provided
  test_params <- params
  test_params$gd_params$lq$reg_results$coef <- NULL
  pipgd_validate_lorenz(welfare = NULL, weight = NULL, params = test_params) |>
    expect_error()

  # Names in output list -------------------------------------------------------------------
  expect_equal(names(res_with_welfare_weight), names(res_with_params))
  expect_equal(names(res_with_params), "gd_params")

  names(res_with_params$gd_params) |>
    expect_equal(c("lq", "lb"))

  names(res_with_params$gd_params$lq) |>
    expect_equal("validity")

  names(res_with_params$gd_params$lq$validity) |>
    expect_equal(c("is_normal", "is_valid", "headcount"))

  names(res_with_params$gd_params$lb$validity) |>
    expect_equal(c("is_valid", "is_normal", "headcount"))

})

# Test pipgd_select_lorenz function ####
test_that("pipgd_select_lorenz", {
  
  res_with_params <-
    pipgd_params(welfare = pip_gd$L,
                 weight = pip_gd$P) |>
    pipgd_validate_lorenz(complete = TRUE) |>
    pipgd_select_lorenz()
    
  res_with_welfare_weight <- pipgd_select_lorenz(welfare = pip_gd$L,
                              weight = pip_gd$P)
  
  expect_equal(res_with_params, res_with_welfare_weight)
  expect_equal(class(res_with_params), "list")

  # Inputs -----------------------------------------------------------------------------------------
  expect_error(pipgd_select_lorenz(welfare = NULL, weight=NULL, params= NULL)) 
  # "Either `welfare` and `weights` should be specified or `params` should be output from `pipster::pipgd_params()`")

  # Names in output list ---------------------------------------------------------------------------
  names(res_with_params) |>
    expect_equal("selected_lorenz")

  names(res_with_params$selected_lorenz) |>
    expect_equal(c("for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov"))

  expect_equal(names(res_with_params), names(res_with_welfare_weight))

})

# Test pipgd_lorenz_curve function ####
test_that("pipgd_lorenz_curve", {
  welfare = pip_gd$L
  weight = pip_gd$P
  params <- pipgd_select_lorenz(welfare = welfare, weight = weight)

  expect_equal(class(pipgd_lorenz_curve(welfare = welfare, weight = weight)), "list")

  # Lorenz input ----------------------------------------------------------------------
  lorenz <- "lq"
  res_with_lorenz <- pipgd_lorenz_curve(welfare = welfare, weight = weight, lorenz = lorenz)
  expect_equal(res_with_lorenz$lorenz_curve$lorenz, lorenz)

  res_null_lorenz <- pipgd_lorenz_curve(welfare = welfare, weight = weight, lorenz = NULL)
  expect_equal(res_null_lorenz$lorenz_curve$lorenz, params$selected_lorenz$for_dist)

  invalid_lorenz <- "neither lq nor lb nor null"
  pipgd_lorenz_curve(welfare = welfare, weight = weight, lorenz = invalid_lorenz) |>
    expect_error()

  # Names in output list ---------------------------------------------------------------
  names(res_with_lorenz) |>
    expect_equal("lorenz_curve")
  
  names(res_with_lorenz$lorenz_curve) |>
    expect_equal(c("output", "points", "lorenz"))

})
