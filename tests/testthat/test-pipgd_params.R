 # Increasing the max number of failures allowed
 #testthat::set_max_fails(Inf)

# pipgd_params function ####
test_that("pipgd_params output", {

  res <- pipgd_params(
    welfare    = pip_gd$L,
    weight     = pip_gd$P,
    mean       = NULL,
    population = NULL)

  # Class --------------------------------
  class(res) |>
    expect_equal("pipgd_params")

  # Names --------------------------------
  names(res) |>
    expect_equal(c("gd_params",
                   "data"))

  names(res$gd_params) |>
    expect_equal(c("lq",
                   "lb"))


  names(res$gd_params$lq) |>
    expect_equal(names(res$gd_params$lb))

  names(res$gd_params$lq) |>
    expect_equal(c("reg_results",
                   "key_values"))

  names(res$gd_params$lq$reg_results) |>
    expect_equal(c("ymean",
                   "sst",
                   "coef",
                   "sse",
                   "r2",
                   "mse",
                   "se"))

  names(res$gd_params$lq$reg_results$coef) |>
    expect_equal(names(res$gd_params$lb$reg_results$coef))

  names(res$gd_params$lq$reg_results$coef) |>
    expect_equal(c("A",
                   "B",
                   "C"))

  names(res$gd_params$lq$key_values) |>
    expect_equal(c("e",
                   "m",
                   "n",
                   "r",
                   "s1",
                   "s2"))

  names(res$gd_params$lb$reg_results$coef) |>
    expect_equal(c("A",
                   "B",
                   "C"))

  names(res$gd_params$lb$key_values) |>
    expect_equal(NULL)

  names(res$data) |>
    expect_equal(c("welfare",
                   "weight"))

  mean_value <- 5
  pop_value  <- 1000

  res_with_mean_pop <- pipgd_params(
    welfare    = pip_gd$L,
    weight     = pip_gd$P,
    mean       = mean_value,
    population = pop_value)

  class(res_with_mean_pop) |>
    expect_equal("pipgd_params")

  names(res_with_mean_pop$data) |>
    expect_equal(c("welfare",
                   "weight",
                   "mean",
                   "population"))

  expect_true(!is.null(res_with_mean_pop$data$mean))
  expect_true(!is.null(res_with_mean_pop$data$population))
  expect_equal(res_with_mean_pop$data$mean,
               mean_value)
  expect_equal(res_with_mean_pop$data$population,
               pop_value)

})

# check_pipgd_params function ####

test_that("check_pipgd_params aborts on invalid params", {

   # Params ---------------------------
  lp <- list(params = list(some_value = 123))
  expect_error(check_pipgd_params(lp))

  # Welfare and params ----------------
  lp <- list(
    params     = structure(c(1, 2, 3), class = "pipgd_params"),
    welfare    = NULL,
    weight = c(1, 2, 3) # This can be omitted or included
  )
  expect_error(check_pipgd_params(lp))

  # povline and popshare --------------
  lp <- list(
    params     = structure(c(1, 2, 3), class = "pipgd_params"),
    povline    = 2,
    popshare   = 0.3
  )
  expect_error(check_pipgd_params(lp))

  lp <- list(
    params     = structure(c(1, 2, 3), class = "pipgd_params"),
    popshare   = -0.3
  )
  expect_error(check_pipgd_params(lp))
  lp <- list(
    params     = structure(c(1, 2, 3), class = "pipgd_params"),
    popshare   = 1.3
  )
  expect_error(check_pipgd_params(lp))
  # Lorenz --------------
  lp <- list(
    params     = structure(c(1, 2, 3), class = "pipgd_params"),
    lorenz     = "not lb or lq"
  )
  expect_error(check_pipgd_params(lp))


})

