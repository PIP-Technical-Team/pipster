 # Increasing the max number of failures allowed
 testthat::set_max_fails(Inf) 

# pipgd_params function ####
test_that("pipgd_params output", {
  res <- pipgd_params(
    welfare = pip_gd$L,
    weight = pip_gd$P,
    mean = NULL,
    population = NULL)
  # Class --------------------------------
  class(res) |>
    expect_equal("pipgd_params")

  # Names --------------------------------
  names(res) |>
    expect_equal(c("gd_params", "data"))

  names(res$gd_params) |>
    expect_equal(c("lq", "lb"))


  names(res$gd_params$lq) |>
    expect_equal(names(res$gd_params$lb))

  names(res$gd_params$lq) |>
    expect_equal(c("reg_results", "key_values"))

  names(res$gd_params$lq$reg_results) |>
    expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

  names(res$gd_params$lq$reg_results$coef) |>
    expect_equal(names(res$gd_params$lb$reg_results$coef))

  names(res$gd_params$lq$reg_results$coef) |>
    expect_equal(c("A", "B", "C"))

  names(res$gd_params$lq$key_values) |>
    expect_equal(c("e", "m", "n", "r", "s1", "s2"))

  names(res$gd_params$lb$reg_results$coef) |>
    expect_equal(c("A", "B", "C"))

  names(res$gd_params$lb$key_values) |>
    expect_equal(NULL)

  names(res$data) |>
    expect_equal(c("welfare", "weight"))

  mean_value <- 5
  pop_value <- 1000

  res_with_mean_pop <- pipgd_params(
    welfare = pip_gd$L,
    weight = pip_gd$P,
    mean = mean_value,
    population = pop_value)

  class(res_with_mean_pop) |>
    expect_equal("pipgd_params")

  names(res_with_mean_pop$data) |>
    expect_equal(c("welfare", "weight", "mean", "population"))

  expect_true(!is.null(res_with_mean_pop$data$mean))
  expect_true(!is.null(res_with_mean_pop$data$population))
  expect_equal(res_with_mean_pop$data$mean, mean_value)
  expect_equal(res_with_mean_pop$data$population, pop_value)

})

# check_pipgd_params function ####

test_that("check_pipgd_params aborts on invalid params", {
  # Params ---------------------------
  lp <- pipgd_params(welfare = pip_gd$L, weight = pip_gd $P)$params
  nlp <- names(lp)
  res <- check_pipgd_params(lp)
  
  if ("params" %in% nlp) {
    if (!is.null(lp$params) && inherits(lp$params, "pipgd_params")) {
      expect_silent(check_pipgd_params(lp))
    }}

  # Welfare and params ----------------
  if ( all(c("params", "welfare") %in% nlp)) {
    if (is.null(lp$params) &&
        (!is.null(lp$welfare)  || !is.null(lp$population))) {
      expect_stop("You must specify either {.field params} or
                {.field welfare} and {.field population}")
    }
  }

  # povline and popshare --------------
  if ( all(c("povline", "popshare") %in% nlp)) {
    if (!is.na(lp$povline) && !is.null(lp$popshare)) {
      expect_stop("You must specify either {.field povline} or
                {.field popshare}")
    }
  }

  # lorenz -----------------------------
  if ( all(c("lorenz") %in% nlp)) {

    if (!is.null(lp$lorenz) && !lp$lorenz %in% c("lq", "lb")) {

      expect_stop("{.field lorenz} must be either 'lq' or 'lb', or
                {.code NULL} to let the algorithm select")}
                }
})

