# Testing utils functions

welfare = pip_gd$L
weight = pip_gd$P
povline = 1
mean = 1
times_mean = 1
lorenz = "lq"

pipgd_pov_gap_v <- Vectorize(pipgd_pov_gap_nv,
                               vectorize.args = "povline",
                               SIMPLIFY = FALSE)

pipmd_pov_headcount_v <- Vectorize(
    FUN            = pipmd_pov_headcount_nv,
    vectorize.args = "povline",
    SIMPLIFY       = FALSE
  )

data_list_gd <- pipgd_pov_gap_v(welfare    = welfare,
                       weight     = weight,
                       params     = NULL,
                       povline    = povline,
                       complete   = TRUE,
                       lorenz     = lorenz,
                       mean       = mean,
                       times_mean = times_mean)

welfare_md = pip_md$welfare
weight_md = pip_md$weight

data_list_md <- pipmd_pov_headcount_v(
    welfare    = welfare_md,
    weight     = weight_md,
    povline    = povline)


# Test return format function (for group data) ####
# When format is "list" -------------------------------------------------------------
test_that("return format works -when format is list", {
  format = "list"

  res <- return_format(data_list_gd,
                       var = "pov_gap",
                       povline = povline,
                       complete = TRUE,
                       format = format)

  # Check output class
  class(res) |>
    expect_equal("list")

  # Check output names
  names(res) |>
    expect_equal("pl1")

})

# When format is "atomic" -------------------------------------------------------------
test_that("return format works -when format is atomic", {
  format = "atomic"

  res <- return_format(data_list_gd,
                       var = "pov_gap",
                       povline = povline,
                       format = format)

  # Check output class
  class(res) |>
    expect_equal("numeric")

  # Check complete = TRUE gives error
  return_format(data_list_gd,
                       var = "pov_gap",
                       povline = povline,
                       complete = TRUE,
                       format = format) |>
    expect_error()

  # Check output names
  names(res) |>
    expect_equal(lorenz)

})

# When format is "dt" -------------------------------------------------------------
test_that("return format works -when format is dt", {
  format = "dt"

  res <- return_format(data_list_gd,
                       var = "pov_gap",
                       povline = povline,
                       format = format)

  #class(res) |>
  #  expect_equal("data.table") & expect_equal("data.frame")

  return_format(data_list_gd,
                       var = "pov_gap",
                       povline = povline,
                       complete = TRUE,
                       format = format) |>
    expect_error()

  # check names in output
  names(res) |>
    expect_equal(c("povline", "pov_gap", "lorenz" ))

})

# Test return format md function (for micro data) ####
# When format is "list" -------------------------------------------------------------

test_that("return_format_md works as expected ", {
  format = "list"

  res <- return_format_md(
    ld      = data_list_md,
    var     = "pov_headcount",
    format  = format,
    povline = povline)

  class(res) |>
    expect_equal("list")

  names(res) |>
    expect_equal("pl1")

  names(res$pl1) |>
    expect_equal("pov_headcount")

})

# When format is "atomic" -------------------------------------------------------------
test_that("return format md works -when format is atomic", {
  format = "atomic"

  res <- return_format_md(data_list_md,
                       var = "pov_gap",
                       povline = povline,
                       format = format)

  # Check output class
  class(res) |>
    expect_equal("numeric")

  # Check complete = TRUE gives error
  return_format(data_list_md,
                       var = "pov_gap",
                       povline = povline,
                       complete = TRUE,
                       format = format) |>
    expect_error()

  # Check output names
  names(res) |>
    expect_equal("pl1")

})

# When format is "dt" -------------------------------------------------------------
test_that("return format md works -when format is dt", {
  format = "dt"

  res <- return_format_md(data_list_md,
                       var = "pov_gap",
                       povline = povline,
                       format = format)

  #class(res) |>
  #  expect_equal("data.table") & expect_equal("data.frame")

  return_format(data_list_md,
                       var = "pov_gap",
                       povline = povline,
                       complete = TRUE,
                       format = format) |>
    expect_error()

  # check names in output
  names(res) |>
    expect_equal(c("povline", "pov_gap"))

})
