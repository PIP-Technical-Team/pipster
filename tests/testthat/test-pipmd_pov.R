# Testing functions related to poverty measures (for micro data) 
welfare_s = pip_md_s$welfare
weight_s = pip_md_s$weight

# Testing poverty headcount function (non vectorized) ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_pov_headcount_nv arguments work as expected", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA
  
  pipmd_pov_headcount_nv(welfare = welfare_test, weight = weight_s) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_pov_headcount_nv(welfare = NULL, weight = weight_s) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_pov_headcount_nv(welfare = welfare_s) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_s, povline = "non numeric povline") |>
    expect_error()
  
  pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_s, povline = NULL) |>
    expect_error()
  
  pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_s, povline = -0.5) |>
    expect_message()
  
  pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_s, povline = 19) |>
    expect_message()

})

# Computations ---------------------------------------------------------------
test_that("pipmd_pov_headcount_nv outputs work as expected", {
  res <- pipmd_pov_headcount_nv(welfare = welfare_s, weight = weight_s)

  class(res) |>
    expect_equal("list")
  
  length(res) |>
    expect_equal(1)
  
  names(res) |>
    expect_equal("pov_headcount")
  
  res$pov_headcount |>
    expect_equal(wbpip::md_compute_poverty_stats(
    welfare      = welfare_s,
    weight       = weight_s,
    povline_lcu  = 1)$headcount)

})

# Testing poverty headcount function (vectorized) ####

# When format is "list" --------------------------------------------------------------------------------

test_that("pipmd_pov_headcount work as expected -when format is list", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  povline <- c(0.5, 1, 2)
  format = "list"

  # Arguments
  pipmd_pov_headcount(welfare = welfare_test, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = NULL, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_test, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, povline = povline, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = "non numeric povline", format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = NULL, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = -0.5, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = 19, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) |>
    expect_no_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <-   pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) 

  class(res) |>
    expect_equal("list")
  
  names(res) |>
    expect_equal(c("pl0.5", "pl1", "pl2"))
  
  length(res) |>
    expect_equal(length(povline))
  
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
  pipmd_pov_headcount(welfare = welfare_test, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = NULL, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_test, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, povline = povline, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = "non numeric povline", format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = NULL, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = -0.5, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = 19, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) |>
    expect_no_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <-   pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) 

  #class(res) |>
  #  expect_equal("data.table")
  
  names(res) |>
    expect_equal(c("povline", "pov_headcount"))
  
  nrow(res) |>
    expect_equal(length(povline))
  
})

# When format is "atomic" -------------------------------------------------
test_that("pipmd_pov_headcount work as expected -when format is atomic", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  povline <- c(0.5, 1, 2)
  format = "atomic"

  # Arguments
  pipmd_pov_headcount(welfare = welfare_test, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = NULL, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_test, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, povline = povline, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = "non numeric povline", format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = NULL, format = format) |>
    expect_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = -0.5, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = 19, format = format) |>
    expect_message()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) |>
    expect_no_error()
  
  pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = "neither atomic, dt or list") |>
    expect_error()

  # Outputs
  res <-   pipmd_pov_headcount(welfare = welfare_s, weight = weight_s, povline = povline, format = format) 

  class(res) |>
    expect_equal("numeric")
  
  names(res) |>
    expect_equal(c("pl0.5", "pl1", "pl2" ))
  
  length(res) |>
    expect_equal(length(povline))
  
})

# Testing poverty gap function (non vectorized) ####
# Arguments --------------------------------------------------------
test_that("pipmd_pov_gap_nv arguments work as expected", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA
  
  pipmd_pov_gap_nv(welfare = welfare_test, weight = weight_s) |>
    expect_error()
  
  pipmd_pov_gap_nv(welfare = NULL, weight = weight_s) |>
    expect_error()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_test) |>
    expect_error()
  
  pipmd_pov_gap_nv(welfare = welfare_s) |>
    expect_message()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s, povline = NULL) |>
    expect_error()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s, povline "not a number") |>
    expect_error()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s, povline = 0.2) |>
    expect_message()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s, povline = 19) |>
    expect_message()
  
  pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s, povline = 0.5) |>
    expect_no_error()

})

# Outputs --------------------------------------------------------
test_that("pipmd_pov_gap_nv outputs work as expected", {
  res <- pipmd_pov_gap_nv(welfare = welfare_s, weight = weight_s)

  class(res) |>
    expect_equal("list")
  
  names(res) |>
    expect_equal("pov_gap")

  res$pov_gap |>
    expect_equal(wbpip:::md_compute_poverty_stats(
    welfare      = welfare_s,
    weight       = weight_s,
    povline_lcu  = 1
  )$poverty_gap)

})

# Testing poverty gap function (vectorized) ####
# Inputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_gap arguments work as expected", { 
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA

  povline <- c(0.5, 1, 2)
  format = "list"

  pipmd_pov_gap(welfare = welfare_test, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_gap(welfare = NULL, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_test, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_gap(welfare = welfare_s, povline = povline, format = format) |>
    expect_message()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = "non numeric povline", format = format) |>
    expect_error()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = NULL, format = format) |>
    expect_error()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = -0.5, format = format) |>
    expect_message()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = 19, format = format) |>
    expect_message()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format = format) |>
    expect_no_error()
  
  pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format = "neither atomic, dt or list") |>
    expect_error()
})

# Outputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_gap work as expected", {
  povline <- c(0.5, 1, 2)
  
  res_list <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format="list")
  res_dt <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format="dt")
  res_atom <- pipmd_pov_gap(welfare = welfare_s, weight = weight_s, povline = povline, format="atomic")

  # When format is "list"
  class(res_list) |>
    expect_equal("list")
  
  names(res_list) |>
    expect_equal(c("pl0.5", "pl1", "pl2"))
  
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
  #class(res_dt) |>
  #  expect_equal("data.table")

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))
  
})

# Testing poverty severity function (non vectorized) ####
# Arguments ----------------------------------------------------------------------------------
test_that("pipmd_pov_severity_nv arguments work as expected", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA
  
  pipmd_pov_severity_nv(welfare = welfare_test, weight = weight_s) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_pov_severity_nv(welfare = NULL, weight = weight_s) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_pov_severity_nv(welfare = welfare_s) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_s, povline = "non numeric povline") |>
    expect_error()
  
  pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_s, povline = NULL) |>
    expect_error()
  
  pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_s, povline = -0.5) |>
    expect_message()
  
  pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_s, povline = 19) |>
    expect_message()

})

# Output ------------------------------------------------------------------
test_that("pipmd_pov_severity_nv outputs work as expected", {
  res <- pipmd_pov_severity_nv(welfare = welfare_s, weight = weight_s)

  class(res) |>
    expect_equal("list")
  
  length(res) |>
    expect_equal(1)
  
  names(res) |>
    expect_equal("pov_severity")
  
  res$pov_severity |>
    expect_equal(wbpip::md_compute_poverty_stats(
    welfare      = welfare_s,
    weight       = weight_s,
    povline_lcu  = 1)$poverty_severity)

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

  pipmd_pov_severity(welfare = welfare_test, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_severity(welfare = NULL, weight = weight_s, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_test, povline = povline, format = format) |>
    expect_error()
  
  pipmd_pov_severity(welfare = welfare_s, povline = povline, format = format) |>
    expect_message()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = "non numeric povline", format = format) |>
    expect_error()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = NULL, format = format) |>
    expect_error()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = -0.5, format = format) |>
    expect_message()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = 19, format = format) |>
    expect_message()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format = format) |>
    expect_no_error()
  
  pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format = "neither atomic, dt or list") |>
    expect_error()
})

# Outputs --------------------------------------------------------------------------------------------------
test_that("pipmd_pov_severity work as expected", {
  povline <- c(0.5, 1, 2)
  
  res_list <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="list")
  res_dt <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="dt")
  res_atom <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="atomic")

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
  #class(res_dt) |>
  #  expect_equal("data.table")
  names(res_dt) |>
    expect_equal(c("povline", "pov_severity"))

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))
  
})

# Testing watts index function (non vectorized) ####
# Arguments ----------------------------------------------------------------------------------
test_that("pipmd_watts_nv arguments work as expected", {
  welfare_test <- welfare_s 
  welfare_test[1] = NA

  weight_test <- weight_s
  weight_test[1] = NA
  
  pipmd_watts_nv(welfare = welfare_test, weight = weight_s) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_watts_nv(welfare = NULL, weight = weight_s) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_watts_nv(welfare = welfare_s, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_watts_nv(welfare = welfare_s) |>
    expect_message("each observation assigned equal weight")
  
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

  class(res) |>
    expect_equal("list")
  
  length(res) |>
    expect_equal(1)
  
  names(res) |>
    expect_equal("watts")
  
  res$watts |>
    expect_equal(wbpip::md_compute_poverty_stats(
    welfare      = welfare_s,
    weight       = weight_s,
    povline_lcu  = 1)$watts)

})

# Testing watts index function (vectorized) ####
test_that("pipmd_pov_severity work as expected", {
  povline <- c(0.5, 1, 2)
  
  res_list <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="list")
  res_dt <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="dt")
  res_atom <- pipmd_pov_severity(welfare = welfare_s, weight = weight_s, povline = povline, format="atomic")

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
  #class(res_dt) |>
  #  expect_equal("data.table")
  names(res_dt) |>
    expect_equal(c("povline", "pov_severity"))

  # When format is "atomic"

  class(res_atom) |>
    expect_equal("numeric")

  length(res_atom) |>
    expect_equal(length(povline))
  
})








