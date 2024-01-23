# Testing functions related to distributional measures on micro data

welfare = pip_md_s$welfare
weight = pip_md_s$weight

welfare_test <- welfare
welfare_test[1] = NA

weight_test <- weight
weight_test[1] = NA

# Testing quantile at specified shared of population function ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_quantile -arguments", {
  
  pipmd_quantile(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_qunatile(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_quantile(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_quantile(welfare = welfare) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_quantile(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error("Either `n` or `popshare` must be non-NULL")
  
  pipmd_quantile(welfare = welfare, weight = weight, n = NULL, popshare = c(0.3, 0.5)) |>
    expect_no_error()
  
  pipmd_quantile(welfare = welfare, weight = weight, n = NULL, popshare = 0.6) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_quantile -outputs", {
  n = 6

  res_list <- pipmd_quantile(welfare = welfare, weight = weight, n = n, format = "list") 
  res_atomic <- pipmd_quantile(welfare = welfare, weight = weight, n = n, format = "atomic") 
  res_dt <- pipmd_quantile(welfare = welfare, weight = weight, n = n, format = "dt") 

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_quantile_values(
        welfare    = welfare,
        weight     = weight,
        n          = n,
        format     = "list")
        )
  
  res_atomic[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$values[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atomic) |>
    expect_equal("numeric")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  names(res_list) |>
    expect_equal(c("16.6%", "33.3%", "50%", "66.6%", "83.3%", "99.9%"))

  length(res_list) |>
    expect_equal(n)
  
  names(res_dt) |>
    expect_equal(c("quantile", "values"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atomic) |>
    expect_equal(names(res_list))
  
  length(res_atomic) |>
    expect_equal(length(res_list))

})

# Testing Welfare share by quantile function ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_welfare_share_at -arguments", {
  
  pipmd_welfare_share_at(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_welfare_share_at(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_welfare_share_at(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_welfare_share_at(welfare = welfare) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error("Either `n` or `popshare` must be non-NULL")
  
  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = c(0.3, 0.5)) |>
    expect_no_error()
  
  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = 0.6) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_welfare_share_at -outputs", {
  n = 6

  res_list <- pipmd_welfare_share_at(welfare = welfare, weight = weight, n = n, format = "list") 
  res_atomic <- pipmd_welfare_share_at(welfare = welfare, weight = weight, n = n, format = "atomic") 
  res_dt <- pipmd_welfare_share_at(welfare = welfare, weight = weight, n = n, format = "dt") 

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_welfare_share_at(
        welfare    = welfare,
        weight     = weight,
        n          = n,
        format     = "list")[[1]]
        )
  
  res_atomic[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$share_at[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atomic) |>
    expect_equal("numeric")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  names(res_list) |>
    expect_equal(c("16.6%", "33.3%", "50%", "66.6%", "83.3%", "99.9%"))

  length(res_list) |>
    expect_equal(n)
  
  names(res_dt) |>
    expect_equal(c("quantile", "share_at"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atomic) |>
    expect_equal(names(res_list))
  
  length(res_atomic) |>
    expect_equal(length(res_list))

})

# Testing the function computing the share of welfare held by a particular quantile ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_quantile_welfare_share -arguments", {
  
  pipmd_quantile_welfare_share(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_quantile_welfare_share(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_quantile_welfare_share(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_quantile_welfare_share(welfare = welfare) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error("Either `n` or `popshare` must be non-NULL")
  
  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = c(0.3, 0.5)) |>
    expect_no_error()
  
  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = 0.6) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_quantile_welfare_share -outputs", {
  n = 6

  res_list <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "list") 
  res_atomic <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "atomic") 
  res_dt <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "dt") 

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_quantile_welfare_share(
        welfare    = welfare,
        weight     = weight,
        n          = n,
        format     = "list")[[1]]
        )
  
  res_atomic[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$share_at[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atomic) |>
    expect_equal("array")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  length(res_list) |>
    expect_equal(n)
  
  names(res_dt) |>
    expect_equal(c("quantile", "share_at"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atomic) |>
    expect_equal(names(res_list))
  
  length(res_atomic) |>
    expect_equal(length(res_list))

})

# Testing the Gini coefficient function ####
# Testing the function computing the share of welfare held by a particular quantile ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_gini -arguments", {
  
  pipmd_gini(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_gini(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_gini(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_gini(welfare = welfare) |>
    expect_message("each observation assigned equal weight")

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_gini -outputs", {
  res_list <- pipmd_gini(welfare = welfare, weight = weight, format = "list")
  res_atom <- pipmd_gini(welfare = welfare, weight = weight, format = "atomic")
  res_dt <- pipmd_gini(welfare = welfare, weight = weight, format = "dt")

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_compute_gini(
        welfare = welfare,
        weight  = weight)
        )
  
  res_atom[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$value[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atomic) |>
    expect_equal("array")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  length(res_list) |>
    expect_equal(1)
  
  names(res_list) |>
    expect_equal("gini")
  
  names(res_dt) |>
    expect_equal(c("indicator", "value"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atom) |>
    expect_equal(names(res_list))
  
  length(res_atom) |>
    expect_equal(length(res_list))

})

# Testing Wolfson polarization index function ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_polarization -arguments", {
  
  pipmd_polarization(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_polarization(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_polarization(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_polarization(welfare = welfare) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_polarization(welfare = welfare, weight = weight, mean = NULL, median = NULL) |>
    expect_no_error()
  
  pipmd_polarization(welfare = welfare, weight = weight, gini = NULL) |>
    expect_no_error()
  
})

# Outputs ---------------------------------------------------------------
test_that("pipmd_polarization -outputs", {
  median <- fquantile(
      x     = welfare,
      w     = weight,
      probs = 0.5)
  
  mean <- weighted.mean(
      x = welfare,
      w = weight)
  
  gini <- pipmd_gini(
      welfare = welfare,
      weight  = weight,
      format  = "atomic")
  
  res_list <- pipmd_polarization(welfare = welfare, weight = weight, format = "list")
  res_atom <- pipmd_polarization(welfare = welfare, weight = weight, format = "atomic")
  res_dt <- pipmd_polarization(welfare = welfare, weight = weight, format = "dt")

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_compute_polarization(
        welfare = welfare,
        weight  = weight,
        gini    = gini,
        mean    = mean,
        median  = median)[[1]]
        )
  
  res_atom[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$value[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atom) |>
    expect_equal("numeric")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  length(res_list) |>
    expect_equal(1)
  
  names(res_list) |>
    expect_equal("polarization")
  
  names(res_dt) |>
    expect_equal(c("indicator", "value"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atom) |>
    expect_equal(names(res_list))
  
  length(res_atom) |>
    expect_equal(length(res_list))
})

# Testing Mean Log Deviation function ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_mld -arguments", {
  
  pipmd_mld(welfare = welfare_test, weight = weight) |>
    expect_error("no elements in welfare can be NAs")
  
  pipmd_mld(welfare = NULL, weight = weight) |>
    expect_error("welfare cannot be NULL")
  
  pipmd_mld(welfare = welfare, weight = weight_test) |>
    expect_error("no elements in weight can be NAs")
  
  pipmd_mld(welfare = welfare) |>
    expect_message("each observation assigned equal weight")
  
  pipmd_mld(welfare = welfare, weight = weight, mean = NULL) |>
    expect_no_error()
  
})

# Outputs ---------------------------------------------------------------
test_that("pipmd_mld -outputs", {

  mean <- weighted.mean(
      x = welfare,
      w = weight)

  res_list <- pipmd_mld(welfare = welfare, weight = weight, format = "list")
  res_atom <- pipmd_mld(welfare = welfare, weight = weight, format = "atomic")
  res_dt <- pipmd_mld(welfare = welfare, weight = weight, format = "dt")

  # Check computations 
  res_list[[1]] |>
    expect_equal(
      wbpip::md_compute_mld(
        welfare = welfare,
        weight  = weight,
        mean    = mean)[[1]]
        )
  
  res_atom[[1]] |>
    expect_equal(res_list[[1]])
  
  res_dt$value[1] |>
    expect_equal(res_list[[1]])
  
  # Check output class
  class(res_list) |>
    expect_equal("list")
  
  class(res_atom) |>
    expect_equal("numeric")
  
  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  length(res_list) |>
    expect_equal(1)
  
  names(res_list) |>
    expect_equal("mld")
  
  names(res_dt) |>
    expect_equal(c("indicator", "value"))
  
  nrow(res_dt) |>
    expect_equal(length(res_list))
  
  names(res_atom) |>
    expect_equal(names(res_list))
  
  length(res_atom) |>
    expect_equal(length(res_list))

})






  



  
