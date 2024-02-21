# Testing functions related to distributional measures on micro data

welfare = pip_md_s$welfare
weight  = pip_md_s$weight

welfare = pip_md$welfare
weight  = pip_md$weight

welfare = pip_md_s$welfare
weight  = pip_md_s$weight



welfare_test <- welfare
welfare_test[1] = NA

weight_test <- weight
weight_test[1] = NA

attributes(welfare) <- NULL
attributes(weight)  <- NULL


# Testing quantile at specified shared of population function ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_quantile -arguments", {

  pipmd_quantile(welfare = welfare_test, weight = weight) |>
    expect_error()

  pipmd_quantile(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_quantile(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_quantile(welfare = welfare) |>
  #   expect_message()

  pipmd_quantile(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error()

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

  # Benchmarks
  res_bm_list <- list("16.6%" = 1.15720864490713, "33.3%" = 1.71560515751511,
                      "50%" = 2.35138374470022, "66.6%" = 3.48346293082367,
                      "83.3%" = 5.27090655645221, "99.9%" = 18.019516550669)
  res_bm_atomic <- c("16.6%" = 1.15720864490713, "33.3%" = 1.71560515751511, "50%" = 2.35138374470022,
                    "66.6%" = 3.48346293082367, "83.3%" = 5.27090655645221, "99.9%" = 18.019516550669)
  res_bm_dt <- structure(list(quantile = c("q_16.6%", "q_33.3%", "q_50%", "q_66.6%", "q_83.3%", "q_99.9%"),
                              values = c(1.15720864490713, 1.71560515751511, 2.35138374470022, 3.48346293082367, 5.27090655645221, 18.019516550669)),
                              row.names = c(NA, -6L), class = c("data.table", "data.frame"))

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

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
    expect_error()

  pipmd_welfare_share_at(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_welfare_share_at(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_welfare_share_at(welfare = welfare) |>
  #   expect_message()

  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error()

  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = c(0.3, 0.5)) |>
    expect_no_error()

  pipmd_welfare_share_at(welfare = welfare, weight = weight, n = NULL, popshare = 0.6) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_welfare_share_at -outputs", {
  skip()
  n = 6

  res_list <-
    pipmd_welfare_share_at(
      welfare = welfare,
      weight = weight,
      n = n,
      format = "list"
    )
  res_atomic <-
    pipmd_welfare_share_at(
      welfare = welfare,
      weight = weight,
      n = n,
      format = "atomic"
    )
  res_dt <-
    pipmd_welfare_share_at(
      welfare = welfare,
      weight = weight,
      n = n,
      format = "dt"
    )


  res_bm_list <-
    list(
      "16.6%" = 0.179898119766951,
      "33.3%" = 0.334554080564441,
      "50%" = 0.498891556443476,
      "66.6%" = 0.671063477546949,
      "83.3%" = 0.833064761190862,
      "99.9%" = 1
    )
  res_bm_atomic <-
    c(
      "16.6%" = 0.179898119766951,
      "33.3%" = 0.334554080564441,
      "50%" = 0.498891556443476,
      "66.6%" = 0.671063477546949,
      "83.3%" = 0.833064761190862,
      "99.9%" = 1
    )
  res_bm_dt <-
    structure(
      list(
        quantile = c("q_16.6%", "q_33.3%", "q_50%", "q_66.6%", "q_83.3%", "q_99.9%"),
        share_at = c(
          0.179898119766951,
          0.334554080564441,
          0.498891556443476,
          0.671063477546949,
          0.833064761190862,
          1
        )
      ),
      row.names = c(NA, -6L),
      class = c("data.table", "data.frame")
    )

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

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
    expect_error()

  pipmd_quantile_welfare_share(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_quantile_welfare_share(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_quantile_welfare_share(welfare = welfare) |>
  #   expect_message()

  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = NULL) |>
    expect_error()

  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = c(0.3, 0.5)) |>
    expect_no_error()

  pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = NULL, popshare = 0.6) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_quantile_welfare_share -outputs", {
  skip()
  n = 6

  res_list <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "list")
  res_atomic <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "atomic")
  res_dt <- pipmd_quantile_welfare_share(welfare = welfare, weight = weight, n = n, format = "dt")

  res_bm_list <- list("16.6666666666667%" = 0.041993366907175, "33.3333333333333%" = 0.0724995422851018, "50%" = 0.11959445511606, "66.6666666666667%" = 0.169235324964795,
                      "83.3333333333333%" = 0.217187544524222, "100%" = 0.379489766202646)
  res_bm_atomic <- structure(c("16.6666666666667%" = 0.041993366907175, "33.3333333333333%" = 0.0724995422851018, "50%" = 0.11959445511606, "66.6666666666667%" = 0.169235324964795, "83.3333333333333%" = 0.217187544524222, "100%" = 0.379489766202646),
                            dim = 6L,
                            dimnames = list(c("16.6666666666667%", "33.3333333333333%", "50%", "66.6666666666667%", "83.3333333333333%", "100%")))
  res_bm_dt <- structure(list(quantile = c("q_16.6666666666667%", "q_33.3333333333333%", "q_50%", "q_66.6666666666667%", "q_83.3333333333333%", "q_100%"),
                              share_at = c(0.041993366907175, 0.0724995422851018, 0.11959445511606, 0.169235324964795, 0.217187544524222, 0.379489766202646)),
                              row.names = c(NA, -6L), class = c("data.table", "data.frame"))

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

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
    expect_error()

  pipmd_gini(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_gini(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_gini(welfare = welfare) |>
  #   expect_message()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_gini -outputs", {
  res_list <- pipmd_gini(welfare = welfare, weight = weight, format = "list")
  res_atomic <- pipmd_gini(welfare = welfare, weight = weight, format = "atomic")
  res_dt <- pipmd_gini(welfare = welfare, weight = weight, format = "dt")

  res_bm_list <- list(gini = 0.41905333648877)
  res_bm_atomic <- c(gini = 0.41905333648877)
  res_bm_dt <- structure(list(indicator = "gini", value = 0.41905333648877),
                              row.names = c(NA, -1L), class = c("data.table", "data.frame"))

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

  # Check output class
  class(res_list) |>
    expect_equal("list")

  class(res_atomic) |>
    expect_equal("numeric")

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

  names(res_atomic) |>
    expect_equal(names(res_list))

  length(res_atomic) |>
    expect_equal(length(res_list))

})

# Testing Wolfson polarization index function ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_polarization -arguments", {

  pipmd_polarization(welfare = welfare_test, weight = weight) |>
    expect_error()

  pipmd_polarization(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_polarization(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_polarization(welfare = welfare) |>
  #   expect_message()

  pipmd_polarization(welfare = welfare, weight = weight, mean = NULL, median = NULL) |>
    expect_no_error()

  pipmd_polarization(welfare = welfare, weight = weight, gini = NULL) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_polarization -outputs", {
  median <- fmedian(
      x     = welfare,
      w     = weight)

  mean <- fmean(
      x = welfare,
      w = weight)

  gini <- pipmd_gini(
      welfare = welfare,
      weight  = weight,
      format  = "atomic")

  res_list <-
    pipmd_polarization(welfare = welfare,
                       weight = weight,
                       format = "list")
  res_atomic <-
    pipmd_polarization(welfare = welfare,
                       weight = weight,
                       format = "atomic")
  res_dt <-
    pipmd_polarization(welfare = welfare,
                       weight = weight,
                       format = "dt")

  res_bm_list <- list(polarization = 0.430115535839879)
  res_bm_atomic <- c(polarization = 0.430115535839879)
  res_bm_dt <-
    structure(
      list(indicator = "polarization", value = 0.430115535839879),
      row.names = c(NA, -1L),
      class = c("data.table", "data.frame")
    )

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic |>
    expect_equal(res_bm_atomic)

  res_dt |>
    expect_equal(res_bm_dt)

  # Check output class
  class(res_list) |>
    expect_equal("list")

  class(res_atomic) |>
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

  names(res_atomic) |>
    expect_equal(names(res_list))

})

# Testing Mean Log Deviation function ####
# Arguments ------------------------------------------------------------------------------
test_that("pipmd_mld -arguments", {

  pipmd_mld(welfare = welfare_test, weight = weight) |>
    expect_error()

  pipmd_mld(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_mld(welfare = welfare, weight = weight_test) |>
    expect_error()

  # pipmd_mld(welfare = welfare) |>
  #   expect_message()

  pipmd_mld(welfare = welfare, weight = weight, mean = NULL) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_mld -outputs", {

  mean <- collapse::fmean(
      x = welfare,
      w = weight)

  res_list <- pipmd_mld(welfare = welfare, weight = weight, format = "list")
  res_atomic <- pipmd_mld(welfare = welfare, weight = weight, format = "atomic")
  res_dt <- pipmd_mld(welfare = welfare, weight = weight, format = "dt")

  res_bm_list <- list(mld = 0.301620140444736)
  res_bm_atomic <- c(mld = 0.301620140444736)
  res_bm_dt <-  structure(list(indicator = "mld",
                               value = 0.301620140444736),
                          row.names = c(NA, -1L),
                          class = c("data.table", "data.frame"))

  # Check computations
  res_list |>
    expect_equal(res_bm_list)

  res_atomic[[1]] |>
    expect_equal(res_bm_atomic[[1]])

  res_dt |>
    expect_equal(res_bm_dt)

  # Check output class
  class(res_list) |>
    expect_equal("list")

  class(res_atomic) |>
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

  names(res_atomic) |>
    expect_equal(names(res_list))

  length(res_atomic) |>
    expect_equal(length(res_list))

})











