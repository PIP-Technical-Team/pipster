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
  #skip()
  pipmd_quantile(welfare = welfare_test, weight = weight) |>
    expect_error()

  pipmd_quantile(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_quantile(welfare = welfare, weight = weight_test) |>
    expect_error()

  pipmd_quantile(welfare = welfare) |>
    expect_no_error()

  pipmd_quantile(
    welfare = welfare,
    weight = weight,
    n = NULL,
    popshare = NULL
  ) |>
    expect_error()

  pipmd_quantile(
    welfare = welfare,
    weight = weight,
    n = NULL
  ) |>
    expect_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_quantile -outputs", {
  #skip()
  n = 5

  results <- c(1.246769, 1.934927, 2.979334, 4.737228, 18.019517)

  res_list <-
    pipmd_quantile(
      welfare = welfare,
      weight  = weight,
      n       = n,
      format  = "list"
    )
  res_atomic <-
    pipmd_quantile(
      welfare = welfare,
      weight = weight,
      n = n,
      format = "atomic"
    )
  res_dt <-
    pipmd_quantile(
      welfare = welfare,
      weight = weight,
      n = n,
      format = "dt"
    )


  # Complete objects
  res_list_com <- pipmd_quantile(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "list",
    complete = TRUE
  )
  res_dt_com <- pipmd_quantile(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "dt",
    complete = TRUE
  )
  res_atomic_com <- pipmd_quantile(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "atomic",
    complete = TRUE
  )


  # Benchmarks
  res_bm_list <-
    list(
      "20%" = results[1],
      "40%" = results[2],
      "60%" = results[3],
      "80%" = results[4],
      "100%" = results[5]
    )
  res_bm_atomic <-
    c(
      "20%"  = results[1],
      "40%"  = results[2],
      "60%"  = results[3],
      "80%"  = results[4],
      "100%" = results[5]
    )
  res_bm_dt <-
    structure(
      list(
        quantiles = c(
          "q_20%",
          "q_40%",
          "q_60%",
          "q_80%",
          "q_100%"
        ),
        values = results
      ),
      row.names = c(NA, -5L),
      class = c("data.table", "data.frame")
    )

  # Check computations
  res_list$dist_stats$quantiles |>
    expect_equal(res_bm_list, tolerance = 0.000001)

  res_atomic$dist_stats$quantiles |>
    expect_equal(res_bm_atomic, tolerance = 0.000001)

  res_dt$dist_stats$quantiles |>
    expect_equal(res_bm_dt, tolerance = 0.000001)

  # Check output class
  class(res_list$dist_stats$quantiles) |>
    expect_equal("list")

  class(res_atomic$dist_stats$quantiles) |>
    expect_equal("numeric")

  class(res_dt$dist_stats$quantiles) |>
   expect_equal(c("data.table", "data.frame"))

  # Check names in output
  names(res_list$dist_stats$quantiles) |>
    expect_equal(c("20%", "40%", "60%", "80%", "100%"))

  length(res_list$dist_stats$quantiles) |>
    expect_equal(n)

  names(res_dt$dist_stats$quantiles) |>
    expect_equal(c("quantiles", "values"))

  nrow(res_dt$dist_stats$quantiles) |>
    expect_equal(length(res_list$dist_stats$quantiles))

  names(res_atomic$dist_stats$quantiles) |>
    expect_equal(names(res_list$dist_stats$quantiles))

  length(res_atomic$dist_stats$quantiles) |>
    expect_equal(length(res_list$dist_stats$quantiles))

  # check "complete" arg
  res_list |>
    expect_equal(res_list_com$results)
  res_dt |>
    expect_equal(res_dt_com$results)
  res_atomic |>
    expect_equal(res_atomic_com$results)

})

# Testing Welfare share by quantile function ####
# Arguments ---------------------------------------------------------------
test_that("pipmd_welfare_share_at -arguments", {
  #skip()
  pipmd_welfare_share_at(welfare = welfare_test, weight = weight) |>
    expect_error()

  pipmd_welfare_share_at(welfare = NULL, weight = weight) |>
    expect_error()

  pipmd_welfare_share_at(welfare = welfare, weight = weight_test) |>
    expect_error()

  pipmd_welfare_share_at(welfare = welfare) |>
    expect_no_error()

  pipmd_welfare_share_at(
    welfare = welfare,
    weight = weight,
    n = NULL
  ) |>
    expect_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_welfare_share_at -outputs", {
  #skip()
  n = 5

  results <- wbpip::md_welfare_share_at(welfare = welfare,
                                        weight  = weight,
                                        n       = n,
                                        format  = "atomic")
  results <- as.numeric(results)
  # Not complete objects
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

  # Complete objects
  res_list_com <- pipmd_welfare_share_at(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "list",
    complete = TRUE
  )
  res_dt_com <- pipmd_welfare_share_at(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "dt",
    complete = TRUE
  )
  res_atomic_com <- pipmd_welfare_share_at(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "atomic",
    complete = TRUE
  )


  res_bm_list <-
    list(
      "20%"  = results[1],
      "40%"  = results[2],
      "60%"  = results[3],
      "80%"  = results[4],
      "100%" = results[5]
    )
  res_bm_atomic <-
    c(
      "20%"  = results[1],
      "40%"  = results[2],
      "60%"  = results[3],
      "80%"  = results[4],
      "100%" = results[5]
    )
  res_bm_dt <-
    structure(
      list(
        quantiles = c(
          "q_20%",
          "q_40%",
          "q_60%",
          "q_80%",
          "q_100%"
        ),
        share_at = results
      ),
      row.names = c(NA, -5L),
      class = c("data.table", "data.frame")
    )

  # Check computations
  res_list$dist_stats$welfare_share_at |>
    expect_equal(res_bm_list)

  res_atomic$dist_stats$welfare_share_at |>
    expect_equal(res_bm_atomic)

  res_dt$dist_stats$welfare_share_at |>
    expect_equal(res_bm_dt)

  # Check output class
  class(res_list$dist_stats$welfare_share_at) |>
    expect_equal("list")

  class(res_atomic$dist_stats$welfare_share_at) |>
    expect_equal("numeric")

  #class(res_dt) |>
  #  expect_equal("data.frame")

  # Check names in output
  names(res_list$dist_stats$welfare_share_at) |>
    expect_equal(c("20%", "40%", "60%", "80%", "100%"))

  length(res_list$dist_stats$welfare_share_at) |>
    expect_equal(n)

  names(res_dt$dist_stats$welfare_share_at) |>
    expect_equal(c("quantiles", "share_at"))

  nrow(res_dt$dist_stats$welfare_share_at) |>
    expect_equal(length(res_list$dist_stats$welfare_share_at))

  names(res_atomic$dist_stats$welfare_share_at) |>
    expect_equal(names(res_list$dist_stats$welfare_share_at))

  length(res_atomic$dist_stats$welfare_share_at) |>
    expect_equal(length(res_list$dist_stats$welfare_share_at))

  # check "complete" arg
  res_list |>
    expect_equal(res_list_com$results)
  res_dt |>
    expect_equal(res_dt_com$results)
  res_atomic |>
    expect_equal(res_atomic_com$results)

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

   pipmd_quantile_welfare_share(welfare = welfare) |>
     expect_no_error()

  pipmd_quantile_welfare_share(
    welfare  = welfare,
    weight   = weight,
    n        = NULL
  ) |>
    expect_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_quantile_welfare_share -outputs", {

  n = 5
  results <- wbpip::md_quantile_welfare_share(
    welfare    = welfare,
    weight     = weight,
    n          = n,
    format     = "atomic"
  )
  results <- as.numeric(results)


  res_list <-
    pipmd_quantile_welfare_share(
      welfare = welfare,
      weight  = weight,
      n       = n,
      format  = "list"
    )
  res_atomic <-
    pipmd_quantile_welfare_share(
      welfare = welfare,
      weight  = weight,
      n       = n,
      format  = "atomic"
    )
  res_dt <-
    pipmd_quantile_welfare_share(
      welfare = welfare,
      weight  = weight,
      n       = n,
      format  = "dt"
    )

  # Complete objects
  res_list_com <- pipmd_quantile_welfare_share(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "list",
    complete = TRUE
  )
  res_dt_com <- pipmd_quantile_welfare_share(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "dt",
    complete = TRUE
  )
  res_atomic_com <- pipmd_quantile_welfare_share(
    welfare  = welfare,
    weight   = weight,
    n        = n,
    format   = "atomic",
    complete = TRUE
  )


  res_bm_list <-
    list(
      "20%"  = results[1],
      "40%"  = results[2],
      "60%"  = results[3],
      "80%"  = results[4],
      "100%" = results[5]
    )
  res_bm_atomic <-
    c(
      "20%"  = results[1],
      "40%"  = results[2],
      "60%"  = results[3],
      "80%"  = results[4],
      "100%" = results[5]
    )
  res_bm_dt <-
    structure(
      list(
        quantiles = c(
          "q_20%",
          "q_40%",
          "q_60%",
          "q_80%",
          "q_100%"
        ),
        share_at = results
      ),
      row.names = c(NA, -5L),
      class = c("data.table", "data.frame")
    )
  res_bm_dt <-
    structure(
      list(
        quantiles = c(
          "q_20%",
          "q_40%",
          "q_60%",
          "q_80%",
          "q_100%"
        ),
        share_at = c(
          results[1],
          results[2],
          results[3],
          results[4],
          results[5]
        )
      ),
      row.names = c(NA, -5L),
      class = c("data.table", "data.frame")
    )

  # Check computations
  res_list$dist_stats$quantile_welfare_share |>
    expect_equal(res_bm_list)

  res_atomic$dist_stats$quantile_welfare_share |>
    expect_equal(res_bm_atomic)

  res_dt$dist_stats$quantile_welfare_share |>
    expect_equal(res_bm_dt)

  # Check output class
  class(res_list$dist_stats$quantile_welfare_share) |>
    expect_equal("list")

  class(res_atomic$dist_stats$quantile_welfare_share) |>
    expect_equal("numeric")

  class(res_dt$dist_stats$quantile_welfare_share) |>
    expect_equal(c("data.table", "data.frame"))

  # Check names in output
  length(res_list$dist_stats$quantile_welfare_share) |>
    expect_equal(n)

  names(res_dt$dist_stats$quantile_welfare_share) |>
    expect_equal(c("quantiles", "share_at"))

  nrow(res_dt$dist_stats$quantile_welfare_share) |>
    expect_equal(length(res_list$dist_stats$quantile_welfare_share))

  names(res_atomic$dist_stats$quantile_welfare_share) |>
    expect_equal(names(res_list$dist_stats$quantile_welfare_share))

  length(res_atomic) |>
    expect_equal(length(res_list))

  # check "complete" arg
  res_list |>
    expect_equal(res_list_com$results)
  res_dt |>
    expect_equal(res_dt_com$results)
  res_atomic |>
    expect_equal(res_atomic_com$results)


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

  res_atomic <-
    pipmd_gini(welfare = welfare,
               weight = weight)
  res_atomic_com <-
    pipmd_gini(welfare = welfare,
               weight = weight,
               complete = TRUE)

  res_bm_atomic <- c(0.41905333648877)

  res_atomic$dist_stats$gini |>
    expect_equal(res_bm_atomic)


  # Check output class
  class(res_atomic) |>
    expect_equal("list")
  class(res_atomic$dist_stats$gini) |>
    expect_equal("numeric")

  # check "complete" arg
  expect_false(identical(res_atomic_com,
                         res_bm_atomic))

  expect_equal(res_atomic_com$results,
               res_atomic)
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

  pipmd_polarization(
    welfare = welfare,
    weight = weight,
    mean = NULL,
    median = NULL
  ) |>
    expect_no_error()

  pipmd_polarization(welfare = welfare,
                     weight = weight,
                     gini = NULL) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_polarization -outputs", {
  median <- fmedian(x     = welfare,
                    w     = weight)

  mean <- fmean(x = welfare,
                w = weight)

  gini <- pipmd_gini(welfare = welfare,
                     weight  = weight)

  res_atomic <-
    pipmd_polarization(welfare = welfare,
                       weight = weight)
  res_atomic_com <-
    pipmd_polarization(welfare = welfare,
                       weight = weight,
                       complete = TRUE)

  res_bm_atomic <- c(0.430115535839879)

  # Check computations
  res_atomic$dist_stats$polarization |>
    expect_equal(res_bm_atomic)
  res_atomic_com$results$dist_stats$polarization |>
    expect_equal(res_bm_atomic)
  # Check output class
  class(res_atomic$dist_stats$polarization) |>
    expect_equal("numeric")


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

   pipmd_mld(welfare = welfare) |>
     expect_no_error()

  pipmd_mld(welfare = welfare,
            weight  = weight,
            mean    = NULL) |>
    expect_no_error()

})

# Outputs ---------------------------------------------------------------
test_that("pipmd_mld -outputs", {
  mean <- collapse::fmean(x = welfare,
                          w = weight)

  res_atomic <-
    pipmd_mld(welfare = welfare,
              weight = weight)
  res_atomic_com <-
    pipmd_mld(welfare = welfare,
              weight = weight,
              complete = TRUE)

  res_bm_atomic <- c(0.301620140444736)

  # Check computations
  res_atomic$dist_stats$mld |>
    expect_equal(res_bm_atomic)

  class(res_atomic) |>
    expect_equal("list")

  # Check names in output
  names(res_atomic$dist_stats) |>
    expect_equal(c("mld"))

  expect_false(identical(res_atomic,
                         res_atomic_com))
  expect_true(identical(res_atomic_com$results$dist_stats$mld,
                        res_atomic$dist_stats$mld))


})

