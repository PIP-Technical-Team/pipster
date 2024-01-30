# Test functions related to distributional measures for group data ####

# Test pipgd_welfare_share_at function ####

welfare <- pip_gd$L
weight <- pip_gd$P
params <- pipgd_select_lorenz(welfare = welfare, weight = weight, complete = TRUE)

# Inputs ------------------------------------------------------------------------------

test_that("pipgd_welfare_share_at inputs work as expected", {
    res1 <- pipgd_welfare_share_at(welfare = welfare, weight = weight)
    res2 <- pipgd_welfare_share_at(params = params)

    expect_equal(res1, res2)

    # Welfare and weight arguments
    pipgd_welfare_share_at(params = NULL, welfare = NULL, weight = weight) |>
        expect_error()
    pipgd_welfare_share_at(params = NULL, welfare = welfare, weight = NULL) |>
        expect_error()
    # invalid n argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, n = "invalid n") |>
        expect_error()
    # lorenz argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, lorenz = "Neither NULL, lq or lb") |>
        expect_error()
    # valid n argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, n=4) |>
        expect_no_error()
    # valid popshare argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, popshare = c(0.3, 0.4)) |>
        expect_no_error()
    # invalid complete argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, complete = "neither TRUE or FALSE") |>
        expect_no_error()
    # complete argument
    pipgd_welfare_share_at(welfare = welfare, weight = weight, complete = TRUE) |>
        expect_no_error()
})

# Outputs ------------------------------------------------------------------------------
test_that("pipgd_welfare_share_at outputs work as expected", {
    res1 <- pipgd_welfare_share_at(welfare = welfare, weight = weight)
    res2 <- pipgd_welfare_share_at(params = params)
    res_complete <- pipgd_welfare_share_at(welfare = welfare, weight = weight, complete = TRUE)

    # Class of outputs
    class(res1) |>
        expect_equal("list")

    # Length of output
    n = 5
    res_n5 <- pipgd_welfare_share_at(welfare = welfare, weight = weight, n = n)

    length(res_n5$dist_stats$popshare) |>
        expect_equal(n)

    length(res_n5$dist_stats$welfare_share_at) |>
        expect_equal(n)

    # Value of output
    res_n5$dist_stats$popshare[n] |>
        expect_equal(1)

    res_n5$dist_stats$popshare[n] |>
        expect_equal(res_n5$dist_stats$welfare_share_at[n])

    # Names in output list
    names(res1) |>
        expect_equal(names(res2))
    names(res1) |>
        expect_equal("dist_stats")
    names(res1$dist_stats) |>
        expect_equal(c("popshare", "welfare_share_at"))

    # Names in output list when COMPLETE is TRUE
    names(res_complete) |>
        expect_equal(c("gd_params", "data", "selected_lorenz", "dist_stats"))

    names(res_complete$gd_params) |>
        expect_equal(c("lq", "lb"))

    names(res_complete$gd_params$lq) |>
        expect_equal(names(res_complete$gd_params$lb))

    names(res_complete$gd_params$lq) |>
        expect_equal(c("reg_results", "key_values", "validity"))

    names(res_complete$gd_params$lq$reg_results) |>
        expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

    names(res_complete$gd_params$lq$key_values) |>
        expect_equal(c("e", "m", "n", "r", "s1", "s2"))

    names(res_complete$gd_params$lq$validity) |>
        expect_equal(c("is_normal", "is_valid", "headcount"))

    names(res_complete$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$data) |>
        expect_equal(c("welfare", "weight"))

    names(res_complete$selected_lorenz) |>
        expect_equal(c( "for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov" ))

    names(res_complete$dist_stats) |>
        expect_equal(c("popshare", "welfare_share_at"))

})

# Test pipgd_quantile_welfare_share function ####
# Inputs -------------------------------------------------------------------------
test_that("pipgd_quantile_welfare_share inputs works as expected", {
    res1 <- pipgd_quantile_welfare_share(welfare = welfare, weight = weight)
    res2 <- pipgd_quantile_welfare_share(params = params)

    # Welfare and weight arguments
    pipgd_quantile_welfare_share(welfare = welfare, weight = NULL) |>
        expect_error()

    pipgd_quantile_welfare_share(welfare = NULL, weight = weight) |>
        expect_error()

    # Lorenz argument
    pipgd_quantile_welfare_share(welfare = welfare, weight = weight, lorenz = "Neither NULL, lq or lb") |>
        expect_error()

    # n argument
    pipgd_quantile_welfare_share(welfare = welfare, weight = weight, n = "Not a valid number") |>
        expect_error()
    pipgd_quantile_welfare_share(welfare = welfare, weight = weight, n = -2) |>
        expect_error()

    # popshare arguments
    pipgd_quantile_welfare_share(welfare = welfare, weight = weight, popshare = c(0.3, 0.5, 0.8)) |>
        expect_no_error()

})

# Outputs ----------------------------------------------------------------------------
test_that("pipgd_quantile_welfare_share outputs work as expected", {

    # Check same results when params or (welfare and weights) are provided
    res1 <- pipgd_quantile_welfare_share(welfare = welfare, weight = weight)
    res2 <- pipgd_quantile_welfare_share(params = params)
    res_complete <- pipgd_quantile_welfare_share(welfare = welfare, weight = weight, complete = TRUE)

    expect_equal(res1, res2)

    # Check welfare shares values in output list for an example value of n
    n = 3
    params_from_select_l <- pipgd_select_lorenz(welfare = welfare, weight = weight, complete = TRUE)
    res_n3 <- pipgd_quantile_welfare_share(welfare = welfare, weight = weight, n=3)
    res_welfare_share <- pipgd_welfare_share_at(params = params_from_select_l, complete = FALSE)

    length(res_n3$dist_stats$popshare) |>
        expect_equal(n)

    length(res_n3$dist_stats$quantile_welfare_share) |>
        expect_equal(n)

    res_n3$dist_stats$quantile_welfare_share[1] |>
        expect_equal(res_welfare_share$dist_stats$welfare_share_at[1])

    res_n3$dist_stats$quantile_welfare_share |>
        expect_equal(c(res_welfare_share$dist_stats$welfare_share_at[1],
             diff(res_welfare_share$dist_stats$welfare_share_at)))

    # Names in output list when complete is TRUE
    names(res_complete) |>
     expect_equal(c("gd_params", "data", "selected_lorenz", "dist_stats"))

    names(res_complete$gd_params) |>
        expect_equal(c("lq", "lb"))

    names(res_complete$gd_params$lq) |>
        expect_equal(names(res_complete$gd_params$lb))

    names(res_complete$gd_params$lq) |>
        expect_equal(c("reg_results", "key_values", "validity"))

    names(res_complete$gd_params$lq$reg_results) |>
        expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

    names(res_complete$gd_params$lq$key_values) |>
        expect_equal(c("e", "m", "n", "r", "s1", "s2"))

    names(res_complete$gd_params$lq$validity) |>
        expect_equal(c("is_normal", "is_valid", "headcount"))

    names(res_complete$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$data) |>
        expect_equal(c("welfare", "weight"))

    names(res_complete$selected_lorenz) |>
        expect_equal(c( "for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov" ))

    names(res_complete$dist_stats) |>
        expect_equal(c("popshare", "quantile_welfare_share"))

})


# Test pipgd_quantile function ####
# Inputs -------------------------------------------------------------------------
test_that("pipgd_quantile inputs works as expected", {
    res1 <- pipgd_quantile(welfare = welfare, weight = weight)
    res2 <- pipgd_quantile(params = params)

    # Welfare and weight arguments
    pipgd_quantile(welfare = welfare, weight = NULL) |>
        expect_error()

    pipgd_quantile(welfare = NULL, weight = weight) |>
        expect_error()

    # Lorenz argument
    pipgd_quantile(welfare = welfare, weight = weight, lorenz = "Neither NULL, lq or lb") |>
        expect_error()

    # n argument
    pipgd_quantile(welfare = welfare, weight = weight, n = "Not a valid number") |>
        expect_error()
    pipgd_quantile(welfare = welfare, weight = weight, n = -2) |>
        expect_error()

    # popshare arguments
    pipgd_quantile(welfare = welfare, weight = weight, popshare = c(0.3, 0.5, 0.8)) |>
        expect_no_error()
    pipgd_quantile(welfare = welfare, weight = weight, popshare = c(0.3, 0.5, -0.8)) |>
        expect_error()

    # complete argument
    pipgd_quantile(welfare = welfare, weight = weight, complete = "neither TRUE or FALSE") |>
        expect_error()

})

# Outputs ----------------------------------------------------------------------------
test_that("pipgd_quantile outputs work as expected", {

    # Check same results when params or (welfare and weights) are provided
    res1 <- pipgd_quantile(welfare = welfare, weight = weight)
    res2 <- pipgd_quantile(params = params)
    expect_equal(res1, res2)

    # Check same results when n or popshare are provided
    n = 5
    res_n5 <- pipgd_quantile(welfare = welfare, weight = weight, n = n)
    res_with_popshare <- pipgd_quantile(welfare = welfare, weight = weight, popshare = c(0.2, 0.4, 0.6, 0.8, 1.0))
    expect_equal(res_with_popshare, res_n5)

    res_complete <- pipgd_quantile(welfare = welfare, weight = weight, complete = TRUE)

    # Check output type
    class(res1) |>
        expect_equal(class(res2))

    class(res2) |>
        expect_equal("list")

    class(res_complete) |>
        expect_equal("pipgd_params")

    # Check output length
    length(res_n5$dist_stats$popshare) |>
        expect_equal(n)

    length(res_n5$dist_stats$quantile) |>
        expect_equal(n)

    # Check popshare output
    res_n5$dist_stats$popshare[2] |>
        expect_equal(0.4)

    # Check quantile output when lorenz = "lb"
    mean = 1
    popshare = c(0.2, 0.4, 0.6, 0.8, 1.0)
    lorenz = "lb"

    res <- pipgd_quantile(welfare = welfare, weight = weight,  popshare = popshare, lorenz = lorenz)
    qt <- mean*wbpip:::derive_lb(x = popshare,
                    params$gd_params[["lb"]]$reg_results$coef[["A"]],
                    params$gd_params[["lb"]]$reg_results$coef[["B"]],
                    params$gd_params[["lb"]]$reg_results$coef[["C"]])

    res$dist_stats$quantile |>
        expect_equal(qt)

    # Check quantile output when lorenz = "lq"
    lorenz = "lq"
    res <- pipgd_quantile(welfare = welfare, weight = weight,  popshare = popshare, lorenz = lorenz)
    qt <- mean*wbpip:::derive_lq(x = popshare,
                    params$gd_params[["lq"]]$reg_results$coef[["A"]],
                    params$gd_params[["lq"]]$reg_results$coef[["B"]],
                    params$gd_params[["lq"]]$reg_results$coef[["C"]])

    res$dist_stats$quantile |>
        expect_equal(qt)


    # Check names in output list when complete is TRUE
    names(res_complete) |>
     expect_equal(c("gd_params", "data", "selected_lorenz", "dist_stats"))

    names(res_complete$gd_params) |>
        expect_equal(c("lq", "lb"))

    names(res_complete$gd_params$lq) |>
        expect_equal(names(res_complete$gd_params$lb))

    names(res_complete$gd_params$lq) |>
        expect_equal(c("reg_results", "key_values", "validity"))

    names(res_complete$gd_params$lq$reg_results) |>
        expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

    names(res_complete$gd_params$lq$key_values) |>
        expect_equal(c("e", "m", "n", "r", "s1", "s2"))

    names(res_complete$gd_params$lq$validity) |>
        expect_equal(c("is_normal", "is_valid", "headcount"))

    names(res_complete$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$data) |>
        expect_equal(c("welfare", "weight"))

    names(res_complete$selected_lorenz) |>
        expect_equal(c( "for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov" ))

    names(res_complete$dist_stats) |>
        expect_equal(c("popshare", "quantile"))

})


# Test pipgd_gini function ####
test_that("pipgd_gini works as expected", {
    res1 <- pipgd_gini(welfare = welfare, weight = weight)
    res2 <- pipgd_gini(params = params)

    res1 |>
        expect_equal(res2)

    # Output type
    class(res2) |>
        expect_equal(class(res1))

    class(res1) |>
        expect_equal("list")

    # Names in output list
    names(res1) |>
        expect_equal("dist_stats")

    names(res1$dist_stats) |>
        expect_equal(c("gini", "lorenz"))

    # Check that invalid (valid) inputs (do not) raise erros
    pipgd_gini(welfare = welfare, weight = NULL) |>
        expect_error()

    pipgd_gini(welfare = NULL, weight = weight) |>
        expect_error()

    pipgd_gini(welfare = welfare, weight = weight, mean = "invalid mean") |>
        expect_error()

    pipgd_gini(welfare = welfare, weight = weight, popshare = 0.5) |>
        expect_no_error()

    pipgd_gini(welfare = welfare, weight = weight, popshare = 0.5, povline = 1) |>
        expect_error()

    pipgd_gini(welfare = welfare, weight = weight, times_mean = "invalid times_mean") |>
        expect_error()

    pipgd_gini(welfare = welfare, weight = weight, popshare = "invalid popshare") |>
        expect_error()

    pipgd_gini(welfare = welfare, weight = weight, lorenz = "invalid Lorenz") |>
        expect_error()

    # Check gini in output list
    # When lorenz = "lb"
    res_lb <- pipgd_gini(welfare = welfare, weight = weight, lorenz = 'lb')
    res_lq <- pipgd_gini(welfare = welfare, weight = weight, lorenz = 'lq')

    gini_wbpip_lb <- wbpip:::gd_compute_gini_lb(
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        nbins     = 499
      )

    res_lb$dist_stats$gini |>
        expect_equal(gini_wbpip_lb)

    gini_wbpip_lq <- wbpip:::gd_compute_gini_lq(
        A         = params$gd_params$lq$reg_results$coef[["A"]],
        B         = params$gd_params$lq$reg_results$coef[["B"]],
        C         = params$gd_params$lq$reg_results$coef[["C"]],
        e         = params$gd_params$lq$key_values$e,
        m         = params$gd_params$lq$key_values$m,
        n         = params$gd_params$lq$key_values$n,
        r         = params$gd_params$lq$key_values$r
      )

    res_lq$dist_stats$gini |>
        expect_equal(gini_wbpip_lq)

})

# Test pipgd_mld ####
# Inputs -----------------------------------------------------------------
test_that("pipgd_mld inputs works as expected", {

    pipgd_mld(welfare = welfare, weight = NULL) |>
        expect_error()

    pipgd_mld(welfare = NULL, weight = weight) |>
        expect_error()

    pipgd_mld(welfare = welfare, weight = weight, mean = "invalid mean") |>
        expect_error()

    pipgd_mld(welfare = welfare, weight = weight, popshare = 0.5) |>
        expect_no_error()

    pipgd_mld(welfare = welfare, weight = weight, popshare = 0.5, povline = 1) |>
        expect_error()

    pipgd_mld(welfare = welfare, weight = weight, times_mean = "invalid times_mean") |>
        expect_error()

    pipgd_params(welfare = welfare, weight = weight, popshare = "invalid popshare") |>
        expect_error()

    pipgd_mld(welfare = welfare, weight = weight, lorenz = "Neither NULL, lq or lb") |>
        expect_error()

    pipgd_mld(welfare = welfare, weight = weight, complete = "invalid complete") |>
        expect_error()

})

# Outputs -----------------------------------------------------------------
test_that("pipgd_mld outputs work as expected", {
    res <- pipgd_mld(welfare = welfare, weight = weight)
    res_complete <- pipgd_mld(welfare = welfare, weight = weight, complete = TRUE)

    class(res) |>
        expect_equal("list")

    names(res) |>
        expect_equal("dist_stats")

    names(res$dist_stats) |>
        expect_equal(c("mld","lorenz"))

    # Names in output list when complete = TRUE
    names(res_complete) |>
     expect_equal(c("gd_params", "data", "selected_lorenz", "dist_stats"))

    names(res_complete$gd_params) |>
        expect_equal(c("lq", "lb"))

    names(res_complete$gd_params$lq) |>
        expect_equal(names(res_complete$gd_params$lb))

    names(res_complete$gd_params$lq) |>
        expect_equal(c("reg_results", "key_values", "validity"))

    names(res_complete$gd_params$lq$reg_results) |>
        expect_equal(c("ymean", "sst", "coef", "sse", "r2", "mse", "se"))

    names(res_complete$gd_params$lq$key_values) |>
        expect_equal(c("e", "m", "n", "r", "s1", "s2"))

    names(res_complete$gd_params$lq$validity) |>
        expect_equal(c("is_normal", "is_valid", "headcount"))

    names(res_complete$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$data) |>
        expect_equal(c("welfare", "weight"))

    names(res_complete$selected_lorenz) |>
        expect_equal(c( "for_dist", "for_pov", "use_lq_for_dist", "use_lq_for_pov" ))

    names(res_complete$dist_stats) |>
        expect_equal(c("mld", "lorenz"))

})

test_that("pipgd_mld calculates mld as expected", {

    mld_wbpip_lb <- wbpip:::gd_compute_mld_lb(
        A         = params$gd_params$lb$reg_results$coef[["A"]],
        B         = params$gd_params$lb$reg_results$coef[["B"]],
        C         = params$gd_params$lb$reg_results$coef[["C"]],
        dd        = 0.01)

    mld_wbpip_lq <- wbpip:::gd_compute_mld_lq(
        A         = params$gd_params$lq$reg_results$coef[["A"]],
        B         = params$gd_params$lq$reg_results$coef[["B"]],
        C         = params$gd_params$lq$reg_results$coef[["C"]],
        dd        = 0.01)

    res_lq <- pipgd_mld(welfare = welfare, weight = weight, lorenz = "lq")
    res_lb <- pipgd_mld(welfare = welfare, weight = weight, lorenz = "lb")
    res_complete <- pipgd_mld(welfare = welfare, weight = weight, complete = FALSE)

    res_lq$dist_stats$mld |>
        expect_equal(mld_wbpip_lq)

    res_lb$dist_stats$mld |>
        expect_equal(mld_wbpip_lb)

})


