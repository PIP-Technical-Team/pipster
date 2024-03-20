# Test functions related to distributional measures for group data ####

welfare <- pip_gd$L# |> as.numeric()
weight  <- pip_gd$P# |> as.numeric()
gd1 <- create_pipster_object(welfare = pip_gd$L, # is cumulative
                             weight  = pip_gd$P) # is cumulative
# gd2 <- create_pipster_object(welfare = pip_gd$R, # to one
#                              weight = pip_gd$W)  # to one
gd5 <- create_pipster_object(welfare = pip_gd$X, # neither
                             weight  = pip_gd$W) # to one

# pip_gd$R |> fsum()
# identify_pip_type(welfare = pip_gd$R, weight = pip_gd$W)

#_______________________________________________________________________________
# Tests
#_______________________________________________________________________________


# Inputs ------------------------------------------------------------------------------

test_that("pipgd_welfare_share_at inputs work as expected", {

    res1      <- pipgd_welfare_share_at(welfare  = welfare,
                                        weight   = weight)
    res1_full <- pipgd_welfare_share_at(welfare  = welfare,
                                        weight   = weight,
                                        complete = TRUE)
    res2      <- pipgd_welfare_share_at(pipster_object = gd1)
    res1_lq   <- pipgd_welfare_share_at(welfare  = welfare,
                                        weight   = weight,
                                        complete = TRUE,
                                        lorenz   = "lq")
    res3      <- pipgd_welfare_share_at(pipster_object = gd1)

    expect_equal(res1, res2)
    expect_equal(res1_full$dist_stats,
                 res1_lq$dist_stats)

    # Welfare and weight arguments
    pipgd_welfare_share_at(pipster_object  = NULL,
                           welfare         = NULL,
                           weight          = weight) |>
        expect_error()
    pipgd_welfare_share_at(pipster_object  = NULL,
                           welfare         = welfare,
                           weight          = NULL) |>
        expect_error()
    # invalid n argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           n               = "invalid n") |>
        expect_error()
    # lorenz argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           lorenz          = "Neither NULL, lq or lb") |>
        expect_error()
    # valid n argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           n               = 4) |>
        expect_no_error()
    # valid popshare argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           popshare        = c(0.3, 0.4)) |>
        expect_no_error()
    # invalid complete argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           complete        = "neither TRUE or FALSE") |>
        expect_no_error()
    # complete argument
    pipgd_welfare_share_at(welfare         = welfare,
                           weight          = weight,
                           complete        = TRUE) |>
        expect_no_error()
})

# Outputs ------------------------------------------------------------------------------
test_that("pipgd_welfare_share_at outputs work as expected", {
    popshare = c(0.4, 0.7, 1.0)

    res1         <- pipgd_welfare_share_at(welfare  = welfare,
                                           weight   = weight)
    res2         <- pipgd_welfare_share_at(pipster_object = gd1)
    res_complete <- pipgd_welfare_share_at(welfare  = welfare,
                                           weight   = weight,
                                           complete = TRUE)

    res_complete_params <- pipgd_welfare_share_at(pipster_object = gd1,
                                                  complete = TRUE)
    res1 |>
      expect_equal(res2)

    res_complete_params |>
      expect_equal(res_complete)

    # Class of outputs
    class(res1) |>
        expect_equal("list")

    class(res_complete$params) |>
      expect_equal("pipgd_params")

    # Length of output
    n = 5
    res_n5 <- pipgd_welfare_share_at(welfare = welfare,
                                     weight  = weight,
                                     n       = n)

    length(res_n5$dist_stats$popshare) |>
        expect_equal(n)

    length(res_n5$dist_stats$welfare_share_at) |>
        expect_equal(n)

    length(res1) |>
        expect_equal(1)
    length(res1) |>
        expect_equal(length(res2))

    # Value of output
    res_n5$dist_stats$popshare[n] |>
        expect_equal(1)

    res_n5$dist_stats$popshare[n] |>
        expect_equal(res_n5$dist_stats$welfare_share_at[n])

    # Computations
    # When lorenz = "lb" and popshare is supplied
    out <- pipgd_welfare_share_at(welfare  = welfare,
                                  weight   = weight,
                                  lorenz   = "lb",
                                  popshare = popshare)

    out$dist_stats$popshare |>
        expect_equal(popshare)

    welfare_share_at_benchmark = c(0.222132795469908, 0.499669550515477, 1)

    out$dist_stats$welfare_share_at |>
        expect_equal(welfare_share_at_benchmark)

    # When lorenz = "lq" and popshare is supplied
    out <- pipgd_welfare_share_at(welfare  = welfare,
                                  weight   = weight,
                                  lorenz   = "lq",
                                  popshare = popshare)

    welfare_share_at_benchmark = c(0.224128492162046, 0.499035579672699, 1)

    out$dist_stats$welfare_share_at |>
      expect_equal(welfare_share_at_benchmark)

    # When n is supplied and lorenz is "lb"
    out <- pipgd_welfare_share_at(welfare  = welfare,
                                  weight   = weight,
                                  lorenz   = "lb",
                                  n = 8)

    welfare_share_at_benchmark = c(0.0500334254250001, 0.119298465024495,
                                   0.203491486891788, 0.303035894959234,
                                   0.41981870625436, 0.557817404318982,
                                   0.726600317579613, 1)

    out$dist_stats$welfare_share_at |>
      expect_equal(welfare_share_at_benchmark)

    # When n is supplied and lorenz is "lq"
    out <- pipgd_welfare_share_at(welfare  = welfare,
                                  weight   = weight,
                                  lorenz   = "lq",
                                  n = 8)

    welfare_share_at_benchmark = c(0.0509485786496592, 0.120430587750321,
                                   0.205377235320994, 0.305078427620641,
                                   0.420661080404237, 0.555950473802554,
                                   0.722019187706309, 1)

    out$dist_stats$welfare_share_at |>
      expect_equal(welfare_share_at_benchmark)


    # Names in output list
    names(res1) |>
        expect_equal(names(res2))
    names(res1) |>
        expect_equal("dist_stats")
    names(res1$dist_stats) |>
        expect_equal(c("popshare",
                       "welfare_share_at",
                       "lorenz"))

    # Names in output list when COMPLETE is TRUE
    names(res_complete$params) |>
        expect_equal(c("gd_params",
                       "data",
                       "selected_lorenz"))

    names(res_complete$params$gd_params) |>
        expect_equal(c("lq", "lb"))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(names(res_complete$params$gd_params$lb))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(c("reg_results",
                       "key_values",
                       "validity"))

    names(res_complete$params$gd_params$lq$reg_results) |>
        expect_equal(c("ymean",
                       "sst",
                       "coef",
                       "sse",
                       "r2",
                       "mse",
                       "se"))

    names(res_complete$params$gd_params$lq$key_values) |>
        expect_equal(c("e",
                       "m",
                       "n",
                       "r",
                       "s1",
                       "s2"))

    names(res_complete$params$gd_params$lq$validity) |>
        expect_equal(c("is_normal",
                       "is_valid",
                       "headcount"))

    names(res_complete$params$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$params$data) |>
        expect_equal(c("welfare",
                       "weight",
                       "mean"))

    names(res_complete$params$selected_lorenz) |>
        expect_equal(c( "for_dist",
                        "for_pov",
                        "use_lq_for_dist",
                        "use_lq_for_pov" ))

    names(res_complete$results$dist_stats) |>
        expect_equal(c("popshare",
                       "welfare_share_at",
                       "lorenz"))

})

# Test pipgd_quantile_welfare_share function ####
# Inputs -------------------------------------------------------------------------
test_that("pipgd_quantile_welfare_share inputs works as expected", {

    res1      <- pipgd_quantile_welfare_share(welfare  = welfare,
                                              weight   = weight)
    res2      <- pipgd_quantile_welfare_share(pipster_object = gd1)

    # Welfare and weight arguments
    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = NULL) |>
        expect_error()

    pipgd_quantile_welfare_share(welfare         = NULL,
                                 weight          = weight) |>
        expect_error()

    # Lorenz argument
    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 lorenz          = "Neither NULL, lq or lb") |>
        expect_error()

    # n argument
    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 n               = "Not a valid number") |>
        expect_error()
    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 n               = -2) |>
        expect_error()

    # popshare arguments
    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 popshare        = c(0.3, 0.5, 0.8)) |>
        expect_no_error()

    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 popshare        = c(0.3, 0.5, -0.8)) |>
        expect_error()

    pipgd_quantile_welfare_share(welfare         = welfare,
                                 weight          = weight,
                                 popshare        = c(0.3, 0.5, 2)) |>
        expect_error()

})

# Outputs ----------------------------------------------------------------------------
test_that("pipgd_quantile_welfare_share outputs work as expected", {

    # Check same results when params or (welfare and weights) are provided
    res1         <- pipgd_quantile_welfare_share(welfare  = welfare,
                                                 weight   = weight)
    res2         <- pipgd_quantile_welfare_share(pipster_object   = gd1)
    res_complete <- pipgd_quantile_welfare_share(welfare  = welfare,
                                                 weight   = weight,
                                                 complete = TRUE)
    res_com_lq   <- pipgd_quantile_welfare_share(welfare  = welfare,
                                                 weight   = weight,
                                                 complete = TRUE,
                                                 lorenz   = "lq")
    res_complete_params <- pipgd_quantile_welfare_share(pipster_object   = gd1,
                                                        complete = TRUE)

   res1 |>
      expect_equal(res2)

   expect_equal(res_com_lq$dist_stats,
                res_complete$dist_stats)

   class(res2) |>
      expect_equal("list")

   class(res_complete$params) |>
      expect_equal("pipgd_params")

   res_complete |>
      expect_equal(res_complete_params)

    # Check welfare shares values in output list for an example value of n
    n                                                     = 3
    params_from_select_l <- pipgd_select_lorenz(welfare   = welfare,
                                                weight    = weight,
                                                complete  = TRUE)
    res_n3 <- pipgd_quantile_welfare_share(welfare        = welfare,
                                           weight         = weight,
                                           n              = n)
    res_welfare_share <- pipgd_welfare_share_at(pipster_object    = gd1,
                                                complete  = FALSE,
                                                n = n)

    length(res_n3$dist_stats$popshare) |>
        expect_equal(n)

    length(res_n3$dist_stats$welfare_share) |>
        expect_equal(n)

    res_n3$dist_stats$welfare_share[1] |>
        expect_equal(res_welfare_share$dist_stats$welfare_share_at[1])

    res_n3$dist_stats$welfare_share |>
        expect_equal(c(res_welfare_share$dist_stats$welfare_share_at[1],
             diff(res_welfare_share$dist_stats$welfare_share_at)))

    res_n3$dist_stats$popshare |>
      expect_equal(c(0.333333333333333, 0.666666666666667, 1))

    # Names in output list when complete is TRUE
    names(res_complete$params) |>
     expect_equal(c("gd_params",
                    "data",
                    "selected_lorenz"))

    names(res_complete$params$gd_params) |>
        expect_equal(c("lq",
                       "lb"))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(names(res_complete$params$gd_params$lb))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(c("reg_results",
                       "key_values",
                       "validity"))

    names(res_complete$params$gd_params$lq$reg_results) |>
        expect_equal(c("ymean",
                       "sst",
                       "coef",
                       "sse",
                       "r2",
                       "mse",
                       "se"))

    names(res_complete$params$gd_params$lq$key_values) |>
        expect_equal(c("e",
                       "m",
                       "n",
                       "r",
                       "s1",
                       "s2"))

    names(res_complete$params$gd_params$lq$validity) |>
        expect_equal(c("is_normal",
                       "is_valid",
                       "headcount"))

    names(res_complete$params$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$params$data) |>
        expect_equal(c("welfare",
                       "weight",
                       "mean"))

    names(res_complete$params$selected_lorenz) |>
        expect_equal(c( "for_dist",
                        "for_pov",
                        "use_lq_for_dist",
                        "use_lq_for_pov" ))

    # failing: will be fixed when lorenz task works (ZP: 20 March 2024)
    names(res_complete$results$dist_stats) |>
        expect_equal(c("popshare",
                       "welfare_share_at",
                       "lorenz",
                       "welfare_share"))

})


# Test pipgd_quantile function ####
# Inputs -------------------------------------------------------------------------
test_that("pipgd_quantile inputs works as expected", {

    res1 <- pipgd_quantile(welfare = welfare,
                           weight  = weight)
    res2 <- pipgd_quantile(pipster_object = gd1)

    # Welfare and weight arguments
    pipgd_quantile(welfare         = welfare,
                   weight          = NULL) |>
        expect_error()

    pipgd_quantile(welfare         = NULL,
                   weight          = weight) |>
        expect_error()

    # Lorenz argument
    pipgd_quantile(welfare         = welfare,
                   weight          = weight,
                   lorenz          = "Neither NULL, lq or lb") |>
        expect_error()

    # n argument
    pipgd_quantile(welfare         = welfare,
                   weight          = weight,
                   n               = "Not a valid number") |>
        expect_error()
    pipgd_quantile(welfare         = welfare,
                   weight          = weight,
                   n               = -2) |>
        expect_error()

    # popshare arguments
    pipgd_quantile(welfare         = welfare,
                   weight          = weight,
                   popshare        = c(0.3, 0.5, 0.8)) |>
        expect_no_error()
    pipgd_quantile(welfare         = welfare,
                   weight          = weight,
                   popshare        = c(0.3, 0.5, -0.8)) |>
        expect_error()

})

# Outputs ----------------------------------------------------------------------------
test_that("pipgd_quantile outputs work as expected", {

    # Check same results when params or (welfare and weights) are provided
    res1 <- pipgd_quantile(welfare = welfare,
                           weight = weight)
    res2 <- pipgd_quantile(pipster_object = gd1)


    res1 |>
        expect_equal(res2)

    res_complete       <- pipgd_quantile(welfare  = welfare,
                                         weight   = weight,
                                         complete = TRUE)
    res_comlete_params <- pipgd_quantile(pipster_object = gd1,
                                         complete = TRUE)

    # Check same results when n or popshare are provided
    n                                            = 5
    res_n5 <- pipgd_quantile(welfare             = welfare,
                             weight              = weight,
                             n                   = n)
    res_with_popshare <- pipgd_quantile(welfare  = welfare,
                                        weight   = weight,
                                        popshare = c(0.2, 0.4, 0.6, 0.8, 1.0))
    res_with_popshare |>
      expect_equal(res_n5)


    # Check output type
    class(res1) |>
        expect_equal(class(res2))

    class(res2) |>
        expect_equal("list")

    class(res_complete$params) |>
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
    mean     = 1
    popshare = c(0.2, 0.4, 0.6, 0.8, 1.0)
    lorenz   = "lb"

    res <- pipgd_quantile(welfare  = welfare,
                          weight   = weight,
                          popshare = popshare,
                          lorenz   = lorenz)
    qt  <- mean*c(0.566807360141038,
                 0.757982268357493,
                 0.979182129032173,
                 1.31135609274135,
                 Inf)

    res$dist_stats$quantile |>
        expect_equal(qt)

    res$dist_stats$popshare |>
        expect_equal( c(0.2, 0.4, 0.6, 0.8, 1.0))

    # Check quantile output when lorenz = "lq"
    lorenz = "lq"
    res <- pipgd_quantile(welfare  = welfare,
                          weight   = weight,
                          popshare = popshare,
                          lorenz   = lorenz)
    qt <- mean*c(0.569781112621993,
                 0.761773436866965,
                 0.965764425951865,
                 1.28614768170655,
                 5.82699528574873)

    res$dist_stats$quantile |>
        expect_equal(qt)


    # Check names in output list when complete is TRUE
    names(res_complete) |>
      expect_equal(c("welfare",
                     "weight",
                     "args",
                     "params",
                     "results"))

    names(res_complete$args) |>
      expect_equal(c("mean",
                     "times_mean",
                     "povshare",
                     "n",
                     "popshare",
                     "povline",
                     "lorenz"))
    names(res_complete$params) |>
      expect_equal(c("gd_params",
                     "data",
                     "selected_lorenz"))
    names(res_complete$results) |>
     expect_equal(c("dist_stats"))

    names(res_complete$params$gd_params) |>
        expect_equal(c("lq",
                       "lb"))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(names(res_complete$params$gd_params$lb))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(c("reg_results",
                       "key_values",
                       "validity"))

    names(res_complete$params$gd_params$lq$reg_results) |>
        expect_equal(c("ymean",
                       "sst",
                       "coef",
                       "sse",
                       "r2",
                       "mse",
                       "se"))

    names(res_complete$params$gd_params$lq$key_values) |>
        expect_equal(c("e",
                       "m",
                       "n",
                       "r",
                       "s1",
                       "s2"))

    names(res_complete$params$gd_params$lq$validity) |>
        expect_equal(c("is_normal",
                       "is_valid",
                       "headcount"))

    names(res_complete$params$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$params$data) |>
        expect_equal(c("welfare",
                       "weight",
                       "mean"))

    names(res_complete$params$selected_lorenz) |>
        expect_equal(c( "for_dist",
                        "for_pov",
                        "use_lq_for_dist",
                        "use_lq_for_pov" ))

    names(res_complete$results$dist_stats) |>
        expect_equal(c("popshare",
                       "quantile",
                       "lorenz"))

})


# Test pipgd_gini function ####
test_that("pipgd_gini works as expected", {

    res1      <- pipgd_gini(welfare = welfare,
                            weight  = weight)
    res1_com  <- pipgd_gini(welfare = welfare,
                            weight  = weight,
                            complete = TRUE)
    res1_lq   <- pipgd_gini(welfare = welfare,
                            weight  = weight,
                            complete = TRUE,
                            lorenz   = "lq")
    res2      <- pipgd_gini(pipster_object = gd1)
    res2_com  <- pipgd_gini(pipster_object = gd1,
                            complete = TRUE)
    res2_lq   <- pipgd_gini(pipster_object = gd1,
                            complete = TRUE,
                            lorenz   = "lq")

    res1 |>
        expect_equal(res2)
    res1_com |>
      expect_equal(res2_com)
    res1_lq |>
      expect_equal(res2_lq)

    expect_equal(res2_com$dist_stats$gini,
                 res2_lq$dist_stats$gini)
    expect_equal(res1_com$dist_stats$gini,
                 res1_lq$dist_stats$gini)

    # Output type
    class(res2) |>
        expect_equal(class(res1))

    class(res1) |>
        expect_equal("list")

    # Names in output list
    names(res1) |>
        expect_equal("dist_stats")

    names(res1$dist_stats) |>
        expect_equal(c("gini",
                       "lorenz"))

    # Check that invalid (valid) inputs (do not) raise erros
    pipgd_gini(welfare           = welfare,
               weight            = NULL) |>
        expect_error()

    pipgd_gini(welfare           = NULL,
               weight            = weight) |>
        expect_error()

    pipgd_gini(welfare           = welfare,
               weight            = weight,
               mean              = "invalid mean") |>
        expect_error()

    #pipgd_gini(welfare           = welfare,
    #           weight            = weight,
    #           popshare          = 0.5) |>
    #    expect_no_error()

    pipgd_gini(welfare           = welfare,
               weight            = weight,
               popshare          = 0.5,
               povline           = 1) |>
        expect_error()

    pipgd_gini(welfare           = welfare,
               weight            = weight,
               times_mean        = "invalid times_mean") |>
        expect_error()

    pipgd_gini(welfare           = welfare,
               weight            = weight,
               popshare          = "invalid popshare") |>
        expect_error()

    pipgd_gini(welfare           = welfare,
               weight            = weight,
               lorenz            = "invalid Lorenz") |>
        expect_error()

    # Check gini in output list
    # When lorenz                = "lb"
    res_lb <- pipgd_gini(welfare = welfare,
                         weight  = weight,
                         lorenz  = 'lb')
    res_lq <- pipgd_gini(welfare = welfare,
                         weight  = weight,
                         lorenz  = 'lq')

    gini_wbpip_lb <- 0.289403872256448

    res_lb$dist_stats$gini |>
        expect_equal(gini_wbpip_lb)

    res_lb$dist_stats$lorenz |>
      expect_equal("lb")

    attributes(res_lb$dist_stats$gini) |>
        expect_equal(NULL)

    gini_wbpip_lq <- 0.289017119340459

    res_lq$dist_stats$gini |>
        expect_equal(gini_wbpip_lq)

    res_lq$dist_stats$lorenz |>
        expect_equal("lq")

    attributes(res_lb$dist_stats$gini) |>
        expect_equal(NULL)

})

# Test pipgd_mld ####
# Inputs -----------------------------------------------------------------
test_that("pipgd_mld inputs works as expected", {

    pipgd_mld(welfare     = welfare,
              weight      = NULL) |>
        expect_error()

    pipgd_mld(welfare     = NULL,
              weight      = weight) |>
        expect_error()

    pipgd_mld(welfare     = welfare,
              weight      = weight,
              mean        = "invalid mean") |>
        expect_error()

    pipgd_mld(welfare     = welfare,
              weight      = weight,
              popshare    = 0.5,
              povline     = 1) |>
        expect_error()

    pipgd_mld(welfare     = welfare,
              weight      = weight,
              times_mean  = "invalid times_mean") |>
        expect_error()

    pipgd_mld(welfare     = welfare,
              weight      = weight,
              lorenz      = "Neither NULL, lq or lb") |>
        expect_error()

    pipgd_mld(welfare     = welfare,
              weight      = weight,
              lorenz      = NULL) |>
      expect_no_error()

})

# Outputs -----------------------------------------------------------------
test_that("pipgd_mld outputs work as expected", {

    res          <- pipgd_mld(welfare = welfare,
                              weight  = weight)
    res_params   <- pipgd_mld(pipster_object = gd1)
    res_complete <- pipgd_mld(welfare  = welfare,
                              weight   = weight,
                              complete = TRUE)
    res |>
      expect_equal(res_params)

    class(res) |>
        expect_equal("list")

    class(res_complete$params) |>
        expect_equal("pipgd_params")

    names(res) |>
        expect_equal("dist_stats")

    names(res$dist_stats) |>
        expect_equal(c("mld",
                       "lorenz"))

    # Names in output list when complete = TRUE
    names(res_complete$params) |>
     expect_equal(c("gd_params",
                    "data",
                    "selected_lorenz"))

    names(res_complete$params$gd_params) |>
        expect_equal(c("lq",
                       "lb"))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(names(res_complete$params$gd_params$lb))

    names(res_complete$params$gd_params$lq) |>
        expect_equal(c("reg_results",
                       "key_values",
                       "validity"))

    names(res_complete$params$gd_params$lq$reg_results) |>
        expect_equal(c("ymean",
                       "sst",
                       "coef",
                       "sse",
                       "r2",
                       "mse",
                       "se"))

    names(res_complete$params$gd_params$lq$key_values) |>
        expect_equal(c("e",
                       "m",
                       "n",
                       "r",
                       "s1",
                       "s2"))

    names(res_complete$params$gd_params$lq$validity) |>
        expect_equal(c("is_normal",
                       "is_valid",
                       "headcount"))

    names(res_complete$params$gd_params$lb$key_values) |>
        expect_equal(NULL)

    names(res_complete$params$data) |>
        expect_equal(c("welfare",
                       "weight",
                       "mean"))

    names(res_complete$params$selected_lorenz) |>
        expect_equal(c( "for_dist",
                        "for_pov",
                        "use_lq_for_dist",
                        "use_lq_for_pov" ))

    names(res_complete$results$dist_stats) |>
        expect_equal(c("mld",
                       "lorenz"))

})

test_that("pipgd_mld calculates mld as expected", {

    mld_wbpip_lb <- 0.14055954382846

    mld_wbpip_lq <- 0.137680871901806

    res_lq       <- pipgd_mld(welfare  = welfare,
                              weight   = weight,
                              lorenz   = "lq")
    res_lb       <- pipgd_mld(welfare  = welfare,
                               weight  = weight,
                                lorenz = "lb")
    res_complete <- pipgd_mld(welfare  = welfare,
                              weight   = weight,
                              complete = FALSE)

    res_lq$dist_stats$mld |>
        expect_equal(mld_wbpip_lq)

    res_lq$dist_stats$lorenz |>
        expect_equal("lq")

    res_lb$dist_stats$lorenz |>
      expect_equal("lb")

    res_lb$dist_stats$mld |>
        expect_equal(mld_wbpip_lb)

    attributes(res_lb$dist_stats$mld) |>
        expect_equal(NULL)

    attributes(res_lq$dist_stats$mld) |>
      expect_equal(NULL)

})

# Test pipgd_polarization ####
# Inputs -----------------------------------------------------------------
test_that("pipgd_polarization inputs works as expected", {

  pipgd_polarization(welfare     = welfare,
                     weight      = NULL) |>
    expect_error()

  pipgd_polarization(welfare     = NULL,
                     weight      = weight) |>
    expect_error()

  pipgd_polarization(welfare     = welfare,
                     weight      = weight,
                     mean        = "invalid mean") |>
    expect_error()

  pipgd_polarization(welfare     = welfare,
                     weight      = weight,
                     gini        = "invalid gini") |>
    expect_error()

  pipgd_polarization(welfare     = welfare,
                     weight      = weight,
                     lorenz      = "Neither NULL, lq or lb") |>
    expect_error()

  pipgd_polarization(welfare     = welfare,
                     weight      = weight,
                     lorenz      = NULL) |>
    expect_no_error()

  pipgd_polarization(pipster_object = gd1,
                     lorenz  = NULL) |>
    expect_no_error()

})

# Outputs -----------------------------------------------------------------
test_that("pipgd_polarization outputs work as expected", {

  res          <- pipgd_polarization(welfare = welfare,
                                     weight  = weight)
  res_params   <- pipgd_polarization(pipster_object = gd1)
  res_complete <- pipgd_polarization(welfare  = welfare,
                                     weight   = weight,
                                     complete = TRUE)
  res |>
    expect_equal(res_params)

  class(res) |>
    expect_equal("list")

  class(res_complete$params) |>
    expect_equal("pipgd_params")

  names(res) |>
    expect_equal("dist_stats")

  names(res$dist_stats) |>
    expect_equal(c("gini",
                   "polarization",
                   "lorenz"))

  # Names in output list when complete = TRUE
  names(res_complete$params) |>
    expect_equal(c("gd_params",
                   "data",
                   "selected_lorenz"))

  names(res_complete$params$gd_params) |>
    expect_equal(c("lq",
                   "lb"))

  names(res_complete$params$gd_params$lq) |>
    expect_equal(names(res_complete$params$gd_params$lb))

  names(res_complete$params$gd_params$lq) |>
    expect_equal(c("reg_results",
                   "key_values",
                   "validity"))

  names(res_complete$params$gd_params$lq$reg_results) |>
    expect_equal(c("ymean",
                   "sst",
                   "coef",
                   "sse",
                   "r2",
                   "mse",
                   "se"))

  names(res_complete$params$gd_params$lq$key_values) |>
    expect_equal(c("e",
                   "m",
                   "n",
                   "r",
                   "s1",
                   "s2"))

  names(res_complete$params$gd_params$lq$validity) |>
    expect_equal(c("is_normal",
                   "is_valid",
                   "headcount"))

  names(res_complete$params$gd_params$lb$key_values) |>
    expect_equal(NULL)

  names(res_complete$params$data) |>
    expect_equal(c("welfare",
                   "weight",
                   "mean"))

  names(res_complete$params$selected_lorenz) |>
    expect_equal(c( "for_dist",
                    "for_pov",
                    "use_lq_for_dist",
                    "use_lq_for_pov" ))

  names(res_complete$results$dist_stats) |>
    expect_equal(c("gini",
                   "lorenz",
                   "polarization"))

})

test_that("pipgd_polarization calculates polarization as expected", {

  pol_benchmark_lb <- 0.242583101350098

  pol_benchmark_lq <- 0.234947626306501

  res_lq       <- pipgd_polarization(welfare  = welfare,
                                     weight   = weight,
                                     lorenz   = "lq")
  res_lb       <- pipgd_polarization(welfare  = welfare,
                                      weight  = weight,
                                      lorenz = "lb")
  res_complete <- pipgd_polarization(welfare  = welfare,
                                    weight   = weight,
                                    complete = TRUE)

  res_lq$dist_stats$polarization |>
    expect_equal(pol_benchmark_lq)

  res_lq$dist_stats$lorenz |>
    expect_equal("lq")

  res_lb$dist_stats$lorenz |>
    expect_equal("lb")

  res_lb$dist_stats$polarization |>
    expect_equal(pol_benchmark_lb)

  attributes(res_lb$dist_stats$polarization) |>
    expect_equal(NULL)

  attributes(res_lq$dist_stats$polarization) |>
    expect_equal(NULL)

})




