
rmv_attr <-  \(x) {
  y <- attributes(x) |>
    names()

  for ( i in seq_along(y)) {
    attr(x, y[i]) <- NULL
  }
  x
}

gd_object <- create_pipster_object(welfare = pip_gd$L,
                                   weight  = pip_gd$P)
md_object <- create_pipster_object(welfare = pip_md_s$welfare,
                                   weight  = pip_md_s$weight)

welfare_gd <- pip_gd$L |>
  rmv_attr()

weight_gd  <- pip_gd$P |>
  rmv_attr()
welfare_md <- pip_md_s$welfare |>
  rmv_attr()
weight_md  <- pip_md_s$weight |>
  rmv_attr()


#_______________________________________________________________________________
# Tests
#_______________________________________________________________________________


test_that("get_quantile works", {

  # standard
  out1_gd <- get_quantile(pipster_object = gd_object)
  out1_md <- get_quantile(pipster_object = md_object)
  out2_gd <- pipgd_quantile(welfare      = welfare_gd,
                            weight       = weight_gd)
  out2_md <- pipmd_quantile(welfare      = welfare_md,
                            weight       = weight_md,
                            format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile)
  expect_equal(out1_md,
               out2_md)

  # use arguments - n
  out1_gd <- get_quantile(pipster_object = gd_object,
                          n              = 5)
  out1_md <- get_quantile(pipster_object = md_object,
                          n              = 5)
  out2_gd <- pipgd_quantile(welfare      = welfare_gd,
                            weight       = weight_gd,
                            n            = 5)
  out2_md <- pipmd_quantile(welfare      = welfare_md,
                            weight       = weight_md,
                            n            = 5,
                            format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile)
  expect_equal(out1_md,
               out2_md)

  # use arguments - popshare
  out1_gd <- get_quantile(pipster_object = gd_object,
                          popshare       = 0.5)
  out1_md <- get_quantile(pipster_object = md_object,
                          popshare       = 0.5)
  out2_gd <- pipgd_quantile(welfare      = welfare_gd,
                            weight       = weight_gd,
                            popshare     = 0.5)
  out2_md <- pipmd_quantile(welfare      = welfare_md,
                            weight       = weight_md,
                            popshare     = 0.5,
                            format       = "list")

  expect_equal(names(out1_gd), "50%")
  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile)
  expect_equal(out1_md,
               out2_md)

})



test_that("get_welfare_share_at works", {
  # skip()
  # standard
  out1_gd <- get_welfare_share_at(pipster_object = gd_object)
  out1_md <- get_welfare_share_at(pipster_object = md_object)
  out2_gd <- pipgd_welfare_share_at(welfare      = welfare_gd,
                                    weight       = weight_gd)
  out2_md <- pipmd_welfare_share_at(welfare      = welfare_md,
                                    weight       = weight_md,
                                    format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$welfare_share_at)
  expect_equal(out1_md,
               out2_md)

  # use arguments - n
  out1_gd <- get_welfare_share_at(pipster_object = gd_object,
                                  n              = 5)
  out1_md <- get_welfare_share_at(pipster_object = md_object,
                                  n              = 5)
  out2_gd <- pipgd_welfare_share_at(welfare      = welfare_gd,
                                    weight       = weight_gd,
                                    n            = 5)
  out2_md <- pipmd_welfare_share_at(welfare      = welfare_md,
                                    weight       = weight_md,
                                    n            = 5,
                                    format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$welfare_share_at)
  expect_equal(out1_md,
               out2_md)

  # use arguments - popshare
  out1_gd <- get_welfare_share_at(pipster_object = gd_object,
                                  popshare       = 0.4)
  out1_md <- get_welfare_share_at(pipster_object = md_object,
                                  popshare       = 0.4)
  out2_gd <- pipgd_welfare_share_at(welfare      = welfare_gd,
                                    weight       = weight_gd,
                                    popshare     = 0.4)
  out2_md <- pipmd_welfare_share_at(welfare      = welfare_md,
                                    weight       = weight_md,
                                    popshare     = 0.4,
                                    format       = "list")

  expect_equal(names(out1_gd), "40%")
  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$welfare_share_at)
  expect_equal(out1_md,
               out2_md)

})



test_that("get_quantile_welfare_share works", {
  # skip()
  # standard
  out1_gd <- get_quantile_welfare_share(pipster_object = gd_object)
  out1_md <- get_quantile_welfare_share(pipster_object = md_object)
  out2_gd <- pipgd_quantile_welfare_share(welfare      = welfare_gd,
                                          weight       = weight_gd)
  out2_md <- pipmd_quantile_welfare_share(welfare      = welfare_md,
                                          weight       = weight_md,
                                          format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile_welfare_share)
  expect_equal(out1_md,
               out2_md)

  # use arguments - n
  out1_gd <- get_quantile_welfare_share(pipster_object = gd_object,
                                        n              = 5)
  out1_md <- get_quantile_welfare_share(pipster_object = md_object,
                                        n              = 5)
  out2_gd <- pipgd_quantile_welfare_share(welfare      = welfare_gd,
                                          weight       = weight_gd,
                                          n            = 5)
  out2_md <- pipmd_quantile_welfare_share(welfare      = welfare_md,
                                    weight       = weight_md,
                                    n            = 5,
                                    format       = "list")

  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile_welfare_share)
  expect_equal(out1_md,
               out2_md)

  # use arguments - popshare
  out1_gd <- get_quantile_welfare_share(pipster_object = gd_object,
                                        popshare       = 0.4,
                                        n = NULL)
  out1_md <- get_quantile_welfare_share(pipster_object = md_object,
                                        popshare       = 0.4,
                                        n = NULL)
  out2_gd <- pipgd_quantile_welfare_share(welfare      = welfare_gd,
                                          weight       = weight_gd,
                                          popshare     = 0.4,
                                          n = NULL)
  out2_md <- pipmd_quantile_welfare_share(welfare      = welfare_md,
                                          weight       = weight_md,
                                          popshare     = 0.4,
                                          format       = "list",
                                          n            = NULL)

  expect_equal(names(out1_gd), "40%")
  expect_equal(out1_gd |> unlist() |> unname(),
               out2_gd$dist_stats$quantile_welfare_share)
  expect_equal(out1_md,
               out2_md)

})



test_that("get_gini works", {

  # standard
  out1_gd <- get_gini(pipster_object = gd_object)
  out1_md <- get_gini(pipster_object = md_object)
  out2_gd <- pipgd_gini(welfare      = welfare_gd,
                        weight       = weight_gd)
  out2_md <- pipmd_gini(welfare      = welfare_md,
                        weight       = weight_md,
                        format       = "list")

  expect_equal(out1_gd$gini ,
               out2_gd$dist_stats$gini)
  expect_equal(out1_md,
               out2_md)


})


test_that("get_polarization works", {

  # standard
  #out1_gd <- get_polarization(pipster_object = gd_object)
  out1_md <- get_polarization(pipster_object = md_object)
  #out2_gd <- pipgd_polarization(welfare      = welfare_gd,
  #                              weight       = weight_gd)
  out2_md <- pipmd_polarization(welfare      = welfare_md,
                                weight       = weight_md,
                                format       = "list")

  #expect_equal(out1_gd$polarization ,
  #             out2_gd$dist_stats$polarization)
  expect_equal(out1_md,
               out2_md)

})





test_that("get_mld works", {

  # standard
  out1_gd <- get_mld(pipster_object = gd_object)
  out1_md <- get_mld(pipster_object = md_object)
  out2_gd <- pipgd_mld(welfare      = welfare_gd,
                        weight      = weight_gd)
  out2_md <- pipmd_mld(welfare      = welfare_md,
                        weight      = weight_md,
                        format      = "list")

  expect_equal(out1_gd$mld ,
               out2_gd$dist_stats$mld)
  expect_equal(out1_md,
               out2_md)

})















