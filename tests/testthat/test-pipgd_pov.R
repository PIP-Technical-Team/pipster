welfare_cumshare <- pip_gd$L
weight_cumshare  <- pip_gd$P

#_______________________________________________________________________________
# Test headcount functions -----------------------------------------------------
#_______________________________________________________________________________

# test_that("pipgd_pov_headcount_nv works as expected", {
#
#   #
#   hc <- pipgd_pov_headcount_nv(
#     welfare  = welfare_cumshare,
#     weight   = weight_cumshare,
#     povline  = welfare_cumshare[4],
#     complete = FALSE
#   )$pov_stats$headcount
#
#   expect_equal(
#     hc,
#     weight_cumshare[4]
#   )
#
#
# })

#_______________________________________________________________________________
# Test poverty gap functions ---------------------------------------------------
#_______________________________________________________________________________


#_______________________________________________________________________________
# Test severity functions ------------------------------------------------------
#_______________________________________________________________________________

test_that("pipgd_pov_severity_nv() - `params` works", {
  params <- pipgd_params(
    welfare = welfare_cumshare,
    weight = weight_cumshare
  )
  ps_params <- pipgd_pov_severity_nv(
    params = params
  )
  ps <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare
  )

  expect_equal(
    ps, ps_params
  )

})
test_that("pipgd_pov_severity_nv() - `mean` works", {

  ps1 <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare,
    mean    = 1
  )
  psnone <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare
  )
  psnull <- pipgd_pov_severity_nv(
    welfare = welfare_cumshare,
    weight  = weight_cumshare,
    mean    = NULL
  )

  expect_equal(
    ps1, psnone
  )
  expect_equal(
    ps1, psnull
  )
})



test_that("pipgd_pov_severity_nv() - `lorenz` works", {

})
test_that("pipgd_pov_severity_nv() - `pov_gap` works", {

})
test_that("pipgd_pov_severity_nv() - `complete` works", {

})




