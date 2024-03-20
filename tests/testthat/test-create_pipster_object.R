welfare_gd <- pip_gd$L
weight_gd  <- pip_gd$P
welfare_md <- pip_md_s$welfare
weight_md  <- pip_md_s$weight


#
# # welfare
# pip_gd$L # is cum - gd_1
# pip_gd$X # not cum or one - gd_5 and gd_3
# pip_gd$R # sum to 1
#
# # weight
# pip_gd$W # sum to 1
# pip_gd$P # is cum

# gd_1: cumulative welfare (pip_gd$L)
#       cumulative weight  (pip_gd$P)
# gd_2: sum to 1 welfare   (pip_gd$R)
#       sum to one weight  (pip_gd$W)
# gd_5: welfare is mean of interval (pip_gd$X)
#       sum to one weight  (pip_gd$W)







#_______________________________________________________________________________
# Tests
#_______________________________________________________________________________

test_that("Correct treatment of gd types", {


  gd1 <- create_pipster_object(welfare = pip_gd$L, # is cumulative
                               weight  = pip_gd$P) # is cumulative
  gd2 <- create_pipster_object(welfare = pip_gd$R, # to one
                               weight  = pip_gd$W) # to one
  gd5 <- create_pipster_object(welfare = pip_gd$X, # neither
                               weight  = pip_gd$W) # to one

  # outputs are equal
  expect_equal(gd1, gd2)
  expect_equal(round(gd1$welfare, 4),
               round(gd5$welfare, 4))
  expect_equal(round(gd1$weight, 4),
               round(gd5$weight, 4))
  expect_equal(gd1$params$gd_params$lq$reg_results$coef,
               gd1$params$gd_params$lq$reg_results$coef)

  # error
  expect_error(gd3 <- create_pipster_object(welfare = pip_gd$X, # neither
                                            weight  = pip_gd$P),  # is cumulative
               "Group data of type `gd_3` not supported.")

})


test_that("create_pipster_object - errors", {

  expect_error(
    create_pipster_object(welfare = c(welfare_gd[1],
                                      NA,
                                      welfare_gd[2:length(welfare_gd)]),
                          weight  = weight_gd))

  expect_error(
    create_pipster_object(welfare = LETTERS,
                          weight  = c(weight_gd[1],
                                      NA,
                                      weight_gd[2:length(weight_gd)])))
  expect_error(
    create_pipster_object(welfare = welfare_gd,
                          weight  = c(weight_gd[1],
                                      NA,
                                      weight_gd[3:length(weight_gd)])))


})




test_that("create_pipster_object - correct class identified", {

  obj1 <- create_pipster_object(welfare = welfare_gd,
                                weight  = weight_gd)
  obj2 <- create_pipster_object(welfare = welfare_md,
                                weight  = weight_md)
  obj3 <- create_pipster_object(welfare = welfare_md,
                                weight  = weight_md,
                                imputation_id = rep(c(1, 2),
                                                    length(weight_md)/2))

  expect_equal(obj1$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))
  expect_equal(obj2$welfare |> class(),
               c("pipster_md",
                 "vctrs_vctr"))
  expect_equal(obj3$welfare |> class(),
               c("pipster_md",
                 "vctrs_vctr"))
  expect_true(
    length(obj3$imputation_id |>
             funique()) ==   # imputation id works
      2*length(obj2$imputation_id |>
                 funique()))


})








