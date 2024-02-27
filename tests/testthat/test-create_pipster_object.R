welfare_gd <- pip_gd$L
weight_gd  <- pip_gd$P
welfare_md <- pip_md_s$welfare
weight_md  <- pip_md_s$weight

#_______________________________________________________________________________
# Tests
#_______________________________________________________________________________

## Valid Inputs
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

## Correct Class
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

## Correct Class - Complete - One per Type
test_that("create_pipster_object - correct class identified - gd_1", {

  # gd_1
  gd_1 <- create_pipster_object(welfare = pip_gd$L,
                                weight = pip_gd$P)
  gd_1_100 <- create_pipster_object(welfare = pip_gd$L*100,
                                    weight = pip_gd$P)


  expect_equal(gd_1$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

  expect_equal(gd_1_100$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

})

test_that("create_pipster_object - correct class identified - gd_2", {

  # gd_2
  gd_2 <- create_pipster_object(welfare = pip_gd$R,
                                weight = pip_gd$W/100)
  gd_2_100 <- create_pipster_object(welfare = pip_gd$R*100,
                                    weight = pip_gd$W)


  expect_equal(gd_2$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

  expect_equal(gd_2_100$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

})



test_that("create_pipster_object - correct class identified - gd_3", {

  # gd_3
  gd_3 <- create_pipster_object(welfare = pip_gd$X,
                                weight  = pip_gd$P)
  gd_3_100 <- create_pipster_object(welfare = pip_gd$X,
                                    weight = pip_gd$P*100)


  expect_equal(gd_3$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

  expect_equal(gd_3_100$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

})

test_that("create_pipster_object - correct class identified - gd_5", {

  # gd_5
  gd_5 <- create_pipster_object(welfare = pip_gd$X,
                                weight = pip_gd$W/100)
  gd_5_100 <- create_pipster_object(welfare = pip_gd$X,
                                    weight = pip_gd$P*100)


  expect_equal(gd_5$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

  expect_equal(gd_5_100$welfare |> class(),
               c("pipster_gd",
                 "vctrs_vctr"))

})



## Correct Class with NULL weight
test_that("create_pipster_object - correct class identified - null weight for md", {

  obj21 <- create_pipster_object(welfare = welfare_md,
                                weight  = NULL)
  obj31 <- create_pipster_object(welfare = welfare_md,
                                weight  = NULL,
                                imputation_id = rep(c(1, 2),
                                                    length(weight_md)/2))

  expect_equal(obj21$welfare |> class(),
               c("pipster_md",
                 "vctrs_vctr"))
  expect_equal(obj31$welfare |> class(),
               c("pipster_md",
                 "vctrs_vctr"))
  expect_true(
    length(obj31$imputation_id |>
             funique()) ==   # imputation id works
      2*length(obj21$imputation_id |>
                 funique()))


})

