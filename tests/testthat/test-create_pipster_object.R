welfare_gd <- pip_gd$L
weight_gd  <- pip_gd$P
welfare_md <- pip_md_s$welfare
weight_md  <- pip_md_s$weight

#_______________________________________________________________________________
# Tests
#_______________________________________________________________________________

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





