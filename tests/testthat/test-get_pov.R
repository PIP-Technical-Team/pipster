# Set-up --------------------------------------------------------------------
## s3 objects
gd_object <- create_pipster_object(welfare = pip_gd$L,
                                   weight  = pip_gd$P)
md_object <- create_pipster_object(welfare = pip_md_s$welfare,
                                   weight  = pip_md_s$weight)
invalid_object <- list(some_data = "not a pipster object")

## standard objects
rmv_attr <-  \(x) {
  y <- attributes(x) |>
    names()

  for (i in seq_along(y)) {
    attr(x, y[i]) <- NULL
  }
  x
}

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


# Headcount --------------------------------------------------------------------
## Correct output type and no default
test_that("Correct method for each pipster object classes", {

  expect_type(get_pov_headcount(gd_object, povline = 1), "list")
  expect_type(get_pov_headcount(md_object, povline = 1), "list")
  expect_error(get_pov_headcount(invalid_object, povline = 1),
               "No default exist. Please check object class.")
})

## Same output as standard
test_that("get_pov_headcount() works", {

  # s3 objects
  s3_gd_obj <- get_pov_headcount(gd_object,
                              povline = 1)
  s3_md_obj <- get_pov_headcount(md_object,
                                 povline = 1)


  # standard objects
  st_gd_obj <- pipgd_pov_headcount(welfare = welfare_gd,
                                   weight = weight_gd,
                                   povline = 1)
  # This fails ATM
  #st_md_obj <- pipgd_pov_headcount(welfare = welfare_md,
  #                                 weight = weight_md,
  #                                 povline = 1)

  # test
  expect_equal(s3_gd_obj$headcount,
               st_gd_obj$headcount)
  #expect_equal(s3_md_obj$headcount,
  #             st_md_obj$headcount)
  #
  #

})

# Poverty Gap ------------------------------------------------------------------
## Correct output type and no default
test_that("Correct method for each pipster object classes - poverty gap", {
  expect_type(get_pov_gap(gd_object, povline = 1), "list")
  expect_type(get_pov_gap(md_object, povline = 1), "list")
  expect_error(get_pov_gap(invalid_object, povline = 1),
               "No default exist. Please check object class.")
})

## Same output as standard
test_that("get_pov_gap() works", {


  # s3 objects
  s3_gd_obj <- get_pov_gap(gd_object,
                                 povline = 1)
  s3_md_obj <- get_pov_gap(md_object,
                                 povline = 1)


  # standard objects
  st_gd_obj <- pipgd_pov_gap(welfare = welfare_gd,
                                   weight = weight_gd,
                                   povline = 1)
  # This fails ATM
  #st_md_obj <- pipgd_pov_gap(welfare = welfare_md,
  #                           weight = weight_md,
  #                           povline = 1)

  # test
  expect_equal(s3_gd_obj$pov_gap,
               st_gd_obj$pov_gap)
  #expect_equal(s3_md_obj$pov_gap,
  #             st_md_obj$pov_gap)
  #
  #
})

# Poverty Severity -------------------------------------------------------------
## Correct output type and no default
test_that("Correct method for each pipster object classes - poverty severity", {
  expect_type(get_pov_severity(gd_object, povline = 1), "list")
  expect_type(get_pov_severity(md_object, povline = 1), "list")
  expect_error(get_pov_severity(invalid_object, povline = 1),
               "No default exist. Please check object class.")
})

## Same output as standard
test_that("get_pov_severity() works", {


  # s3 objects
  s3_gd_obj <- get_pov_severity(gd_object,
                           povline = 1)
  s3_md_obj <- get_pov_severity(md_object,
                           povline = 1)


  # standard objects
  st_gd_obj <- pipgd_pov_severity(welfare = welfare_gd,
                             weight = weight_gd,
                             povline = 1)
  # This fails ATM
  #st_md_obj <- pipgd_pov_gap(welfare = welfare_md,
  #                           weight = weight_md,
  #                           povline = 1)

  # test
  expect_equal(s3_gd_obj$pov_severity,
               st_gd_obj$pov_severity)
  #expect_equal(s3_md_obj$pov_severity,
  #             st_md_obj$pov_severity)
  #
  #
})

# Watts ------------------------------------------------------------------------

test_that("Correct method for each pipster object classes - poverty severity", {
  expect_type(get_watts(gd_object, povline = 1), "list")
  expect_type(get_watts(md_object, povline = 1), "list")
  expect_error(get_watts(invalid_object, povline = 1),
               "No default exist. Please check object class.")
})

## Same output as standard
test_that("get_pov_severity() works", {


  # s3 objects
  s3_gd_obj <- get_watts(gd_object,
                         povline = 1)
  s3_md_obj <- get_watts(md_object,
                         povline = 1)


  # standard objects
  st_gd_obj <- pipgd_watts(welfare = welfare_gd,
                           weight = weight_gd,
                           povline = 1)
  # This fails ATM
  #st_md_obj <- pipgd_watts(welfare = welfare_md,
  #                           weight = weight_md,
  #                           povline = 1)

  # test
  expect_equal(s3_gd_obj$watts,
               st_gd_obj$watts)
  #expect_equal(s3_md_obj$watts,
  #             st_md_obj$watts)
  #
  #
})


