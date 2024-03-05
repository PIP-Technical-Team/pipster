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
## Correct method
test_that("Correct method for each pipster object classes", {

  expect_is(get_pov_headcount(gd_object, povline = 1), "list")
  expect_is(get_pov_headcount(md_object, povline = 1), "list")
  expect_error(get_pov_headcount(invalid_object, povline = 1),
               "No default exist. Please check object class.")
})

## Same output as standard
test_that("get_pov_headcount() works",)

