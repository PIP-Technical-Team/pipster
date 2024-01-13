res <- pipgd_params(
  welfare = pip_gd$L,
  weight  = pip_gd$P)
res_class <- class(res)
test_that("pipgd_validate_lorenz works", {
  withr::local_options(pipster.return_complete = TRUE)


  params <- pipgd_validate_lorenz(res, complete = FALSE)
  expect_equal(class(params), "list",
               label = "right class with complete FALSE")

  params <- pipgd_validate_lorenz(res)
  expect_equal(class(params), res_class,
               label = "right class with complete TRUE")

})


test_that("pipgd_select_lorenz work", {
  res <-
    pipgd_params(welfare = pip_gd$L,
                 weight = pip_gd$P) |>
    pipgd_validate_lorenz(complete = TRUE) |>
    pipgd_select_lorenz()

  res2 <- pipgd_select_lorenz(welfare = pip_gd$L,
                              weight = pip_gd$P)
  expect_equal(res, res2)

})
