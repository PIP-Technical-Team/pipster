test_that("Groupd data is converted properly", {

  # Group  Data 1 --------
  ## it does not modify it is already setup ----
  as_pip(dt = pip_gd,
         welfare_var = "L",
         weight_var = "P",
         pip_type = "gd_1") |>
    expect_equal(pip_gd, ignore_attr =  TRUE)


  ## transform to range from 0 to 1 -----
  pip_gd2 <- pip_gd |>
    ftransform(L = L*100)


  as_pip(dt = pip_gd2,
         welfare_var = "L",
         weight_var = "P",
         pip_type = "gd_1") |>
    expect_equal(pip_gd, ignore_attr =  TRUE)

  ## identify type ----
  as_pip(dt = pip_gd2,
         welfare_var = "L",
         weight_var = "P") |>
    expect_equal(pip_gd, ignore_attr =  TRUE)

  # Group Data 2 ------

  ## convert R and W ----
  gd <- as_pip(dt = pip_gd,
               welfare_var = "R",
               weight_var = "W",
               pip_type = "gd_2")


  R <-
    c(
      0.00207969213771117,
      0.0101267960798648,
      0.0312205355219995,
      0.0708335890919907,
      0.128081800059234,
      0.234983916667072,
      0.348872469473792,
      0.51993905110042,
      0.642703206642515,
      0.792012521761858,
      0.86966615566504,
      0.912765984263892,
      1
    )
  W <-
    c(
      0.0092,
      0.0339,
      0.085,
      0.164,
      0.2609,
      0.4133,
      0.5497,
      0.7196,
      0.8196,
      0.9174,
      0.957,
      0.9751,
      1
    )

  expect_equal(gd$R, R)
  expect_equal(gd$W, W)



  ## wrong type provided
  as_pip(dt = pip_gd,
         welfare_var = "R",
         weight_var = "W",
         pip_type = "gd_1") |>
    expect_error()



})



test_that("Microdata is converted properly", {

  # check data is sorted -------
  as_pip(dt = pip_md,
         welfare_var = "welfare",
         weight_var = "weight",
         pip_type = "md") |>
    expect_equal(roworderv(pip_md, "welfare"),
                 ignore_attr =  TRUE)

})


test_that("Imputed data", {
  as_pip(dt = pip_id,
         welfare_var = "welfare",
         weight_var = "weight",
         imputation_id_var = "imputation_id",
         pip_type = "id") |>
    expect_equal(roworderv(pip_id, c("imputation_id", "welfare")),
                 ignore_attr =  TRUE)
})
