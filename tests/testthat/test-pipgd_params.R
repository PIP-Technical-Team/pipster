test_that("pipgd_params works", {
  res <- pipgd_params(
    welfare = pip_gd$L,
    weight = pip_gd$P)

  class(res) |>
    expect_equal("pipgd_params")

})
