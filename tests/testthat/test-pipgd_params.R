
test_that("pipgd_params output", {
  res <- pipgd_params(
    welfare = pip_gd$L,
    weight = pip_gd$P)
  # Class --------
  class(res) |>
    expect_equal("pipgd_params")

  # Names -------------
  names(res) |>
    expect_equal(c("gd_params", "data"))

  names(res$gd_params) |>
    expect_equal(c("lq", "lb"))


  names(res$gd_params$lq) |>
    expect_equal(names(res$gd_params$lb))


  names(res$gd_params$lq) |>
    expect_equal(c("reg_results", "key_values"))


  names(res$data) |>
    expect_equal(c("welfare", "weight"))


})
