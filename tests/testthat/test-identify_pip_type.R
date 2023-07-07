skip_if_not_installed("withr")
library(withr)
local_options(list(pipster.verbose  = FALSE))

# W: Weights, share of population, sum up to 100
# X: welfare vector with mean welfare by decile
# P:Cumulative share of population
# L: Cumulative share of welfare
# R: share of welfare, sum up to 1.

W = c(0.92, 2.47, 5.11, 7.9, 9.69, 15.24, 13.64, 16.99, 10, 9.78, 3.96, 1.81, 2.49)
X = c(24.84, 35.8, 45.36, 55.1, 64.92, 77.08, 91.75, 110.64, 134.9, 167.76, 215.48, 261.66, 384.97)
P = c(0.0092, 0.0339, 0.085, 0.164, 0.2609, 0.4133, 0.5497, 0.7196, 0.8196, 0.9174, 0.957, 0.9751, 1)
L = c(0.00208, 0.01013, 0.03122, 0.07083, 0.12808, 0.23498, 0.34887, 0.51994, 0.6427, 0.79201, 0.86966, 0.91277, 1)

R = (W * X) / sum(W * X)



test_that("Identifies group data", {

  # type 1 ------
  ## up to 1 ---------
  gd1_1 <- identify_pip_type(welfare = L,
                          weight = P)

  expect_equal(gd1_1, "gd_1")

  ## up to 100 ---------
  gd1_100 <- identify_pip_type(welfare = L*100,
                           weight = P)

  expect_equal(gd1_100, "gd_1")

  # type 2 -----------

  ## up to 1 -----------
  gd2_1 <- identify_pip_type(welfare = R,
                            weight = W/100)

  expect_equal(gd2_1, "gd_2")

  ## up to 100 ---------
  gd2_100 <- identify_pip_type(welfare = R*100,
                               weight = W)

  expect_equal(gd2_100, "gd_2")


  # type 5 -----------

  ## up to 1 -----------
  gd5_1 <- identify_pip_type(welfare = X,
                             weight = W/100)

  expect_equal(gd5_1, "gd_5")

  ## up to 100 ---------
  gd5_100 <- identify_pip_type(welfare = X,
                               weight = W)

  expect_equal(gd5_100, "gd_5")

  # type 3 -----------

  ## up to 1 -----------
  gd3_1 <- identify_pip_type(welfare = X,
                             weight  = P)

  expect_equal(gd3_1, "gd_3")

  ## up to 100 ---------
  gd3_100 <- identify_pip_type(welfare = X,
                               weight = P*100)

  expect_equal(gd3_100, "gd_3")


})

l <- 300
Y <- sample(1000, l,replace = TRUE)
Q <- sample(35, l,replace = TRUE)
I <- sample(1:5, l,replace = TRUE)

test_that("Identifies microdata", {
  md <- identify_pip_type(welfare = Y,
                          weight  = Q)

  expect_equal(md, "md")


  id <- identify_pip_type(welfare = Y,
                          weight  = Q,
                          imputation_id = I)

  expect_equal(id, "id")

  I2 <- rep(1, l)
  md2 <- identify_pip_type(welfare = Y,
                          weight  = Q,
                          imputation_id = I2)

  expect_equal(md2, "md")


})


test_that("Errors are triggered", {

  # vectors of the different length --------
  Y <- sample(1000, 300,replace = TRUE)
  Q <- sample(35, 299,replace = TRUE)
  identify_pip_type(welfare = Y,
                    weight  = Q) |>
    expect_error()

  # vectors of the different length --------
  Y <- sample(1000, 300,replace = TRUE)
  Q <- sample(35, 300,replace = TRUE)
  I <- sample(35, 299,replace = TRUE)
  identify_pip_type(welfare = Y,
                    weight  = Q,
                    imputation_id = I) |>
    expect_error()

  # negative present in weights ------
  Y <- sample(1000, 300,replace = TRUE)
  Q <- sample(35, 300,replace = TRUE)
  Q[sample(length(Q), 1)] <- -18
  identify_pip_type(welfare = Y,
                    weight  = Q) |>
    expect_error()


})


test_that("Warnings", {
  # NA present in one of the vectors ----
  expect_equal(2 * 2, 4)

  # negative present in welfare -----


  # unequal number of observations in imputations -----


  # identified as microdata but few observations  ----

})
