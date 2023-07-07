

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


  # type 2 -----------

  ## up to 1 -----------
  gd5_1 <- identify_pip_type(welfare = X,
                             weight = W/100)

  expect_equal(gd5_1, "gd_5")

  ## up to 100 ---------
  gd5_100 <- identify_pip_type(welfare = X,
                               weight = W)

  expect_equal(gd5_100, "gd_5")


})

test_that("Identifies microdata", {
  expect_equal(2 * 2, 4)
})
