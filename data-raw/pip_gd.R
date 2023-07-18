## code to prepare `pip_gd` dataset goes here
# library(tibble)


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


pip_gd <- tibble::tibble(W = W,
                 X = X,
                 P = P,
                 L = L,
                 R = R)

usethis::use_data(pip_gd, overwrite = TRUE)
