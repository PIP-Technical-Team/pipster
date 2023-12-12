
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pipster

<!-- badges: start -->
<!-- badges: end -->

The goal of pipster is to make use of `{wbpip}` functions easily.

## Installation

You can install the development version of pipster from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/pipster")
```

## Examples

``` r
library(pipster)
## basic example code
```

### Identify type of data

#### Group Data

``` r
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
```

``` r
# type 1 ------
## up to 1 ---------
identify_pip_type(welfare = L,
                    weight = P)
#> [1] "gd_1"

## up to 100 ---------
identify_pip_type(welfare = L*100,
                    weight = P)
#> [1] "gd_1"

# type 2 -----------
## up to 1 -----------
identify_pip_type(welfare = R,
                    weight = W/100)
#> ! vectors not sorted
#> [1] "gd_2"

## up to 100 ---------
identify_pip_type(welfare = R*100,
                   weight = W)
#> ! vectors not sorted
#> [1] "gd_2"

# type 5 -----------
identify_pip_type(welfare = X,
                    weight = W/100)
#> [1] "gd_5"

# type 3 -----------
identify_pip_type(welfare = X,
                   weight  = P)
#> [1] "gd_3"
```

#### Microdata

``` r

# l: length
# Y: welfare
# Q: population or weights
# I: imputation ID

l <- 300
Y <- sample(1000, l,replace = TRUE)
Q <- sample(35, l,replace = TRUE)
I <- sample(1:5, l,replace = TRUE)
```

``` r
identify_pip_type(welfare = Y,
                  weight  = Q)
#> ! vectors not sorted
#> [1] "md"
identify_pip_type(welfare = Y,
                  weight  = Q,
                  imputation_id = I)
#> ! vectors not sorted
#> [1] "id"

I2 <- rep(1, l)
identify_pip_type(welfare = Y,
                  weight  = Q,
                  imputation_id = I2)
#> ! vectors not sorted
#> [1] "md"
```
