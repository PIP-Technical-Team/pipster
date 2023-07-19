## code to prepare `pip_md` dataset goes here
library(acid)
library(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## microdata --------

data(dist.para.t)
set.seed(123)
theta <-  c(1, 2, 2, 3)
p0    <- 0.1
p1    <- 0.3
p2    <- 0.6
n     <- 1000
welfare <- ysample.md(n, "LOGNO", "LOGNO", theta, p0, p1, p2, dist.para.t)

weight  <- sample(1e4, n, replace = TRUE)

pip_md <- tibble::tibble(welfare = welfare,
                         weight  = weight)



attr(pip_md$welfare,  "label") <- "welfare(income of consumption)"
attr(pip_md$weight,   "label") <- "population wieghts"


usethis::use_data(pip_md, overwrite = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## imputed data --------
n_i <- 5

o <- sample(1:n, n*n_i, replace = TRUE)

pip_id <- tibble::tibble(welfare = welfare[o],
                          weight  = weight[o],
                          imputation_id = rep(c(1:n_i), n)
)



attr(pip_id$welfare,       "label") <- "welfare(income of consumption)"
attr(pip_id$weight,        "label") <- "population wieghts"
attr(pip_id$imputation_id, "label") <- "Imputation ID"


usethis::use_data(pip_id, overwrite = TRUE)





