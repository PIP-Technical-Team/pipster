
# Constructor function ---------------------------------------------------------

new_pipster_gd <- function(x = double(), ...) {
  new_vctr(x, class = "pipster_gd")
}


# User-facing functions --------------------------------------------------------

#' `pipster_gd` vector
#'
#' This creates a double vector of pipster grouped data. It casts the input to
#' double(numeric), before passing it to the constructor function `new_pipster_gd()`.
#' `is_pipster_gd()` whether an object inherits from the `pipster_gd` class, and
#' `as_pipster_gd()` casts and object to a `pipster_gd` object.
#'
#'
#' @param x
#'  * For `pipster_gd`: numeric vector
#'  * For `is_pipster_gd`: object to test
#'  * For `as_pipster_gd`: object that should inherit `pipster_gd` class
#'
#' @return an S3 vector of class `pipster_gd`
#' @export
#'
#' @examples
#' pipster_gd(c(0.1, 0.5, 0.9, 1))
pipster_gd <- function(x = double()) {
  x <- vec_cast(x, double())
  new_pipster_gd(x)
}

#' @export
#' @rdname pipster_gd
is_pipster_gd <- function(x) {
  inherits(x, "pipster_gd")
}

#' @export
#' @rdname pipster_gd
as_pipster_gd <- function(x) {
  vec_cast(x, new_pipster_gd())
}

#' @export
format.pipster_gd <- function(x, ...) {
  out <- formatC(signif(vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

#' @export
vec_ptype_abbr.pipster_gd <- function(x, ...) {
  "pipgd"
}






#' @export
vec_ptype2.pipster_gd.pipster_gd <- function(x, y, ...) {
  new_pipster_gd()
}

#' @export
vec_ptype2.pipster_gd.double     <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.pipster_gd     <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.pipster_gd.integer    <- function(x, y, ...) {
  integer()
}

#' @export
vec_ptype2.integer.pipster_gd    <- function(x, y, ...) {
  integer()
}

# vec_ptype_show(pipster_gd(), double(), pipster_gd())

#' @export
vec_cast.pipster_gd.pipster_gd <- function(x, to, ...) {
  x
}


#' @export
vec_cast.pipster_gd.double     <- function(x, to, ...) {
  pipster_gd(x)
}

#' @export
vec_cast.double.pipster_gd     <- function(x, to, ...) {
  vec_data(x)
}

# vec_cast(0.5, pipster_gd())
# vec_cast(pipster_gd(0.5), double())


#' @export
#' @method vec_arith pipster_gd
vec_arith.pipster_gd <- function(op, x, y, ...) {
  UseMethod("vec_arith.pipster_gd", y)
}



#' @export
#' @method vec_arith.pipster_gd default
vec_arith.pipster_gd.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.pipster_gd pipster_gd
vec_arith.pipster_gd.pipster_gd <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_gd(vec_arith_base(op, x, y)),
    "-" = new_pipster_gd(vec_arith_base(op, x, y)),
    "/" = new_pipster_gd(vec_arith_base(op, x, y)),
    "*" = new_pipster_gd(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.pipster_gd numeric
vec_arith.pipster_gd.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_gd(vec_arith_base(op, x, y)),
    "-" = new_pipster_gd(vec_arith_base(op, x, y)),
    "/" = new_pipster_gd(vec_arith_base(op, x, y)),
    "*" = new_pipster_gd(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric pipster_gd
vec_arith.numeric.pipster_gd <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_gd(vec_arith_base(op, x, y)),
    "-" = new_pipster_gd(vec_arith_base(op, x, y)),
    "/" = new_pipster_gd(vec_arith_base(op, x, y)),
    "*" = new_pipster_gd(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}



#' @export
vec_math.pipster_gd <- function(.fn, .x, ...) {
  switch(.fn,
         sum  = attr(.x, "sum"),
         mean = attr(.x, "sum") / length(.x),
         vec_math_base(.fn, .x, ...)
  )
}














