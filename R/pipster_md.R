
# constructor----
new_pipster_md <- function(x = double(), ...) {
  new_vctr(x, class = "pipster_md")
}


#' `pipster_md` vector
#'
#' This creates a double vector of group pipster data.
#'
#' @param x
#'  * For `pipster_md`: numeric vector
#'  * For `is_pipster_md`: object to test
#'  * For `as_pipster_md`: object that should inherit `pipster_md` class
#'
#' @return an S3 vector of class `pipster_md`
#' @export
#'
#' @examples
#' pipster_md(c(0.1, 0.5, 0.9, 1))
pipster_md <- function(x = double()) {
  x <- vec_cast(x, double())
  new_pipster_md(x)
}

#' @export
#' @rdname pipster_md
is_pipster_md <- function(x) {
  inherits(x, "pipster_md")
}

#' @export
#' @rdname pipster_md
as_percent <- function(x) {
  vec_cast(x, new_pipster_md())
}

#' @export
format.pipster_md <- function(x, ...) {
  out <- formatC(signif(vec_data(x), 3))
  out[is.na(x)] <- NA
  out
}

#' @export
vec_ptype_abbr.pipster_md <- function(x, ...) {
  "pipmd"
}






#' @export
vec_ptype2.pipster_md.pipster_md <- function(x, y, ...) {
  new_pipster_md()
}

#' @export
vec_ptype2.pipster_md.double     <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.pipster_md     <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.pipster_md.integer    <- function(x, y, ...) {
  integer()
}

#' @export
vec_ptype2.integer.pipster_md    <- function(x, y, ...) {
  integer()
}

# vec_ptype_show(pipster_md(), double(), pipster_md())

#' @export
vec_cast.pipster_md.pipster_md <- function(x, to, ...) {
  x
}


#' @export
vec_cast.pipster_md.double     <- function(x, to, ...) {
  pipster_md(x)
}

#' @export
vec_cast.double.pipster_md     <- function(x, to, ...) {
  vec_data(x)
}


#' @export
#' @method vec_arith pipster_md
vec_arith.pipster_md <- function(op, x, y, ...) {
  UseMethod("vec_arith.pipster_md", y)
}



#' @export
#' @method vec_arith.pipster_md default
vec_arith.pipster_md.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.pipster_md pipster_md
vec_arith.pipster_md.pipster_md <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_md(vec_arith_base(op, x, y)),
    "-" = new_pipster_md(vec_arith_base(op, x, y)),
    "/" = new_pipster_md(vec_arith_base(op, x, y)),
    "*" = new_pipster_md(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.pipster_md numeric
vec_arith.pipster_md.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_md(vec_arith_base(op, x, y)),
    "-" = new_pipster_md(vec_arith_base(op, x, y)),
    "/" = new_pipster_md(vec_arith_base(op, x, y)),
    "*" = new_pipster_md(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric pipster_md
vec_arith.numeric.pipster_md <- function(op, x, y, ...) {
  switch(
    op,
    "+" = new_pipster_md(vec_arith_base(op, x, y)),
    "-" = new_pipster_md(vec_arith_base(op, x, y)),
    "/" = new_pipster_md(vec_arith_base(op, x, y)),
    "*" = new_pipster_md(vec_arith_base(op, x, y)),
    stop_incompatible_op(op, x, y)
  )
}




#' @export
vec_math.pipster_md <- function(.fn, .x, ...) {
  switch(.fn,
         sum  = attr(.x, "sum"),
         mean = attr(.x, "sum") / length(.x),
         vec_math_base(.fn, .x, ...)
  )
}











#' Warning message used in default methods for all pipster S3 generic functions
#'
#' @param generic_name character: name of generic function of default method
#' @param first_arg object: first argument in the generic function, the class of
#' which determines the S3 method.
#'
#' @return warning message
#' @keywords internal
default_warning_message <- function(generic_name, first_arg){
  cli::cli_warn(paste0(generic_name, " does not know how to handle object of class ",
                       class(first_arg),
                       " and can only be used for classes `pipster_gd` and `pipster_md`."))
}








#
