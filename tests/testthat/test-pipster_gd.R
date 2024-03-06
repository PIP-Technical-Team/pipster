# Old tests ----
# Testing new_pipster_gd and pipster_gd functions
test_that("new_pipster_gd and pipster_gd work correctly", {
  # Test for normal input
  v <- pipster_gd(c(0.1, 0.5, 0.9, 1))
  expect_s3_class(v, "pipster_gd")
  expect_equal(v |> unclass(), c(0.1, 0.5, 0.9, 1))

  # Test for edge cases like empty vector and NA
  v_empty <- pipster_gd(numeric(0))
  expect_s3_class(v_empty, "pipster_gd")
  expect_equal(v_empty |> unclass(), numeric(0))

  v_na <- pipster_gd(NA)
  expect_s3_class(v_na, "pipster_gd")
  expect_true(all(is.na(v_na)))
})

# Testing is_pipster_gd function
test_that("is_pipster_gd identifies pipster_gd objects correctly", {
  expect_true(is_pipster_gd(pipster_gd(1:5)))
  expect_false(is_pipster_gd(1:5))
  expect_false(is_pipster_gd("not a pipster_gd"))
})

# Testing as_pipster_gd function
test_that("as_pipster_gd casts correctly", {
  v <- as_pipster_gd(as.double(c(1:5)))
  expect_s3_class(v, "pipster_gd")

  # Testing with invalid input
  expect_error(as_pipster_gd("invalid"))
})

# Testing format.pipster_gd function
test_that("format.pipster_gd formats correctly", {
  v <- format(pipster_gd(c(0.12345, 0.67890)))
  expect_equal(v, c("0.123", "0.679"))
})

# Testing vec_ptype_abbr.pipster_gd function
test_that("vec_ptype_abbr.pipster_gd returns correct abbreviation", {
  expect_equal(vec_ptype_abbr.pipster_gd(pipster_gd()), "pipgd")
})

# Testing vector casting functions
test_that("vector casting functions work correctly", {
  # pipster_gd to pipster_gd
  v_md <- vec_cast.pipster_gd.pipster_gd(pipster_gd(1:5), new_pipster_gd())
  expect_s3_class(v_md, "pipster_gd")

  v_double <- vec_cast.pipster_gd.double(pipster_gd(1:5), double())
  expect_s3_class(v_double, "pipster_gd")

  v_cast_double <- vec_cast.double.pipster_gd(1:5, new_pipster_gd())
  expect_failure(expect_s3_class((v_cast_double), "pipster_gd"))
})

# Testing arithmetic functions
test_that("arithmetic functions work correctly", {
  x <- pipster_gd(c(1, 2))
  y <- pipster_gd(c(3, 4))

  # Test addition
  expect_equal(vec_arith.pipster_gd("+", x, y), pipster_gd(c(4, 6)))

  # Test subtraction
  expect_equal(vec_arith.pipster_gd("-", x, y), pipster_gd(c(-2, -2)))

  # Test multiplication
  expect_equal(vec_arith.pipster_gd("*", x, y), pipster_gd(c(3, 8)))

  # Test division
  expect_equal(vec_arith.pipster_gd("/", x, y), pipster_gd(c(1/3, 2/4)))

  # Test with numeric
  z <- c(5, 6)
  expect_equal(vec_arith.pipster_gd("+", x, z), pipster_gd(c(6, 8)))

  # Test incompatible operations
  expect_error(vec_arith.pipster_gd("%", x, y))
})

# Testing vec_math.pipster_gd function
test_that("vec_math.pipster_gd handles mathematical operations", {
  v <- pipster_gd(c(1, 2, 3, 4))
  expect_equal(vec_math.pipster_gd("sum", v), sum(v))
  expect_equal(vec_math.pipster_gd("mean", v), mean(v))
})

# Testing default_warning_message function
test_that("default_warning_message generates correct message", {
  expect_warning(default_warning_message("test_generic", "non_pipster_gd"),
                 "test_generic does not know how to handle object of class")
})

# New tests ----
# Test combining two pipster_gd objects
test_that("combining two pipster_gd objects returns a pipster_gd", {
  x <- pipster_gd(c(1, 2))
  y <- pipster_gd(c(3, 4))
  combined <- vec_ptype2(x, y)
  expect_s3_class(combined, "pipster_gd")
})

# Test combining pipster_gd with double returns double
test_that("combining pipster_gd with double returns double", {
  x <- pipster_gd(c(1, 2))
  y <- c(3.5, 4.5) # double
  combined <- vec_ptype2(x, y)
  expect_type(combined, "double")
})

# Test combining double with pipster_gd returns double
test_that("combining double with pipster_gd returns double", {
  x <- c(3.5, 4.5) # double
  y <- pipster_gd(c(1, 2))
  combined <- vec_ptype2(x, y)
  expect_type(combined, "double")
})

# Test combining pipster_gd with integer returns integer
test_that("combining pipster_gd with integer returns integer", {
  x <- pipster_gd(c(1, 2))
  y <- c(3L, 4L) # integer
  combined <- vec_ptype2(x, y)
  expect_type(combined, "integer")
})

# Test combining integer with pipster_gd returns integer
test_that("combining integer with pipster_gd returns integer", {
  x <- c(3L, 4L) # integer
  y <- pipster_gd(c(1, 2))
  combined <- vec_ptype2(x, y)
  expect_type(combined, "integer")
})


test_that("unsupported arithmetic operations on pipster_gd throw an error", {
  x <- pipster_gd(c(1, 2))
  y <- pipster_gd(c(3, 4))

  # Test an unsupported operation, like modulo
  expect_error(vec_arith.pipster_gd.default(`%`, x, y),
               "operation is not supported for pipster_gd objects")
})


test_that("numeric operations with pipster_gd perform correctly", {
  x <- pipster_gd(c(2, 4))
  y <- pipster_gd(c(1, 2)) # For pipster_gd operations
  z <- 2 # Numeric scalar for simplified operations

  # Test pipster_gd to pipster_gd operations
  expect_equal(vec_arith.pipster_gd.pipster_gd("+", x, y), pipster_gd(c(3, 6)))
  expect_equal(vec_arith.pipster_gd.pipster_gd("-", x, y), pipster_gd(c(1, 2)))
  expect_equal(vec_arith.pipster_gd.pipster_gd("*", x, y), pipster_gd(c(2, 8)))
  expect_equal(vec_arith.pipster_gd.pipster_gd("/", x, y), pipster_gd(c(2, 2)))

  # Test pipster_gd to numeric operations
  expect_equal(vec_arith.pipster_gd.numeric("+", x, z), pipster_gd(c(4, 6)))
  expect_equal(vec_arith.pipster_gd.numeric("-", x, z), pipster_gd(c(0, 2)))
  expect_equal(vec_arith.pipster_gd.numeric("*", x, z), pipster_gd(c(4, 8)))
  expect_equal(vec_arith.pipster_gd.numeric("/", x, z), pipster_gd(c(1, 2)))

  # Test numeric to pipster_gd operations (ensure symmetry)
  expect_equal(vec_arith.numeric.pipster_gd("+", z, x), pipster_gd(c(4, 6)))
  expect_equal(vec_arith.numeric.pipster_gd("-", z, x), pipster_gd(c(-1, -2))) # Negate for reverse operation
  expect_equal(vec_arith.numeric.pipster_gd("*", z, x), pipster_gd(c(4, 8)))
  expect_equal(vec_arith.numeric.pipster_gd("/", z, x), pipster_gd(c(1, 0.5))) # Reciprocal for reverse operation
})

test_that("mathematical functions handle sum and mean calculations", {
  v <- pipster_gd(c(1, 2, 3, 4))

  # Ensure sum and mean calculations are covered
  expect_equal(vec_math.pipster_gd("sum", v), sum(c(1, 2, 3, 4)))
  expect_equal(vec_math.pipster_gd("mean", v), mean(c(1, 2, 3, 4)))

  # Test unsupported function falls back to base
  expect_error(vec_math.pipster_gd("median", v))
})

test_that("unsupported operations correctly throw errors", {
  x <- pipster_gd(c(1, 2))

  # Attempt an unsupported operation, which should trigger the stop_incompatible_op
  expect_error(vec_arith.pipster_gd.numeric("^", x, 2),
               "No default exist. Please check object class.")
  expect_error(vec_arith.numeric.pipster_gd("^", 2, x),
               "No default exist. Please check object class.")

  # Assuming '^' (exponentiation) is not implemented for pipster_gd,
})

test_that("unsupported operations correctly throw errors for vec_arith.numeric.pipster_gd", {
  x <- pipster_gd(c(1, 2))
  y <- 2 # or any numeric value

  # Attempt an unsupported operation, e.g., "^", which should trigger the stop_incompatible_op
  expect_error(vec_arith.numeric.pipster_gd("^", y, x),
               "operation not supported",
               fixed = TRUE)
})

