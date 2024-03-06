# Old tests -----
# Testing new_pipster_md and pipster_md functions
test_that("new_pipster_md and pipster_md work correctly", {
  # Test for normal input
  v <- pipster_md(c(0.1, 0.5, 0.9, 1))
  expect_s3_class(v, "pipster_md")
  expect_equal(v |> unclass(), c(0.1, 0.5, 0.9, 1))

  # Test for edge cases like empty vector and NA
  v_empty <- pipster_md(numeric(0))
  expect_s3_class(v_empty, "pipster_md")
  expect_equal(v_empty |> unclass(), numeric(0))

  v_na <- pipster_md(NA)
  expect_s3_class(v_na, "pipster_md")
  expect_true(all(is.na(v_na)))
})

# Testing is_pipster_md function
test_that("is_pipster_md identifies pipster_md objects correctly", {
  expect_true(is_pipster_md(pipster_md(1:5)))
  expect_false(is_pipster_md(1:5))
  expect_false(is_pipster_md("not a pipster_md"))
})

# Testing as_pipster_md function
test_that("as_pipster_md casts correctly", {
  v <- as_pipster_md(as.double(c(1:5)))
  expect_s3_class(v, "pipster_md")

  # Testing with invalid input
  expect_error(as_pipster_md("invalid"))
})

# Testing format.pipster_md function
test_that("format.pipster_md formats correctly", {
  v <- format(pipster_md(c(0.12345, 0.67890)))
  expect_equal(v, c("0.123", "0.679"))
})

# Testing vec_ptype_abbr.pipster_md function
test_that("vec_ptype_abbr.pipster_md returns correct abbreviation", {
  expect_equal(vec_ptype_abbr.pipster_md(pipster_md()), "pipmd")
})

# Testing vector casting functions
test_that("vector casting functions work correctly", {
  # pipster_md to pipster_md
  v_md <- vec_cast.pipster_md.pipster_md(pipster_md(1:5), new_pipster_md())
  expect_s3_class(v_md, "pipster_md")

  v_double <- vec_cast.pipster_md.double(pipster_md(1:5), double())
  expect_s3_class(v_double, "pipster_md")

  v_cast_double <- vec_cast.double.pipster_md(1:5, new_pipster_md())
  expect_failure(expect_s3_class((v_cast_double), "pipster_md"))
})

# Testing arithmetic functions
test_that("arithmetic functions work correctly", {
  x <- pipster_md(c(1, 2))
  y <- pipster_md(c(3, 4))

  # Test addition
  expect_equal(vec_arith.pipster_md("+", x, y), pipster_md(c(4, 6)))

  # Test subtraction
  expect_equal(vec_arith.pipster_md("-", x, y), pipster_md(c(-2, -2)))

  # Test multiplication
  expect_equal(vec_arith.pipster_md("*", x, y), pipster_md(c(3, 8)))

  # Test division
  expect_equal(vec_arith.pipster_md("/", x, y), pipster_md(c(1/3, 2/4)))

  # Test with numeric
  z <- c(5, 6)
  expect_equal(vec_arith.pipster_md("+", x, z), pipster_md(c(6, 8)))

  # Test incompatible operations
  expect_error(vec_arith.pipster_md("%", x, y))
})

# Testing vec_math.pipster_md function
test_that("vec_math.pipster_md handles mathematical operations", {
  v <- pipster_md(c(1, 2, 3, 4))
  expect_equal(vec_math.pipster_md("sum", v), sum(v))
  expect_equal(vec_math.pipster_md("mean", v), mean(v))
})

# Testing default_warning_message function
test_that("default_warning_message generates correct message", {
  expect_warning(default_warning_message("test_generic", "non_pipster_md"),
                 "test_generic does not know how to handle object of class")
})


# New tests -------
# Test combining two pipster_md objects
test_that("combining two pipster_md objects returns a pipster_md", {
  x <- pipster_md(c(1, 2))
  y <- pipster_md(c(3, 4))
  combined <- vec_ptype2(x, y)
  expect_s3_class(combined, "pipster_md")
})

# Test combining pipster_md with double returns double
test_that("combining pipster_md with double returns double", {
  x <- pipster_md(c(1, 2))
  y <- c(3.5, 4.5) # double
  combined <- vec_ptype2(x, y)
  expect_type(combined, "double")
})

# Test combining double with pipster_md returns double
test_that("combining double with pipster_md returns double", {
  x <- c(3.5, 4.5) # double
  y <- pipster_md(c(1, 2))
  combined <- vec_ptype2(x, y)
  expect_type(combined, "double")
})

# Test combining pipster_md with integer returns integer
test_that("combining pipster_md with integer returns integer", {
  x <- pipster_md(c(1, 2))
  y <- c(3L, 4L) # integer
  combined <- vec_ptype2(x, y)
  expect_type(combined, "integer")
})

# Test combining integer with pipster_md returns integer
test_that("combining integer with pipster_md returns integer", {
  x <- c(3L, 4L) # integer
  y <- pipster_md(c(1, 2))
  combined <- vec_ptype2(x, y)
  expect_type(combined, "integer")
})


test_that("unsupported arithmetic operations on pipster_md throw an error", {
  x <- pipster_md(c(1, 2))
  y <- pipster_md(c(3, 4))

  # Test an unsupported operation, like modulo
  expect_error(vec_arith.pipster_md.default("^", x, y))
})


test_that("numeric operations with pipster_md perform correctly", {
  x <- pipster_md(c(2, 4))
  y <- pipster_md(c(1, 2)) # For pipster_md operations
  z <- 2 # Numeric scalar for simplified operations

  # Test pipster_md to pipster_md operations
  expect_equal(vec_arith.pipster_md.pipster_md("+", x, y), pipster_md(c(3, 6)))
  expect_equal(vec_arith.pipster_md.pipster_md("-", x, y), pipster_md(c(1, 2)))
  expect_equal(vec_arith.pipster_md.pipster_md("*", x, y), pipster_md(c(2, 8)))
  expect_equal(vec_arith.pipster_md.pipster_md("/", x, y), pipster_md(c(2, 2)))

  # Test pipster_md to numeric operations
  expect_equal(vec_arith.pipster_md.numeric("+", x, z), pipster_md(c(4, 6)))
  expect_equal(vec_arith.pipster_md.numeric("-", x, z), pipster_md(c(0, 2)))
  expect_equal(vec_arith.pipster_md.numeric("*", x, z), pipster_md(c(4, 8)))
  expect_equal(vec_arith.pipster_md.numeric("/", x, z), pipster_md(c(1, 2)))

  # Test numeric to pipster_md operations
  expect_equal(vec_arith.numeric.pipster_md("+", z, x), pipster_md(c(4, 6)))
  expect_equal(vec_arith.numeric.pipster_md("-", z, x), pipster_md(c(0, -2)))
  expect_equal(vec_arith.numeric.pipster_md("*", z, x), pipster_md(c(4, 8)))
  expect_equal(vec_arith.numeric.pipster_md("/", z, x), pipster_md(c(1, 0.5))) # Reciprocal for reverse operation
})

# !!! This test fails, but I do not understand why?
test_that("mathematical functions handle sum and mean calculations", {
  v <- pipster_md(c(1, 2, 3, 4))

  # Ensure sum and mean calculations are covered
  expect_equal(vec_math.pipster_md("sum", v), sum(c(1, 2, 3, 4)))
  expect_equal(vec_math.pipster_md("mean", v), mean(c(1, 2, 3, 4)))

  # Test unsupported function falls back to base
  expect_error(vec_math.pipster_md("median", v))
})


test_that("unsupported operations correctly throw errors", {
  x <- pipster_md(c(1, 2))

  # Attempt an unsupported operation, which should trigger the stop_incompatible_op
  expect_error(vec_arith.pipster_md.numeric("^", x, 2))
  expect_error(vec_arith.numeric.pipster_md("^", 2, x))

})

test_that("unsupported operations correctly throw errors for vec_arith.numeric.pipster_md", {
  x <- pipster_md(c(1, 2))
  y <- 2 # or any numeric value

  # Attempt an unsupported operation
  expect_error(vec_arith.numeric.pipster_md("^", y, x))
})
