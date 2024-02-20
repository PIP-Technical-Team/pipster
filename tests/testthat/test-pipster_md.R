
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


