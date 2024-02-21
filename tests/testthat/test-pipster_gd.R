
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

