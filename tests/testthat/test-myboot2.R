test_that("myboot2 returns correct structure", {
  data(ddt, package = "MATH4753")

  obj <- myboot2(x = ddt$DDT, iter = 500, fun = mean)

  expect_type(obj, "list")
  expect_true(all(c("ci", "xstat", "x", "fun") %in% names(obj)))
  expect_length(obj$ci, 2)
  expect_length(obj$xstat, 500)
  expect_equal(obj$x, ddt$DDT)
})

test_that("myboot2 handles median statistic", {
  data(ddt, package = "MATH4753")

  obj <- myboot2(x = ddt$DDT, iter = 300, fun = median)

  expect_length(obj$xstat, 300)
})

test_that("myboot2 errors on invalid x", {
  expect_error(myboot2(x = "not numeric"))
  expect_error(myboot2(x = c(TRUE, FALSE)))
  expect_error(myboot2(x = c(1, 2, NA)))
})
