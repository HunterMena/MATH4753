test_that("myncurve returns correct output structure and values", {
  # Run the function
  result <- myncurve(mu = 10, sigma = 5, a = 6)

  # Check that it returns a list
  expect_type(result, "list")

  # Check that it includes the right named components
  expect_named(result, c("mu", "sigma", "area"))

  # Check that the returned mu and sigma match the inputs
  expect_equal(result$mu, 10)
  expect_equal(result$sigma, 5)

  # Check that the area (probability) is correct for P(X ≤ a)
  expected_area <- pnorm(6, mean = 10, sd = 5)
  expect_equal(result$area, expected_area, tolerance = 1e-4)
})
