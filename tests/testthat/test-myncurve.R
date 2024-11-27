test_that("mu is correct", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$mu, 10)  # We expect the mu to be 10
})

# Test 2: Check if 'sigma' is correct
test_that("sigma is correct", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expect_equal(result$sigma, 2)  # We expect sigma to be 2
})

# Test 3: Check if 'P_X_less_than_a' (probability) is correct
test_that("P(X <= a) is correct", {
  result <- myncurve(mu = 10, sigma = 2, a = 12)
  expected_prob <- pnorm(12, mean = 10, sd = 2)  # Use pnorm to calculate expected probability
  expect_equal(result$P_X_less_than_a, expected_prob, tolerance = 1e-4)  # Check if the probability matches
})
