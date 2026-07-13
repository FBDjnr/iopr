test_that("compute_delta_variance matches the delta-method formula", {
  num_est <- 0.3
  denom_est <- 0.5
  num_var <- 0.001
  denom_var <- 0.002
  cov_both <- 0.0005
  expected <- (num_est / denom_est)^2 *
    (num_var / num_est^2 + denom_var / denom_est^2 -
      2 * cov_both / (num_est * denom_est))
  expect_equal(
    compute_delta_variance(num_est, denom_est, num_var, denom_var, cov_both),
    expected
  )
})

test_that("compute_delta_variance guards against zero and bad estimates", {
  expect_error(
    compute_delta_variance(0.3, 0, 0.001, 0.002, 0.0005),
    "denominator"
  )
  expect_error(
    compute_delta_variance(0, 0.5, 0.001, 0.002, 0.0005),
    "numerator"
  )
  expect_error(
    compute_delta_variance("a", 0.5, 0.001, 0.002, 0.0005),
    "single finite number"
  )
})
