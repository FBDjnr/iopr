test_that("n_opt matches the two-sided z-test formula", {
  delta <- (0.5 - 0) / 1
  expected <- ((qnorm(0.05 / 2) + qnorm(1 - 0.20)) / delta)^2
  expect_equal(n_opt(alpha = 0.05, beta = 0.20, H0 = 0, H1 = 0.5, sd = 1),
    expected)
})

test_that("n_opt validates its arguments", {
  expect_error(n_opt(alpha = 0, beta = 0.2, H0 = 0, H1 = 1, sd = 1),
    "between 0 and 1")
  expect_error(n_opt(alpha = 1.2, beta = 0.2, H0 = 0, H1 = 1, sd = 1),
    "between 0 and 1")
  expect_error(n_opt(alpha = 0.05, beta = 0.2, H0 = 0, H1 = 1, sd = 0),
    "positive")
  expect_error(n_opt(alpha = 0.05, beta = 0.2, H0 = 1, H1 = 1, sd = 1),
    "must differ")
})
