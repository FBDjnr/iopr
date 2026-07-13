test_that("gini_ineq matches the rank-based formula (unweighted)", {
  # For 1:5 the Gini index is 0.2666667
  expect_equal(gini_ineq(1:5), 0.2666667, tolerance = 1e-6)
  # Perfect equality gives 0
  expect_equal(gini_ineq(rep(5, 4)), 0)
})

test_that("gini_ineq drops missing values before computing", {
  expect_equal(gini_ineq(c(1:5, NA)), gini_ineq(1:5))
})

test_that("gini_ineq validates x and weight", {
  expect_error(gini_ineq("a"), "numeric vector")
  expect_error(gini_ineq(numeric(0)), "length 0")
  expect_error(gini_ineq(c(1, 2, 3), weight = c(1, 2)), "must match")
  expect_error(gini_ineq(c(1, 2, 3), weight = c(1, -1, 1)), "negative")
  expect_error(gini_ineq(c(0, 0, 0)), "all values are zero")
})

test_that("gini_ineq warns on negative values", {
  expect_warning(gini_ineq(c(-1, 2, 3)), "negative")
})

test_that("weighted gini_ineq works when reldist is available", {
  skip_if_not_installed("reldist")
  expect_type(gini_ineq(c(1, 2, 3, 4, 5), weight = c(1, 1, 2, 1, 1)), "double")
})
