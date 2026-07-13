test_that("count_unique counts distinct values", {
  expect_equal(count_unique(c(1, 2, 2, 3)), 3L)
  expect_equal(count_unique(c("a", "a", "b")), 2L)
  expect_equal(count_unique(integer(0)), 0L)
})

test_that("count_unique rejects invalid input", {
  expect_error(count_unique(), "must be supplied")
  expect_error(count_unique(NULL), "must be supplied")
  expect_error(count_unique(list(1, 2)), "atomic vector")
})
