test_that("colnumbering builds a LaTeX numbering row", {
  expect_equal(
    colnumbering(2),
    "\\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} \\\\"
  )
  expect_type(colnumbering(1), "character")
})

test_that("colnumbering rejects non-positive-integer input", {
  expect_error(colnumbering(0), "positive integer")
  expect_error(colnumbering(-1), "positive integer")
  expect_error(colnumbering(2.5), "positive integer")
  expect_error(colnumbering(c(1, 2)), "positive integer")
})
