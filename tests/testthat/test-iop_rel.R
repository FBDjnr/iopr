# Integration-style tests that exercise the survey machinery. These require the
# tidyverse/ggdist/reldist stack, so they skip gracefully when those are absent.

make_toy_data <- function() {
  set.seed(1)
  n <- 200
  data.frame(
    income = round(exp(rnorm(n, log(1000), 0.4))),
    region = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    stratum = rep(1:4, each = n / 4),
    cluster = rep(1:20, each = n / 20)
  )
}

test_that("iop_rel returns the three IOP components without variance", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("reldist")
  skip_if_not_installed("janitor")

  d <- make_toy_data()
  res <- iop_rel(
    x = d$income, circumstances = d$region,
    distribution = "smoothed"
  )
  expect_named(res, c("total_iop", "abs_iop", "rel_iop"))
  expect_true(res$rel_iop >= 0 && res$rel_iop <= 1)
})

test_that("iop_rel validates required arguments", {
  d <- make_toy_data()
  expect_error(
    iop_rel(x = d$income),
    "circumstances"
  )
  expect_error(
    iop_rel(x = c(-1, 2, 3), circumstances = c(1, 1, 2)),
    "strictly positive"
  )
})
