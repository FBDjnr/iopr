test_that("iop_ablation_study returns one row per circumstance with expected columns", {
  round1 <- subset(synthetic_survey, round == "round_1")
  circ <- c("social_group", "religion", "land_category", "hh_type")

  res <- iop_ablation_study(
    x = expenditure, stratum = stratum, cluster = cluster, weight = weight,
    circumstances = circ, data = round1
  )

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), length(circ))
  expect_setequal(res$circumstance, circ)
  expect_named(
    res,
    c("circumstance", "iop_full", "iop_reduced", "contribution",
      "pc_contribution")
  )
  # Removing a circumstance cannot increase relative IOP
  expect_true(all(res$contribution >= -1e-8))
  expect_true(all(is.finite(res$pc_contribution)))
})


test_that("iop_ablation_study requires at least two circumstances", {
  round1 <- subset(synthetic_survey, round == "round_1")
  expect_error(
    iop_ablation_study(
      x = expenditure, circumstances = "social_group", data = round1
    ),
    "at least two"
  )
})


test_that("iop_ablation_test returns valid p-values and is reproducible", {
  round1 <- subset(synthetic_survey, round == "round_1")
  round2 <- subset(synthetic_survey, round == "round_2")
  circ <- c("social_group", "religion", "land_category", "hh_type")

  run <- function() {
    iop_ablation_test(
      data1 = round1, data2 = round2,
      x = "expenditure", cluster = "cluster", stratum = "stratum",
      weight = "weight", circumstances = circ, n_boot = 25, seed = 42
    )
  }

  res <- run()
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), length(circ))
  expect_named(
    res,
    c("circumstance", "pc_1", "pc_2", "diff", "se", "p_value", "significance")
  )
  expect_true(all(res$p_value >= 0 & res$p_value <= 1))
  expect_equal(res$diff, res$pc_2 - res$pc_1)

  # Same seed => identical result
  expect_equal(run(), res)
})
