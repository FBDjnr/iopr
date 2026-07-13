#' Variance of the Gini index using the Bhattacharya decomposition
#'
#' Estimates the variance of the Gini index and decomposes it into naive,
#' cluster, and stratum components following Bhattacharya (2007).
#'
#' @inheritParams gini_var
#' @returns A named list with elements:
#'   - `est`: Gini index estimate.
#'   - `var`: Total estimated variance.
#'   - `var.decompose`: Named numeric vector with components `naive`, `cluster`,
#'     and `stratum`.
#' @export
#' @importFrom dplyr mutate arrange group_by summarise ungroup desc
#' @importFrom ggdist weighted_ecdf
gini_var_bhatt <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                           data = NULL) {
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      abort_iopr("`data` must be a data frame or NULL.")
    }
    pf <- parent.frame()
    x <- resolve_column(substitute(x), x, data, pf)
    weight <- resolve_column(substitute(weight), weight, data, pf)
    stratum <- resolve_column(substitute(stratum), stratum, data, pf)
    cluster <- resolve_column(substitute(cluster), cluster, data, pf)
  }

  check_outcome(x, "x")
  warn_if_negative(x, "x")
  n <- length(x)
  check_design(stratum, n, "stratum")
  check_design(cluster, n, "cluster")
  check_weight(weight, n, "weight")

  ones <- rep(1, n)
  if (is.null(stratum)) stratum <- ones
  if (is.null(cluster)) cluster <- ones
  if (is.null(weight)) weight <- ones

  # Normalize weights
  w <- weight / sum(weight)

  gini_hat <- gini_ineq(x, weight)

  # Weighted mean
  mu <- sum(x * w)

  dt <- data.frame(x, stratum, cluster, w)

  dt1 <- dt |>
    dplyr::arrange(stratum, cluster, x) |>
    dplyr::mutate(
      Fx = {
        ggdist::weighted_ecdf(x, w)
      }(x),
      q11 = Fx - 0.5 * (gini_hat + 1)
    ) |>
    dplyr::arrange(dplyr::desc(Fx)) |>
    dplyr::mutate(q12 = cumsum(w * x)) |>
    dplyr::mutate(
      mtemp = (2 / mu) * (q11 * x + q12 - (0.5 * mu * (gini_hat + 1)))
    ) |>
    dplyr::mutate(ztemp = w * mtemp)

  # Naive variance
  dt_naive <- dt1 |>
    dplyr::summarise(naive = sum(ztemp^2))

  # Cluster effect
  dt_cluster <- dt1 |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(
      t1 = sum(ztemp)^2 - sum(ztemp^2),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(cluster = sum(t1))

  # Stratum effect
  dt_SUMWS <- dt1 |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(
      n_s = count_unique(cluster),
      sums = sum(ztemp),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(sumws = sum(sums^2 / n_s))

  s1 <- dt_naive$naive
  s2 <- dt_cluster$cluster
  s3 <- -dt_SUMWS$sumws

  gini_variance <- s1 + s2 + s3

  ans <- list(
    est = gini_hat,
    var = gini_variance,
    var.decompose = c(naive = s1, cluster = s2, stratum = s3)
  )

  return(ans)
}
