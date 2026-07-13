#' Variance of the relative IOP with Bhattacharya decomposition
#'
#' Estimates the variance of the relative IOP and decomposes it into naive,
#' cluster, and stratum components following Bhattacharya (2007).
#'
#' @inheritParams iop_var
#' @returns A data frame with rows `total_iop`, `abs_iop`, `rel_iop` and
#'   columns `est`, `var`, `var.naive`, `var.stratum`, and `var.cluster`.
#' @keywords internal
#' @importFrom dplyr arrange mutate group_by summarise ungroup desc
#' @importFrom ggdist weighted_ecdf
iop_var_decompose <- function(x, y, stratum = NULL, cluster = NULL,
                              weight = NULL,
                              distribution = c("smoothed", "standardized")) {
  distribution <- match.arg(distribution)

  check_outcome(x, "x")
  check_outcome(y, "y")
  n <- length(x)
  if (length(y) != n) {
    abort_iopr("`y` must have the same length as `x` (", n, ").")
  }
  check_design(stratum, n, "stratum")
  check_design(cluster, n, "cluster")
  check_weight(weight, n, "weight")

  ones <- rep(1, n)
  if (is.null(stratum)) stratum <- ones
  if (is.null(cluster)) cluster <- ones
  if (is.null(weight)) weight <- ones

  gini_x <- gini_ineq(x, weight)
  gini_y <- gini_ineq(y, weight)

  w <- weight / sum(weight)
  mu_x <- sum(w * x)
  mu_y <- sum(w * y)

  dt <- data.frame(stratum, cluster, x, w, y) |>
    dplyr::arrange(stratum, cluster, x) |>
    dplyr::mutate(
      Fx = {
        ggdist::weighted_ecdf(x, w)
      }(x),
      q11x = Fx - 0.5 * (gini_x + 1)
    ) |>
    dplyr::mutate(
      Fy = {
        ggdist::weighted_ecdf(y, w)
      }(y),
      q11y = Fy - 0.5 * (gini_y + 1)
    ) |>
    dplyr::arrange(dplyr::desc(Fx)) |>
    dplyr::mutate(q12x = cumsum(w * x)) |>
    dplyr::arrange(stratum, cluster, y) |>
    dplyr::arrange(dplyr::desc(Fy)) |>
    dplyr::mutate(q12y = cumsum(w * y)) |>
    dplyr::mutate(
      mtempx = (2 / mu_x) * (q11x * x + q12x - (0.5 * mu_x * (gini_x + 1))),
      mtempy = (2 / mu_y) * (q11y * y + q12y - (0.5 * mu_y * (gini_y + 1)))
    ) |>
    dplyr::mutate(
      ztempx = w * mtempx,
      ztempy = w * mtempy
    )

  dt_naive <- dt |>
    dplyr::summarise(
      naive_x = sum(ztempx^2),
      naive_y = sum(ztempy^2),
      naive_xy = sum(ztempx * ztempy)
    )

  dt_cluster <- dt |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(
      t1x = sum(ztempx)^2 - sum(ztempx^2),
      t1y = sum(ztempy)^2 - sum(ztempy^2),
      t1xy = sum(ztempx) * sum(ztempy) - sum(ztempx * ztempy),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      cluster_x = sum(t1x),
      cluster_y = sum(t1y),
      cluster_xy = sum(t1xy),
      .groups = "drop"
    )

  dt_SUMWS <- dt |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(
      n_s = count_unique(cluster),
      sumsx = sum(ztempx),
      sumsy = sum(ztempy),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      sumws_x = sum(sumsx^2 / n_s),
      sumws_y = sum(sumsy^2 / n_s),
      sumws_xy = sum(sumsx * sumsy / n_s),
      .groups = "drop"
    )

  varcov <- dt_naive + dt_cluster - dt_SUMWS
  colnames(varcov) <- c("est_x", "est_y", "est_xy")

  total_iop <- gini_x

  if (distribution == "standardized") {
    abs_iop <- total_iop - gini_y
  } else {
    abs_iop <- gini_y
  }

  rel_iop <- abs_iop / total_iop

  var_ratio <- compute_delta_variance(
    gini_y, gini_x,
    varcov$est_y, varcov$est_x, varcov$est_xy
  )

  var_ratio_naive <- compute_delta_variance(
    gini_y, gini_x,
    dt_naive$naive_y, dt_naive$naive_x, dt_naive$naive_xy
  )
  var_ratio_stratum <- compute_delta_variance(
    gini_y, gini_x,
    -dt_SUMWS$sumws_y, -dt_SUMWS$sumws_x, -dt_SUMWS$sumws_xy
  )
  var_ratio_cluster <- compute_delta_variance(
    gini_y, gini_x,
    dt_cluster$cluster_y, dt_cluster$cluster_x, dt_cluster$cluster_xy
  )

  ans <- data.frame(
    est = c(total_iop, abs_iop, rel_iop),
    var = c(varcov$est_x, varcov$est_y, var_ratio),
    var.naive = c(dt_naive$naive_x, dt_naive$naive_y, var_ratio_naive),
    var.stratum = c(-dt_SUMWS$sumws_x, -dt_SUMWS$sumws_y, var_ratio_stratum),
    var.cluster = c(
      dt_cluster$cluster_x, dt_cluster$cluster_y, var_ratio_cluster
    )
  )
  rownames(ans) <- c("total_iop", "abs_iop", "rel_iop")

  return(ans)
}
