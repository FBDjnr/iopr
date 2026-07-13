#' Variance of the relative Inequality of Opportunity
#'
#' Estimates the variance of the relative IOP (and its components) using the
#' influence function approach for stratified cluster samples.
#'
#' @param x A numeric vector of observed outcome values.
#' @param y A numeric vector of the hypothesized "fair" outcome values (either
#'   standardized or smoothed, matching the `distribution` argument of
#'   [iop_rel()]). Must be the same length as `x`.
#' @param stratum Stratum ID vector. Defaults to a single stratum if `NULL`.
#' @param cluster Cluster ID vector. Defaults to individual observations if
#'   `NULL`.
#' @param weight Sampling weight vector. Defaults to equal weights if `NULL`.
#' @param distribution Character; `"smoothed"` or `"standardized"`. Controls
#'   whether the absolute IOP is `G(y)` (smoothed) or `G(x) - G(y)`
#'   (standardized).
#' @returns A data frame with rows `total_iop`, `abs_iop`, `rel_iop` and
#'   columns `est` (point estimate) and `var` (variance estimate).
#' @keywords internal
#' @importFrom dplyr arrange mutate group_by summarise desc n
#' @importFrom ggdist weighted_ecdf
iop_var <- function(x, y, stratum = NULL, cluster = NULL, weight = NULL,
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
      Fy = {
        ggdist::weighted_ecdf(y, w)
      }(y)
    ) |>
    dplyr::arrange(dplyr::desc(Fx)) |>
    dplyr::mutate(
      u_sch_x = w * (2 / mu_x) *
        (x * Fx + cumsum(w * x) - 0.5 * (mu_x + x) * (gini_x + 1))
    ) |>
    dplyr::arrange(stratum, cluster, y) |>
    dplyr::arrange(dplyr::desc(Fy)) |>
    dplyr::mutate(
      u_sch_y = w * (2 / mu_y) *
        (y * Fy + cumsum(w * y) - 0.5 * (mu_y + y) * (gini_y + 1))
    )

  varcov <- dt |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(
      u_sc_x = sum(u_sch_x),
      u_sc_y = sum(u_sch_y),
      .groups = "drop"
    ) |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(
      n_s = dplyr::n(),
      nvar_x = sum((u_sc_x - mean(u_sc_x))^2),
      nvar_y = sum((u_sc_y - mean(u_sc_y))^2),
      nvar_xy = sum((u_sc_x - mean(u_sc_x)) * (u_sc_y - mean(u_sc_y))),
      .groups = "drop"
    ) |>
    dplyr::summarise(
      est_x = sum(nvar_x),
      est_y = sum(nvar_y),
      est_xy = sum(nvar_xy)
    )

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

  ans <- data.frame(
    est = c(total_iop, abs_iop, rel_iop),
    var = c(varcov$est_x, varcov$est_y, var_ratio)
  )
  rownames(ans) <- c("total_iop", "abs_iop", "rel_iop")

  return(ans)
}
