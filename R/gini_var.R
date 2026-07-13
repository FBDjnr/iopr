#' Variance of the Gini index for a complex survey
#'
#' Estimates the variance of the Gini index using the influence function
#' approach, accommodating stratified cluster sampling.
#'
#' @param x A numeric vector of outcome values. If `data` is supplied, a bare
#'   column name or a character column name may be given instead.
#' @param stratum Stratum ID vector. Defaults to a single stratum if `NULL`.
#' @param cluster Cluster ID vector. Defaults to individual observations if
#'   `NULL`.
#' @param weight Sampling weight vector. Defaults to equal weights if `NULL`.
#' @param data Optional data frame. If provided, `x`, `stratum`, `cluster`, and
#'   `weight` are evaluated in the context of `data`.
#' @returns A named list with elements:
#'   - `est`: Gini index estimate.
#'   - `var`: Estimated variance of the Gini index.
#' @export
#' @importFrom dplyr mutate group_by summarise pull n
#' @importFrom ggdist weighted_ecdf
gini_var <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
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

  gini_hat <- gini_ineq(x, weight)

  # Normalize weights
  w <- weight / sum(weight)

  # Weighted mean
  mu <- sum(w * x)

  dt <- data.frame(stratum, cluster, x, w) |>
    dplyr::mutate(Fx = {
      ggdist::weighted_ecdf(x, w)
    }(x)) |>
    dplyr::mutate(B = my_revcumsum(x, w * x)) |>
    dplyr::mutate(
      u_sch = w * (2 / mu) * (x * Fx + B - 0.5 * (mu + x) * (gini_hat + 1))
    )

  gini_variance <- dt |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(u_sc = sum(u_sch), .groups = "drop") |>
    dplyr::group_by(stratum) |>
    dplyr::summarise(
      n_s = dplyr::n(),
      nvar = sum((u_sc - mean(u_sc))^2),
      .groups = "drop"
    ) |>
    dplyr::summarise(est = sum(nvar)) |>
    dplyr::pull()

  ans <- list(est = gini_hat, var = gini_variance)
  return(ans)
}
