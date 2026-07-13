# Internal helper: relative IOP point estimates only (no variance).
#
# @param x Numeric vector of positive outcome values.
# @param weight Numeric weight vector.
# @param circumstances Data frame (or coercible) of circumstance variables.
# @param distribution "smoothed" or "standardized".
# @returns A list with `total_iop`, `abs_iop`, and `rel_iop`.
# @keywords internal
# @noRd
#' @importFrom dplyr summarise across
#' @importFrom tidyselect where
#' @importFrom stats lm predict
iop_rel_est <- function(x, weight, circumstances,
                        distribution = c("smoothed", "standardized")) {
  distribution <- match.arg(distribution)
  check_outcome(x, "x")
  check_positive(x, "x")
  circumstances <- as.data.frame(circumstances)

  total_iop <- gini_ineq(x, weight = weight)

  lm_dt <- as.data.frame(cbind(x, circumstances))
  lm_mod <- stats::lm(log(x) ~ ., lm_dt)

  if (distribution == "standardized") {
    e_hat <- lm_mod$residuals

    circumstances_mean <- circumstances |>
      dplyr::summarise(
        dplyr::across(
          where(is.factor), \(v) names(sort(table(v), decreasing = TRUE))[1]
        ),
        dplyr::across(where(is.numeric), \(v) mean(v, na.rm = TRUE))
      ) |>
      data.frame()

    x_hat <- exp(stats::predict(lm_mod, newdata = circumstances_mean) + e_hat)
    iop_xhat <- gini_ineq(x_hat, weight = weight)
    abs_iop <- total_iop - iop_xhat
  } else {
    mu_hat <- exp(lm_mod$fitted.values)
    abs_iop <- gini_ineq(mu_hat, weight = weight)
  }

  rel_iop <- abs_iop / total_iop

  ans <- list(total_iop = total_iop, abs_iop = abs_iop, rel_iop = rel_iop)
  return(ans)
}
