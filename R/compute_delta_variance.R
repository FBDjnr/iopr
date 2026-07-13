#' Compute the delta-method variance of a ratio
#'
#' Approximates the variance of the ratio `num_est / denom_est` using the
#' delta method.
#'
#' @param num_est Estimate of the numerator. Must be non-zero.
#' @param denom_est Estimate of the denominator. Must be non-zero.
#' @param num_var Variance of the numerator estimator.
#' @param denom_var Variance of the denominator estimator.
#' @param cov_both Covariance between the numerator and denominator estimators.
#' @returns A numeric scalar: the approximate variance of the ratio.
#' @export
#' @examples
#' compute_delta_variance(
#'   num_est = 0.3, denom_est = 0.5,
#'   num_var = 0.001, denom_var = 0.002, cov_both = 0.0005
#' )
compute_delta_variance <- function(num_est, denom_est, num_var, denom_var,
                                   cov_both) {
  check_number(num_est, "num_est")
  check_number(denom_est, "denom_est")
  check_number(num_var, "num_var")
  check_number(denom_var, "denom_var")
  check_number(cov_both, "cov_both")

  if (denom_est == 0) {
    abort_iopr("`denom_est` (denominator estimate) cannot be zero.")
  }
  if (num_est == 0) {
    abort_iopr("`num_est` (numerator estimate) cannot be zero.")
  }

  var_ratio <- (num_est / denom_est)^2 *
    (num_var / num_est^2 + denom_var / denom_est^2 -
      2 * cov_both / (num_est * denom_est))

  return(var_ratio)
}
