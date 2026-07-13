#' Compute optimal sample size for a hypothesis test
#'
#' Calculates the minimum sample size needed to detect an effect of size
#' `(H1 - H0) / sd` at significance level `alpha` with power `1 - beta`,
#' using a two-sided z-test.
#'
#' @param alpha Significance level (Type I error rate). A number in `(0, 1)`.
#' @param beta Type II error rate, so power is `1 - beta`. A number in `(0, 1)`.
#' @param H0 Null hypothesis value.
#' @param H1 Alternative hypothesis value. Must differ from `H0`.
#' @param sd Standard deviation of the test statistic. Must be positive.
#' @returns A numeric scalar: the required sample size.
#' @export
#' @importFrom stats qnorm
#' @examples
#' n_opt(alpha = 0.05, beta = 0.20, H0 = 0, H1 = 0.5, sd = 1)
n_opt <- function(alpha, beta, H0, H1, sd) {
  check_prob(alpha, "alpha")
  check_prob(beta, "beta")
  check_number(H0, "H0")
  check_number(H1, "H1")
  check_number(sd, "sd")

  if (sd <= 0) {
    abort_iopr("`sd` must be a positive number.")
  }
  if (H1 == H0) {
    abort_iopr("`H1` must differ from `H0`; otherwise the effect size is zero.")
  }

  delta <- (H1 - H0) / sd
  ans <- ((stats::qnorm(alpha / 2) + stats::qnorm(1 - beta)) / delta)^2
  return(ans)
}
