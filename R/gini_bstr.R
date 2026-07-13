#' Bootstrap variance of the Gini index
#'
#' Estimates the variance of the Gini index via stratified cluster bootstrap
#' resampling. Observations with missing values are dropped before resampling.
#'
#' @inheritParams gini_var
#' @param nboot Number of bootstrap replicates. A positive integer; default is
#'   `1000`.
#' @param parallel Logical; use parallel computation? Default is `FALSE`.
#' @param no_cores Number of cores to use when `parallel = TRUE`. Defaults to
#'   all available cores minus 2.
#' @returns A named list with elements:
#'   - `est`: Gini index estimate on the original sample.
#'   - `var`: Bootstrap variance estimate.
#'   - `thetastar`: Numeric vector of bootstrap Gini estimates.
#' @export
#' @importFrom foreach foreach registerDoSEQ
#' @importFrom future availableCores
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
gini_bstr <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                      data = NULL, nboot = 1000,
                      parallel = FALSE, no_cores = NULL) {
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
  check_count(nboot, "nboot")
  check_flag(parallel, "parallel")
  if (!is.null(no_cores)) check_count(no_cores, "no_cores")

  ones <- rep(1, n)
  if (is.null(stratum)) stratum <- ones
  if (is.null(cluster)) cluster <- ones
  if (is.null(weight)) weight <- ones

  # Remove missing values
  na_indices <- is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight)
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]

  if (length(x) == 0L) {
    abort_iopr("No complete observations remain after removing missing values.")
  }

  orig_data <- data.frame(stratum, cluster, x, weight)

  theta <- gini_ineq(orig_data$x, orig_data$weight)

  if (parallel) {
    all_cores <- future::availableCores()
    if (is.null(no_cores)) no_cores <- all_cores
    use_cores <- min(no_cores, max(all_cores - 2, 1))
    cl <- parallel::makeCluster(use_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else {
    foreach::registerDoSEQ()
  }

  thetastar <- foreach::foreach(
    i = 1:nboot,
    .combine = "c",
    .export = c("gini_ineq", "bootstrap_sample")
  ) %dopar% {
    boot_sample <- bootstrap_sample(stratum, cluster, orig_data)
    gini_ineq(boot_sample$x, boot_sample$weight)
  }

  theta.var <- sum((thetastar - theta)^2) / nboot

  ans <- list(est = theta, var = theta.var, thetastar = thetastar)
  return(ans)
}
