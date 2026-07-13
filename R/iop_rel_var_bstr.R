#' Bootstrap variance of relative Inequality of Opportunity
#'
#' Estimates the bootstrap variance of the relative IOP via stratified cluster
#' resampling.
#'
#' @param x A numeric vector of strictly positive outcome values.
#' @param stratum Stratum ID vector. Defaults to a single stratum if `NULL`.
#' @param cluster Cluster ID vector. Defaults to individual observations if
#'   `NULL`.
#' @param weight Sampling weight vector. Defaults to equal weights if `NULL`.
#' @param circumstances A vector, factor, or data frame of circumstance
#'   variables. If `data` is provided, may instead be a character vector of
#'   column names.
#' @param data Optional data frame.
#' @param distribution `"smoothed"` (default) or `"standardized"`.
#' @param nboot Number of bootstrap replicates. A positive integer; default is
#'   `1000`.
#' @param parallel Logical; use parallel computation? Default is `FALSE`.
#' @param no_cores Number of cores to use when `parallel = TRUE`.
#' @returns A named list with elements:
#'   - `est`: Relative IOP estimate on the original sample.
#'   - `var`: Bootstrap variance estimate.
#'   - `theta.cov`: Bootstrap covariance between numerator and denominator.
#'   - `thetastar`: Matrix of bootstrap IOP estimates.
#' @export
#' @importFrom dplyr select group_by mutate ungroup left_join join_by summarise across
#' @importFrom tidyselect where
#' @importFrom tidyr nest unnest
#' @importFrom purrr map2
#' @importFrom stats lm predict
#' @importFrom foreach foreach registerDoSEQ
#' @importFrom future availableCores
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
iop_rel_var_bstr <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                             circumstances, data = NULL,
                             distribution = c("smoothed", "standardized"),
                             nboot = 1000, parallel = FALSE,
                             no_cores = NULL) {
  distribution <- match.arg(distribution)

  if (missing(circumstances) || is.null(circumstances)) {
    abort_iopr(
      "`circumstances` must be supplied: a vector, factor, or data frame the ",
      "same length/number of rows as `x`, or (with `data`) a character vector ",
      "of column names."
    )
  }

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

  # Convert circumstances to a data frame
  if (is.vector(circumstances) || is.factor(circumstances)) {
    if (length(circumstances) == n) {
      circumstances <- as.data.frame(circumstances)
    } else if (!is.null(data) && is.character(circumstances)) {
      missing_cols <- setdiff(circumstances, names(data))
      if (length(missing_cols)) {
        abort_iopr(
          "Column(s) not found in `data`: ",
          paste(missing_cols, collapse = ", "), "."
        )
      }
      circumstances <- dplyr::select(data, dplyr::all_of(circumstances))
    } else {
      abort_iopr(
        "`circumstances` must have the same length as `x` (", n, "), or be a ",
        "character vector of column names when `data` is supplied."
      )
    }
  } else {
    circumstances <- as.data.frame(circumstances)
  }

  # Remove missing values
  na_indices <- is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight)
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  circumstances <- subset(circumstances, !na_indices)

  if (length(x) == 0L) {
    abort_iopr("No complete observations remain after removing missing values.")
  }
  check_positive(x, "x")

  iop_total <- gini_ineq(x, weight)

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
    iop_r <- 1 - iop_xhat / iop_total

    theta <- list(
      iop_xhat = iop_xhat,
      iop_total = iop_total,
      iop_r = iop_r
    )
    orig_data <- data.frame(stratum, cluster, x, weight, circumstances, iop_xhat)
  } else {
    mu_hat <- lm_mod$fitted.values
    iop_muhat <- gini_ineq(mu_hat, weight = weight)
    iop_r <- iop_muhat / iop_total

    theta <- list(
      iop_muhat = iop_muhat,
      iop_total = iop_total,
      iop_r = iop_r
    )
    orig_data <- data.frame(stratum, cluster, x, weight, circumstances, mu_hat)
  }

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
    .combine = "rbind",
    .export = c("gini_ineq", "iop_rel_est")
  ) %dopar% {
    boot_sel <- orig_data |>
      dplyr::select(stratum, cluster) |>
      dplyr::group_by(stratum) |>
      dplyr::mutate(Hs = length(unique(cluster))) |>
      dplyr::group_by(stratum, Hs) |>
      tidyr::nest() |>
      dplyr::ungroup() |>
      dplyr::mutate(
        samp = purrr::map2(
          data, Hs, \(d, h) dplyr::slice_sample(d, n = h, replace = TRUE)
        )
      ) |>
      dplyr::select(-data, -Hs) |>
      tidyr::unnest(samp)

    boot_sample <- boot_sel |>
      dplyr::left_join(
        orig_data,
        by = dplyr::join_by(stratum, cluster),
        relationship = "many-to-many"
      )

    iop_total_b <- gini_ineq(boot_sample$x, boot_sample$weight)

    if (distribution == "standardized") {
      iop_xhat_b <- gini_ineq(boot_sample$iop_xhat, boot_sample$weight)
      iop_r_b <- 1 - iop_xhat_b / iop_total_b
      list(iop_xhat = iop_xhat_b, iop_total = iop_total_b, iop_r = iop_r_b)
    } else {
      iop_muhat_b <- gini_ineq(boot_sample$mu_hat, boot_sample$weight)
      iop_r_b <- iop_muhat_b / iop_total_b
      list(iop_muhat = iop_muhat_b, iop_total = iop_total_b, iop_r = iop_r_b)
    }
  }

  theta.var <- sum((unlist(thetastar[, "iop_r"]) - theta$iop_r)^2) / nboot
  theta.cov <- sum(
    (unlist(thetastar[, 1]) - theta[[1]]) *
      (unlist(thetastar[, "iop_total"]) - theta$iop_total)
  ) / nboot

  ans <- list(
    est = theta$iop_r,
    var = theta.var,
    theta.cov = theta.cov,
    thetastar = thetastar
  )

  return(ans)
}
