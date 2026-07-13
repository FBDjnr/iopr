#' Relative Inequality of Opportunity
#'
#' Estimates relative IOP by regressing log-outcome on circumstance variables
#' and computing the Gini index of the resulting hypothesized distribution.
#' Optionally computes the variance using analytical or decomposed methods.
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
#' @param variance Logical; compute the variance? Default is `FALSE`.
#' @param var.decompose Logical; use Bhattacharya decomposition for variance?
#'   Default is `FALSE`. Only relevant when `variance = TRUE`.
#' @param distribution `"smoothed"` (default) or `"standardized"`. The
#'   `"smoothed"` method uses fitted values from the log-linear regression;
#'   `"standardized"` additionally controls for residual variation at the
#'   mean of circumstances.
#' @returns
#'   - If `variance = FALSE`: a named list with `total_iop`, `abs_iop`, and
#'     `rel_iop`.
#'   - If `variance = TRUE`: a data frame with rows `total_iop`, `abs_iop`,
#'     `rel_iop` and columns `est` and `var` (plus decomposed columns if
#'     `var.decompose = TRUE`).
#' @examples
#' round1 <- subset(synthetic_survey, round == "round_1")
#'
#' # Relative IOP from a set of circumstance variables (point estimate)
#' iop_rel(
#'   x = expenditure,
#'   circumstances = c("social_group", "religion", "land_category"),
#'   data = round1
#' )
#'
#' # With the survey design and analytical variance
#' iop_rel(
#'   x = expenditure, stratum = stratum, cluster = cluster, weight = weight,
#'   circumstances = c("social_group", "religion", "land_category"),
#'   data = round1, variance = TRUE
#' )
#' @export
#' @importFrom dplyr summarise across select all_of
#' @importFrom tidyselect where
#' @importFrom janitor remove_constant
#' @importFrom stats lm predict
iop_rel <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                    circumstances, data = NULL, variance = FALSE,
                    var.decompose = FALSE,
                    distribution = c("smoothed", "standardized")) {
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
    stratum <- resolve_column(substitute(stratum), stratum, data, pf)
    cluster <- resolve_column(substitute(cluster), cluster, data, pf)
    weight <- resolve_column(substitute(weight), weight, data, pf)
  }

  check_outcome(x, "x")
  n <- length(x)
  check_design(stratum, n, "stratum")
  check_design(cluster, n, "cluster")
  check_weight(weight, n, "weight")
  check_flag(variance, "variance")
  check_flag(var.decompose, "var.decompose")

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

  if (nrow(circumstances) != n) {
    abort_iopr(
      "`circumstances` must have ", n, " rows to match the length of `x`."
    )
  }

  # Remove missing values
  na_indices <- is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight) |
    apply(is.na(circumstances), 1, any)

  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  circumstances <- subset(circumstances, !na_indices)

  if (length(x) == 0L) {
    abort_iopr("No complete observations remain after removing missing values.")
  }
  check_positive(x, "x")

  # Drop any constant circumstance variable
  circumstances <- circumstances |>
    janitor::remove_constant(na.rm = TRUE)

  if (ncol(circumstances) == 0L) {
    warning(
      "All circumstance variables are constant; relative IOP will be 0.",
      call. = FALSE
    )
  }

  # Log-linear model of outcome on circumstance variables
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

    y <- exp(stats::predict(lm_mod, newdata = circumstances_mean) + e_hat)
  } else {
    y <- exp(lm_mod$fitted.values)
  }

  if (!variance) {
    total_iop <- gini_ineq(x, weight)

    if (distribution == "standardized") {
      abs_iop <- total_iop - gini_ineq(y, weight)
    } else {
      abs_iop <- gini_ineq(y, weight)
    }

    rel_iop <- abs_iop / total_iop
    ans <- list(total_iop = total_iop, abs_iop = abs_iop, rel_iop = rel_iop)
  } else {
    if (!var.decompose) {
      ans <- iop_var(x, y, stratum, cluster, weight, distribution)
    } else {
      ans <- iop_var_decompose(x, y, stratum, cluster, weight, distribution)
    }
  }

  return(ans)
}
