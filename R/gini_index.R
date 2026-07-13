#' Gini index and its variance
#'
#' Computes the Gini index and, optionally, its variance using either the
#' influence function approach or the Bhattacharya decomposition, for complex
#' survey designs with stratification and clustering. Observations with missing
#' values are dropped before computation.
#'
#' @inheritParams gini_var
#' @param variance Logical; compute the variance? Default is `TRUE`.
#' @param var.decompose Logical; decompose the variance using the Bhattacharya
#'   method? Default is `FALSE`. Only used when `variance = TRUE`.
#' @param ... Currently unused.
#' @returns
#'   - If `variance = FALSE`: a scalar Gini estimate.
#'   - If `variance = TRUE` and `var.decompose = FALSE`: a list with elements
#'     `est` and `var`.
#'   - If `variance = TRUE` and `var.decompose = TRUE`: a list with elements
#'     `est`, `var`, and `var.decompose`.
#' @export
#' @seealso [gini_ineq()], [gini_var()], [gini_var_bhatt()]
gini_index <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                       data = NULL, variance = TRUE, var.decompose = FALSE,
                       ...) {
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
  warn_if_negative(x, "x")
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

  # Remove missing values
  na_indices <- is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight)
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]

  if (length(x) == 0L) {
    abort_iopr("No complete observations remain after removing missing values.")
  }

  if (!variance) {
    ans <- gini_ineq(x, weight)
  } else if (!var.decompose) {
    ans <- gini_var(x, stratum, cluster, weight, data = NULL)
  } else {
    ans <- gini_var_bhatt(x, stratum, cluster, weight, data = NULL)
  }

  return(ans)
}
