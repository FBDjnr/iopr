#' Compute the Gini index
#'
#' Estimates the Gini index of a numeric vector, with optional sampling weights.
#' Without weights, uses the standard rank-based formula. With weights, delegates
#' to [reldist::gini()]. Observations with a missing value (in `x` or, when
#' supplied, `weight`) are dropped before computation.
#'
#' @param x A numeric vector of non-negative values.
#' @param weight Optional numeric weight vector of the same length as `x`. Must
#'   be non-negative and not sum to zero.
#' @returns A numeric scalar: the Gini index (between 0 and 1 for non-negative
#'   `x`).
#' @keywords internal
#' @importFrom reldist gini
gini_ineq <- function(x, weight = NULL) {
  check_outcome(x, "x")
  check_weight(weight, length(x), "weight")
  warn_if_negative(x, "x")

  if (is.null(weight)) {
    x <- sort(x)
    n <- length(x)
    if (n == 0L) {
      abort_iopr("`x` has no non-missing values.")
    }
    if (sum(x) == 0) {
      abort_iopr("Cannot compute the Gini index when all values are zero.")
    }
    i <- seq.int(1, n)
    g_num <- 2 * sum(i * x) / sum(x) - (n + 1L)
    g <- g_num / n
  } else {
    keep <- !(is.na(x) | is.na(weight))
    x <- x[keep]
    weight <- weight[keep]
    if (length(x) == 0L) {
      abort_iopr("`x` has no non-missing values.")
    }
    g <- reldist::gini(x = x, weights = weight)
  }

  return(g)
}
