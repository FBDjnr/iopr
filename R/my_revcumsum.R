# Weighted reverse cumulative sum (internal helper)
#
# For each element of `x`, computes the sum of `weight` over all observations
# with value greater than or equal to that element (grouping ties together).
#
# @param x A numeric vector.
# @param weight Optional numeric weight vector of the same length as `x`.
#   Defaults to equal weights.
# @returns A numeric vector of the same length as `x`.
# @keywords internal
# @noRd
#' @importFrom dplyr group_by summarise arrange mutate left_join desc
my_revcumsum <- function(x, weight = NULL) {
  check_outcome(x, "x")
  if (is.null(weight)) {
    weight <- rep(1, length(x))
  } else if (length(weight) != length(x)) {
    abort_iopr(
      "`weight` has length ", length(weight),
      " but must match the length of `x` (", length(x), ")."
    )
  }

  dt <- data.frame(x, weight)

  dt_Sx <- dt |>
    group_by(x) |>
    summarise(w = sum(weight), .groups = "drop") |>
    arrange(desc(x)) |>
    mutate(Sx = cumsum(w))

  dt_full <- dt |>
    left_join(dt_Sx, by = "x")

  return(dt_full$Sx)
}
