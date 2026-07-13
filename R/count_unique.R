#' Count the number of unique values
#'
#' @param x A vector.
#' @returns An integer giving the number of unique values in `x`.
#' @export
#' @examples
#' count_unique(c(1, 2, 2, 3))
count_unique <- function(x) {
  if (missing(x) || is.null(x)) {
    abort_iopr("`x` must be supplied.")
  }
  if (!is.atomic(x)) {
    abort_iopr("`x` must be an atomic vector, not ", class(x)[1], ".")
  }
  length(unique(x))
}
