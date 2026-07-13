#' Create LaTeX column numbering for table output
#'
#' Generates a LaTeX string with `\\multicolumn` entries numbered `(1)`,
#' `(2)`, ..., `(n)`, suitable for use in table headers.
#'
#' @param n Number of columns. A single positive integer.
#' @returns A character string of LaTeX column-number cells separated by `&`.
#' @export
#' @examples
#' colnumbering(3)
colnumbering <- function(n) {
  check_count(n, "n")

  x <- paste0("\\multicolumn{1}{c}{(", seq_len(n), ")}")
  x <- paste(x, collapse = " & ")
  x <- paste(x, "\\\\")
  return(x)
}
