#' Bootstrap sample for a complex survey
#'
#' Draws a bootstrap sample by resampling clusters with replacement within
#' each stratum. The number of clusters drawn per stratum equals the original
#' number of clusters in that stratum.
#'
#' @param stratum Unquoted column name identifying strata.
#' @param cluster Unquoted column name identifying clusters.
#' @param data A data frame containing survey data.
#' @returns A data frame with the same columns as `data`, with duplicated rows
#'   for resampled clusters.
#' @export
#' @importFrom dplyr select group_by mutate ungroup left_join join_by
#' @importFrom tidyr nest unnest
#' @importFrom purrr map2
bootstrap_sample <- function(stratum, cluster, data) {
  if (missing(data) || !is.data.frame(data)) {
    abort_iopr("`data` must be a data frame.")
  }
  if (nrow(data) == 0L) {
    abort_iopr("`data` has no rows.")
  }

  # Sample clusters (with replacement) within each stratum
  boot_sel <- data |>
    dplyr::select({{ stratum }}, {{ cluster }}) |>
    dplyr::group_by({{ stratum }}) |>
    dplyr::mutate(Hs = length(unique({{ cluster }}))) |>
    dplyr::group_by({{ stratum }}, Hs) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      samp = purrr::map2(
        data, Hs, \(d, h) dplyr::slice_sample(d, n = h, replace = TRUE)
      )
    ) |>
    dplyr::select(-data, -Hs) |>
    tidyr::unnest(samp)

  # Extract selected clusters from the original data set
  boot_sample <- boot_sel |>
    dplyr::left_join(
      data,
      by = dplyr::join_by({{ stratum }}, {{ cluster }}),
      relationship = "many-to-many"
    )

  return(boot_sample)
}
