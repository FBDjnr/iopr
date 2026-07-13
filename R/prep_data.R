#' Prepare survey data for inequality analysis
#'
#' Attaches cluster size, household count, and household size information to
#' the data, combines duplicate households by summing the outcome variable, and
#' returns a tidy data frame ready for use in [gini_index()] or [iop_rel()].
#'
#' @param x Outcome variable (bare name or expression referencing a column in
#'   `data`).
#' @param stratum Stratum ID variable. Defaults to a single stratum if `NULL`.
#' @param cluster Cluster ID variable. Defaults to individual observations if
#'   `NULL`.
#' @param hh_id Household ID variable. Defaults to row index if `NULL`.
#' @param hh_size Number of household members. Defaults to 1 if `NULL`.
#' @param circumstances Optional circumstance variable. Omitted from the output
#'   if `NULL`.
#' @param weight Sampling weight. Defaults to `hh_size` if `NULL`.
#' @param data Optional data frame. If provided, all other arguments are
#'   evaluated in the context of `data`.
#' @returns A data frame with columns `stratum`, `cluster`, `hh_id`,
#'   `hh_size`, `circumstances` (if supplied), `weight`, the outcome variable,
#'   `H_s` (clusters per stratum), and `M_scs` (households per cluster-stratum).
#' @export
#' @importFrom dplyr group_by summarise mutate ungroup select arrange
#' @importFrom dplyr rename_with all_of
prep_data <- function(x, stratum = NULL, cluster = NULL, hh_id = NULL,
                      hh_size = NULL, circumstances = NULL,
                      weight = NULL, data = NULL) {
  # Name to restore on the outcome column in the output
  var.name <- deparse(substitute(x))
  if (grepl("\\$", var.name)) {
    var.name <- strsplit(var.name, "\\$")[[1]][2]
  }

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      abort_iopr("`data` must be a data frame or NULL.")
    }
    pf <- parent.frame()
    x <- eval(substitute(x), data, pf)
    stratum <- eval(substitute(stratum), data, pf)
    cluster <- eval(substitute(cluster), data, pf)
    hh_id <- eval(substitute(hh_id), data, pf)
    hh_size <- eval(substitute(hh_size), data, pf)
    circumstances <- eval(substitute(circumstances), data, pf)
    weight <- eval(substitute(weight), data, pf)
  }

  check_outcome(x, "x")
  n <- length(x)
  check_design(stratum, n, "stratum")
  check_design(cluster, n, "cluster")
  check_design(hh_id, n, "hh_id")
  check_design(hh_size, n, "hh_size")
  check_design(circumstances, n, "circumstances")
  check_weight(weight, n, "weight")

  has_circ <- !is.null(circumstances)

  # Fill in missing columns
  ones <- rep(1, n)
  if (is.null(stratum)) stratum <- ones
  if (is.null(cluster)) cluster <- ones
  if (is.null(hh_id)) hh_id <- seq_len(n)
  if (is.null(hh_size)) hh_size <- ones
  if (is.null(weight)) weight <- hh_size

  dta <- data.frame(stratum, cluster, hh_id, hh_size, weight, x)
  if (has_circ) dta$circumstances <- circumstances

  # Combine duplicate households
  if (has_circ) {
    dta <- dta |>
      dplyr::group_by(stratum, cluster, hh_id) |>
      dplyr::summarise(
        x = sum(x, na.rm = TRUE),
        hh_size = mean(hh_size, na.rm = TRUE),
        circumstances = min(circumstances, na.rm = TRUE),
        weight = mean(weight, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    dta <- dta |>
      dplyr::group_by(stratum, cluster, hh_id) |>
      dplyr::summarise(
        x = sum(x, na.rm = TRUE),
        hh_size = mean(hh_size, na.rm = TRUE),
        weight = mean(weight, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Number of clusters per stratum (H_s)
  dta <- dta |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(H_s = count_unique(cluster)) |>
    dplyr::ungroup()

  # Number of households per cluster per stratum (M_scs)
  dta <- dta |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::mutate(M_scs = count_unique(hh_id)) |>
    dplyr::ungroup()

  # Final data
  keep <- c(
    "stratum", "cluster", "hh_id", "hh_size",
    if (has_circ) "circumstances", "weight", "x", "H_s", "M_scs"
  )
  dta <- dta |>
    dplyr::select(dplyr::all_of(keep)) |>
    dplyr::arrange(stratum, cluster, hh_id) |>
    dplyr::rename_with(\(nm) gsub("^x$", var.name, nm))

  return(dta)
}
