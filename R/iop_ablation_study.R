#' Ablation study for relative inequality of opportunity
#'
#' Quantifies how much each circumstance variable contributes to relative
#' inequality of opportunity (IOP) by leave-one-out ablation. Relative IOP is
#' computed once using all circumstances (the full model) and then again with
#' each circumstance removed in turn. The drop in relative IOP when a
#' circumstance is removed measures that circumstance's contribution: a large
#' drop means the circumstance explains much of the opportunity-driven
#' inequality.
#'
#' All models are fit on the same complete-case sample (rows with a missing
#' outcome, design variable, weight, or circumstance are dropped once, up front)
#' so the full and reduced estimates are comparable.
#'
#' @inheritParams iop_rel
#' @param circumstances A data frame (or coercible object) of at least two
#'   circumstance variables, or, when `data` is supplied, a character vector of
#'   at least two column names.
#' @returns A data frame with one row per circumstance and columns:
#'   \describe{
#'     \item{circumstance}{Name of the ablated circumstance.}
#'     \item{iop_full}{Relative IOP using all circumstances.}
#'     \item{iop_reduced}{Relative IOP with this circumstance removed.}
#'     \item{contribution}{`iop_full - iop_reduced`.}
#'     \item{pc_contribution}{Percentage contribution,
#'       `100 * contribution / iop_full`.}
#'   }
#' @seealso [iop_rel()], [iop_ablation_test()]
#' @examples
#' round1 <- subset(synthetic_survey, round == "round_1")
#'
#' # Contribution of each circumstance to relative IOP (leave-one-out)
#' iop_ablation_study(
#'   x = expenditure, stratum = stratum, cluster = cluster, weight = weight,
#'   circumstances = c("social_group", "religion", "land_category", "hh_type"),
#'   data = round1
#' )
#' @export
#' @importFrom dplyr select all_of
iop_ablation_study <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                           circumstances, data = NULL,
                           distribution = c("smoothed", "standardized")) {
  distribution <- match.arg(distribution)

  if (missing(circumstances) || is.null(circumstances)) {
    abort_iopr(
      "`circumstances` must be supplied: a data frame of at least two ",
      "circumstance variables, or (with `data`) a character vector of at ",
      "least two column names."
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

    if (!is.character(circumstances)) {
      abort_iopr(
        "When `data` is supplied, `circumstances` must be a character vector ",
        "of column names."
      )
    }
    missing_cols <- setdiff(circumstances, names(data))
    if (length(missing_cols)) {
      abort_iopr(
        "Column(s) not found in `data`: ",
        paste(missing_cols, collapse = ", "), "."
      )
    }
    circ_df <- dplyr::select(data, dplyr::all_of(circumstances))
  } else {
    circ_df <- as.data.frame(circumstances)
  }

  check_outcome(x, "x")
  n <- length(x)
  check_design(stratum, n, "stratum")
  check_design(cluster, n, "cluster")
  check_weight(weight, n, "weight")

  if (ncol(circ_df) < 2L) {
    abort_iopr(
      "`circumstances` must contain at least two variables for an ablation ",
      "study; only ", ncol(circ_df), " supplied."
    )
  }
  if (nrow(circ_df) != n) {
    abort_iopr(
      "`circumstances` must have ", n, " rows to match the length of `x`."
    )
  }

  if (is.null(stratum)) stratum <- rep(1, n)
  if (is.null(cluster)) cluster <- rep(1, n)
  if (is.null(weight)) weight <- rep(1, n)

  # Drop incomplete cases once so every model uses the same sample
  na_rows <- is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight) |
    apply(is.na(circ_df), 1, any)

  x <- x[!na_rows]
  stratum <- stratum[!na_rows]
  cluster <- cluster[!na_rows]
  weight <- weight[!na_rows]
  circ_df <- circ_df[!na_rows, , drop = FALSE]

  if (nrow(circ_df) == 0L) {
    abort_iopr("No complete observations remain after removing missing values.")
  }
  check_positive(x, "x")

  circ_names <- names(circ_df)

  # Full model: relative IOP with every circumstance included
  iop_full <- iop_rel(
    x, stratum, cluster, weight,
    circumstances = circ_df, data = NULL,
    variance = FALSE, distribution = distribution
  )$rel_iop

  # Leave-one-out: relative IOP with each circumstance removed in turn
  results <- lapply(seq_along(circ_names), function(j) {
    iop_reduced <- iop_rel(
      x, stratum, cluster, weight,
      circumstances = circ_df[, -j, drop = FALSE], data = NULL,
      variance = FALSE, distribution = distribution
    )$rel_iop

    contribution <- iop_full - iop_reduced
    data.frame(
      circumstance = circ_names[j],
      iop_full = iop_full,
      iop_reduced = iop_reduced,
      contribution = contribution,
      pc_contribution = 100 * contribution / iop_full,
      stringsAsFactors = FALSE
    )
  })

  ans <- do.call(rbind, results)
  rownames(ans) <- NULL
  return(ans)
}
