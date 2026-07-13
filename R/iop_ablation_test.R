#' Compare ablation contributions between two data sets
#'
#' Tests, for each circumstance, whether its contribution to relative inequality
#' of opportunity (IOP) differs between two data sets, using a pooled cluster
#' bootstrap. The observed statistic for a circumstance is the difference in
#' percentage contribution (`data2` minus `data1`) obtained by leave-one-out
#' ablation (see [iop_ablation_study()]).
#'
#' Under the null hypothesis that the two data sets share the same distribution,
#' a reference distribution is built by pooling the data sets and, within each
#' stratum, resampling clusters with replacement to form two pseudo-samples of
#' the original per-data-set cluster counts. The two-sided p-value is the
#' proportion of bootstrap differences at least as extreme (in absolute value) as
#' the observed difference. Cluster ids are treated as nested within a data set,
#' so identical ids in `data1` and `data2` are kept distinct.
#'
#' @param data1,data2 Data frames for the two groups being compared. Both must
#'   contain every column named below.
#' @param x Character; name of the strictly positive outcome column.
#' @param circumstances Character vector (length >= 2) of circumstance column
#'   names.
#' @param cluster Character; name of the cluster (primary sampling unit) column,
#'   used for the cluster bootstrap.
#' @param stratum Character; name of the stratum column. A single stratum is
#'   assumed when `NULL`.
#' @param weight Character; name of the sampling-weight column. Equal weights are
#'   used when `NULL`.
#' @param n_boot Number of bootstrap replications. Default `999`.
#' @param distribution `"smoothed"` (default) or `"standardized"`; passed to the
#'   IOP estimator.
#' @param seed Optional integer; sets the random seed for reproducibility.
#' @returns A data frame with one row per circumstance and columns:
#'   \describe{
#'     \item{circumstance}{Name of the circumstance.}
#'     \item{pc_1, pc_2}{Percentage contribution in `data1` and `data2`.}
#'     \item{diff}{`pc_2 - pc_1`.}
#'     \item{se}{Bootstrap standard error of the difference.}
#'     \item{p_value}{Two-sided pooled-bootstrap p-value.}
#'     \item{significance}{Significance code (`***`, `**`, `*`, `.`, or `""`).}
#'   }
#' @seealso [iop_ablation_study()], [iop_rel()]
#' @examples
#' round1 <- subset(synthetic_survey, round == "round_1")
#' round2 <- subset(synthetic_survey, round == "round_2")
#'
#' # Do circumstances contribute differently to relative IOP across the two
#' # rounds? (small n_boot for a quick illustration)
#' \donttest{
#' iop_ablation_test(
#'   data1 = round1, data2 = round2,
#'   x = "expenditure", cluster = "cluster", stratum = "stratum",
#'   weight = "weight",
#'   circumstances = c("social_group", "religion", "land_category", "hh_type"),
#'   n_boot = 199, seed = 1
#' )
#' }
#' @export
#' @importFrom dplyr distinct count group_by group_modify ungroup slice_sample
#' @importFrom dplyr left_join
#' @importFrom stats sd
iop_ablation_test <- function(data1, data2, x, circumstances, cluster,
                          stratum = NULL, weight = NULL, n_boot = 999L,
                          distribution = c("smoothed", "standardized"),
                          seed = NULL) {
  distribution <- match.arg(distribution)

  if (!is.data.frame(data1) || !is.data.frame(data2)) {
    abort_iopr("`data1` and `data2` must both be data frames.")
  }
  if (!is.character(circumstances) || length(circumstances) < 2L) {
    abort_iopr("`circumstances` must be a character vector of at least two ",
               "column names.")
  }
  for (arg in list(list(x, "x"), list(cluster, "cluster"))) {
    if (!is.character(arg[[1]]) || length(arg[[1]]) != 1L) {
      abort_iopr("`", arg[[2]], "` must be a single column name.")
    }
  }
  check_count(n_boot, "n_boot")
  if (!is.null(seed)) {
    check_number(seed, "seed")
    set.seed(seed)
  }

  # Complete-case pieces for one data set; clusters are tagged by source so that
  # identical ids across data sets are not merged.
  extract <- function(d, label, src) {
    need <- c(x, cluster, circumstances)
    if (!is.null(stratum)) need <- c(need, stratum)
    if (!is.null(weight)) need <- c(need, weight)
    missing_cols <- setdiff(need, names(d))
    if (length(missing_cols)) {
      abort_iopr("Column(s) not found in `", label, "`: ",
                 paste(missing_cols, collapse = ", "), ".")
    }

    xx <- d[[x]]
    cl <- paste0(src, "::", as.character(d[[cluster]]))
    st <- if (is.null(stratum)) rep("1", nrow(d)) else as.character(d[[stratum]])
    ww <- if (is.null(weight)) rep(1, nrow(d)) else d[[weight]]
    circ <- d[, circumstances, drop = FALSE]

    keep <- !(is.na(xx) | is.na(cl) | is.na(st) | is.na(ww) |
                apply(is.na(circ), 1, any))
    if (!any(keep)) {
      abort_iopr("`", label, "` has no complete observations.")
    }

    out <- data.frame(.src = src, .stratum = st[keep], .cluster = cl[keep],
                      .x = xx[keep], .w = ww[keep], stringsAsFactors = FALSE)
    out <- cbind(out, circ[keep, , drop = FALSE])
    return(out)
  }

  piece1 <- extract(data1, "data1", 1L)
  piece2 <- extract(data2, "data2", 2L)

  check_outcome(piece1$.x, "x")
  check_positive(piece1$.x, "x")
  check_outcome(piece2$.x, "x")
  check_positive(piece2$.x, "x")

  # Observed percentage contributions and their difference
  pc1 <- ablation_pc(piece1$.x, piece1$.w, piece1[, circumstances, drop = FALSE],
                     distribution)
  pc2 <- ablation_pc(piece2$.x, piece2$.w, piece2[, circumstances, drop = FALSE],
                     distribution)
  diff_orig <- pc2 - pc1

  # Pool the two data sets for the null bootstrap
  pool <- rbind(piece1, piece2)
  cluster_counts <- dplyr::count(
    dplyr::distinct(pool, .src, .stratum, .cluster), .src, .stratum,
    name = "n_clusters"
  )
  n_by <- function(src) {
    sub <- cluster_counts[cluster_counts$.src == src, ]
    counts <- sub$n_clusters
    names(counts) <- sub$.stratum
    return(counts)
  }
  n1_by <- n_by(1L)
  n2_by <- n_by(2L)
  pooled_clusters <- dplyr::distinct(pool, .stratum, .cluster)

  # One pooled-null pseudo-sample: draw `counts` clusters per stratum (with
  # replacement) from the pooled clusters, then gather their rows.
  draw_sample <- function(counts) {
    picked <- pooled_clusters |>
      dplyr::group_by(.stratum) |>
      dplyr::group_modify(function(g, key) {
        n <- counts[[as.character(key$.stratum)]]
        if (is.null(n) || is.na(n) || n < 1L) {
          return(g[0, , drop = FALSE])
        }
        dplyr::slice_sample(g, n = n, replace = TRUE)
      }) |>
      dplyr::ungroup()

    boot <- dplyr::left_join(
      picked, pool,
      by = c(".stratum", ".cluster"), relationship = "many-to-many"
    )
    return(boot)
  }

  # Bootstrap loop; failed replications (e.g. a resampled circumstance with a
  # single level) are recorded as NA and dropped from the summary
  boot_diff <- matrix(NA_real_, nrow = n_boot, ncol = length(circumstances))
  for (b in seq_len(n_boot)) {
    boot_diff[b, ] <- tryCatch({
      s1 <- draw_sample(n1_by)
      s2 <- draw_sample(n2_by)
      b1 <- ablation_pc(s1$.x, s1$.w, s1[, circumstances, drop = FALSE],
                        distribution)
      b2 <- ablation_pc(s2$.x, s2$.w, s2[, circumstances, drop = FALSE],
                        distribution)
      b2 - b1
    }, error = function(e) rep(NA_real_, length(circumstances)))
  }

  # Summarise per circumstance
  results <- lapply(seq_along(circumstances), function(j) {
    d_boot <- boot_diff[, j]
    d_boot <- d_boot[is.finite(d_boot)]
    d_obs <- diff_orig[[j]]
    p_value <- if (length(d_boot) == 0L || !is.finite(d_obs)) {
      NA_real_
    } else {
      mean(abs(d_boot) >= abs(d_obs))
    }
    data.frame(
      circumstance = circumstances[j],
      pc_1 = 100 * pc1[[j]],
      pc_2 = 100 * pc2[[j]],
      diff = 100 * d_obs,
      se = if (length(d_boot) > 1L) 100 * stats::sd(d_boot) else NA_real_,
      p_value = p_value,
      significance = sig_code(p_value),
      stringsAsFactors = FALSE
    )
  })

  ans <- do.call(rbind, results)
  rownames(ans) <- NULL
  return(ans)
}


# Percentage contribution (as a fraction) of each circumstance to relative IOP,
# by leave-one-out ablation, using the fast point estimator. Returns a named
# numeric vector aligned with `names(circ_df)`.
#
# @keywords internal
# @noRd
ablation_pc <- function(x, weight, circ_df, distribution) {
  circ_df <- as.data.frame(circ_df)
  full <- iop_rel_est(x, weight, circ_df, distribution)$rel_iop
  vars <- names(circ_df)

  pc <- vapply(seq_along(vars), function(j) {
    if (!is.finite(full) || full <= 0) {
      return(NA_real_)
    }
    reduced <- iop_rel_est(
      x, weight, circ_df[, -j, drop = FALSE], distribution
    )$rel_iop
    return(1 - reduced / full)
  }, numeric(1L))

  names(pc) <- vars
  return(pc)
}


# Map a p-value to a significance code.
#
# @keywords internal
# @noRd
sig_code <- function(p) {
  if (!is.finite(p)) {
    return(NA_character_)
  }
  if (p <= 0.001) {
    return("***")
  } else if (p <= 0.01) {
    return("**")
  } else if (p <= 0.05) {
    return("*")
  } else if (p <= 0.1) {
    return(".")
  }
  return("")
}
