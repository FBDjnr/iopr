# Internal input-validation helpers for the iopr package.
#
# These functions are not exported. They centralise the argument checks used
# by the user-facing functions so that error messages are consistent and
# informative. Keeping them here keeps each exported function focused on its
# statistical logic while still failing early and clearly on bad input.
#
# @keywords internal
# @noRd

# Abort with a clean message (no "Error in f(...)" call clutter).
abort_iopr <- function(...) {
  stop(..., call. = FALSE)
}

# Resolve one argument against a `data` frame.
#
# `sub` is `substitute(arg)` captured in the caller; `val` is the (still
# unforced) argument; `data` is the supplied data frame; `env` is the caller's
# `parent.frame()`. If the user passed a bare column name it is evaluated in
# `data`; if a character string it is used to index `data`; otherwise the value
# is returned unchanged. `val` is only forced in the non-symbol branches, so a
# bare column name is never evaluated in the wrong environment.
resolve_column <- function(sub, val, data, env) {
  if (is.symbol(sub)) {
    eval(sub, data, env)
  } else if (!is.null(val)) {
    if (is.character(val) && length(val) == 1L) {
      if (!val %in% names(data)) {
        abort_iopr("Column \"", val, "\" was not found in `data`.")
      }
      data[[val]]
    } else {
      val
    }
  } else {
    val
  }
}

# `x` (the outcome): must be a non-empty numeric vector.
check_outcome <- function(x, arg = "x") {
  if (is.null(x)) {
    abort_iopr("`", arg, "` must be supplied.")
  }
  if (!is.numeric(x)) {
    abort_iopr("`", arg, "` must be a numeric vector, not ", class(x)[1], ".")
  }
  if (length(x) == 0L) {
    abort_iopr("`", arg, "` has length 0; supply at least one observation.")
  }
  invisible(x)
}

# Strictly positive values are required before taking log() (IOP models).
check_positive <- function(x, arg = "x") {
  ok <- x[!is.na(x)]
  if (length(ok) && any(ok <= 0)) {
    abort_iopr(
      "`", arg, "` must be strictly positive: a log-linear model is fitted ",
      "on log(", arg, ")."
    )
  }
  invisible(x)
}

# The Gini index is designed for non-negative outcomes; warn otherwise.
warn_if_negative <- function(x, arg = "x") {
  ok <- x[!is.na(x)]
  if (length(ok) && any(ok < 0)) {
    warning(
      "`", arg, "` contains negative values; the Gini index is intended for ",
      "non-negative outcomes and may fall outside [0, 1].",
      call. = FALSE
    )
  }
  invisible(x)
}

# Weight: NULL, or numeric of length `n`, non-negative and not summing to zero.
check_weight <- function(weight, n, arg = "weight") {
  if (is.null(weight)) {
    return(invisible(weight))
  }
  if (!is.numeric(weight)) {
    abort_iopr("`", arg, "` must be numeric or NULL, not ", class(weight)[1], ".")
  }
  if (length(weight) != n) {
    abort_iopr(
      "`", arg, "` has length ", length(weight),
      " but must match the number of observations (", n, ")."
    )
  }
  ok <- weight[!is.na(weight)]
  if (length(ok) && any(ok < 0)) {
    abort_iopr("`", arg, "` must not contain negative values.")
  }
  if (length(ok) && sum(ok) == 0) {
    abort_iopr("`", arg, "` must not sum to zero.")
  }
  invisible(weight)
}

# Design vector (stratum / cluster): NULL, or of length `n`.
check_design <- function(v, n, arg) {
  if (is.null(v)) {
    return(invisible(v))
  }
  if (length(v) != n) {
    abort_iopr(
      "`", arg, "` has length ", length(v),
      " but must match the number of observations (", n, ")."
    )
  }
  invisible(v)
}

# A single, non-missing TRUE/FALSE.
check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    abort_iopr("`", arg, "` must be a single TRUE or FALSE.")
  }
  invisible(x)
}

# A single finite number.
check_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    abort_iopr("`", arg, "` must be a single finite number.")
  }
  invisible(x)
}

# A single positive integer (nboot, no_cores, n).
check_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 ||
    x != round(x)) {
    abort_iopr("`", arg, "` must be a single positive integer.")
  }
  invisible(as.integer(x))
}

# A single probability strictly inside (0, 1).
check_prob <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x <= 0 || x >= 1) {
    abort_iopr("`", arg, "` must be a single number strictly between 0 and 1.")
  }
  invisible(x)
}
