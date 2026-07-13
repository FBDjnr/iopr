# Declare the column names used inside dplyr non-standard-evaluation pipelines
# so that R CMD check does not report them as "no visible binding for global
# variable". These are all transient columns created within `mutate()` /
# `summarise()` calls, not true global variables.
utils::globalVariables(c(
  "B", "Fx", "Fy", "Hs", "mtemp", "mtempx", "mtempy", "n_s",
  "nvar", "nvar_x", "nvar_xy", "nvar_y", "q11", "q11x", "q11y",
  "q12", "q12x", "q12y", "samp", "sums", "sumsx", "sumsy",
  "t1", "t1x", "t1xy", "t1y", "u_sc", "u_sc_x", "u_sc_y",
  "u_sch", "u_sch_x", "u_sch_y", "w", "ztemp", "ztempx", "ztempy"
))
