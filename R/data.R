#' Synthetic household survey data
#'
#' A small simulated household survey that mimics the structure of the National
#' Sample Survey consumption rounds analysed in the iopr paper: a stratified,
#' cluster-sampled design with sampling weights, a strictly positive expenditure
#' outcome, and several categorical circumstance variables. Two survey rounds are
#' included; circumstances matter more in `round_2`, so the data can exercise the
#' ablation study ([iop_ablation_study()]) and the two-sample ablation test
#' ([iop_ablation_test()]). The values are simulated and correspond to no real
#' households.
#'
#' @format A data frame with 1,440 rows and 11 variables:
#' \describe{
#'   \item{round}{Survey round, `round_1` or `round_2` (factor).}
#'   \item{stratum}{Stratum identifier.}
#'   \item{cluster}{Cluster (primary sampling unit) identifier, nested in
#'     stratum.}
#'   \item{weight}{Sampling weight.}
#'   \item{expenditure}{Household consumption expenditure (the outcome).}
#'   \item{social_group}{Social group: SC, ST, OBC, General (factor).}
#'   \item{religion}{Religion: Hindu, Muslim, Christian, Other (factor).}
#'   \item{dwelling_type}{Dwelling tenure: Owned, Rented, Other (factor).}
#'   \item{land_category}{Land possessed, from Landless to Large (factor).}
#'   \item{hh_type}{Household type: Casual, Self-employed, Regular, Other
#'     (factor).}
#'   \item{hh_size}{Number of household members.}
#' }
#' @source Simulated by `data-raw/make_synthetic_survey.R`.
"synthetic_survey"
