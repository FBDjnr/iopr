#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Synthetic Survey Data Generator ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Builds `synthetic_survey`, a small simulated household survey that mimics the
# structure of India's National Sample Survey consumption rounds used in the
# iopr paper: a stratified, cluster-sampled design with sampling weights, a
# positive expenditure outcome, and several categorical "circumstance"
# variables. Two survey rounds are generated with slightly different
# circumstance effects so the data can exercise the two-sample ablation test.
# Objective: provide a self-contained example data set for the package help
# pages. Run this script to (re)generate data/synthetic_survey.rda.

set.seed(2025)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Design Constants ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

n_strata <- 6L               # strata (e.g. state x sector groups)
clusters_per_stratum <- 8L   # primary sampling units per stratum
hh_per_cluster <- 15L        # households sampled per cluster
rounds <- c("round_1", "round_2")

# Circumstance levels
social_groups <- c("SC", "ST", "OBC", "General")
religions <- c("Hindu", "Muslim", "Christian", "Other")
dwellings <- c("Owned", "Rented", "Other")
land_cats <- c("Landless", "Marginal", "Small", "Medium", "Large")
hh_types <- c("Casual", "Self-employed", "Regular", "Other")

# Effect of each circumstance level on log-expenditure. The gaps between levels
# are what create inequality of opportunity; larger gaps => larger contribution.
eff_social <- c(SC = -0.35, ST = -0.45, OBC = -0.15, General = 0.30)
eff_religion <- c(Hindu = 0.00, Muslim = -0.12, Christian = 0.10, Other = 0.05)
eff_dwelling <- c(Owned = 0.10, Rented = -0.05, Other = -0.15)
eff_land <- c(Landless = -0.30, Marginal = -0.10, Small = 0.05,
              Medium = 0.25, Large = 0.55)
eff_hh_type <- c(Casual = -0.25, `Self-employed` = 0.00,
                 Regular = 0.30, Other = -0.05)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Row Builder ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Generate one survey round. `scale` inflates the social-group and land effects
# so the two rounds differ in how much those circumstances matter.
make_round <- function(round_label, scale = 1) {
  n_hh <- n_strata * clusters_per_stratum * hh_per_cluster

  stratum <- rep(seq_len(n_strata), each = clusters_per_stratum * hh_per_cluster)
  cluster_within <- rep(rep(seq_len(clusters_per_stratum), each = hh_per_cluster),
                        times = n_strata)
  stratum_id <- sprintf("S%02d", stratum)
  cluster_id <- sprintf("%s_C%02d", stratum_id, cluster_within)

  # Stratum- and cluster-level random effects induce within-cluster correlation
  stratum_effect <- rnorm(n_strata, 0, 0.20)[stratum]
  cluster_key <- unique(cluster_id)
  cluster_effect <- rnorm(length(cluster_key), 0, 0.15)
  names(cluster_effect) <- cluster_key

  social_group <- sample(social_groups, n_hh, replace = TRUE,
                          prob = c(0.20, 0.10, 0.40, 0.30))
  religion <- sample(religions, n_hh, replace = TRUE,
                     prob = c(0.72, 0.14, 0.08, 0.06))
  dwelling_type <- sample(dwellings, n_hh, replace = TRUE,
                          prob = c(0.80, 0.14, 0.06))
  land_category <- sample(land_cats, n_hh, replace = TRUE,
                          prob = c(0.30, 0.32, 0.20, 0.12, 0.06))
  hh_type <- sample(hh_types, n_hh, replace = TRUE,
                    prob = c(0.30, 0.35, 0.20, 0.15))
  hh_size <- pmax(1L, rpois(n_hh, lambda = 4L))

  log_exp <- 8.5 +
    scale * eff_social[social_group] +
    eff_religion[religion] +
    eff_dwelling[dwelling_type] +
    scale * eff_land[land_category] +
    eff_hh_type[hh_type] +
    0.02 * hh_size +
    stratum_effect +
    cluster_effect[cluster_id] +
    rnorm(n_hh, 0, 0.35)

  expenditure <- round(exp(log_exp), 2)

  # Sampling weight: households in larger clusters get slightly larger weights,
  # plus noise, so the design is informative (weighted != unweighted).
  weight <- round(200 + 40 * hh_size + rgamma(n_hh, shape = 4, scale = 25), 1)

  data.frame(
    round = round_label,
    stratum = stratum_id,
    cluster = cluster_id,
    weight = weight,
    expenditure = expenditure,
    social_group = factor(social_group, levels = social_groups),
    religion = factor(religion, levels = religions),
    dwelling_type = factor(dwelling_type, levels = dwellings),
    land_category = factor(land_category, levels = land_cats),
    hh_type = factor(hh_type, levels = hh_types),
    hh_size = hh_size,
    stringsAsFactors = FALSE
  )
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Build and Save ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

synthetic_survey <- rbind(
  make_round("round_1", scale = 1.0),
  make_round("round_2", scale = 1.6)  # circumstances matter more in round 2
)
synthetic_survey$round <- factor(synthetic_survey$round, levels = rounds)
rownames(synthetic_survey) <- NULL

if (!dir.exists("data")) {
  dir.create("data")
}
save(synthetic_survey, file = "data/synthetic_survey.rda",
     compress = "bzip2", version = 2)
