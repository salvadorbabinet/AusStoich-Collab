# AusStoich Exploratory Data Analysis (Salvador)

# This is my start to a thorough EDA, which involves looking at variation
# within single variables and covariation between variables. This script
# was used to identify merge errors & outliers in the final data object.

# Libraries & functions ----
library(here)
library(tidyverse)
library(corrplot)
library(patchwork)
library(httpgd)

hgd()
hgd_browse()
theme_set(theme_bw())

histogram <- function(data, variable, bins = NULL, ylim = NULL) {
  ggplot(data, aes(x = {{variable}})) + 
    geom_histogram(bins = bins) + 
    coord_cartesian(ylim = ylim)
}

summarize_cont <- function(data, variable, grouping = NULL) {
  data |> summarize(
    min = min({{variable}}, na.rm = T),
    median = median({{variable}}, na.rm = T),
    mean = mean({{variable}}, na.rm = T),
    var = var({{variable}}, na.rm = T), 
    sd = sd({{variable}}, na.rm = T), 
    max = max({{variable}}, na.rm = T), 
    n = n(), 
    is_NA = sum(is.na({{variable}})),
    .by = {{grouping}}
  )
}

dot_plot_by_family <- function(data = aus_data, xvar, yvar) {
  p1 <- ggplot(data, aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point(alpha = 0.2, size = 0.6) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    theme_bw()
  p2 <- ggplot(mapping = aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point(
      data = filter(data, !family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      alpha = 0.2,
      size = 0.6) +
    geom_smooth(data = data, method = "lm", linetype = "dashed", se = FALSE) +
    geom_point(
      data = filter(data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      alpha = 0.2,
      size = 0.6) +
    geom_smooth(
      data = filter(data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      method = "lm",
      se = FALSE) +
    theme_bw()
  p1 + p2
}

dot_plot_by_factor <- function(xvar, yvar, factor, data = aus_data) {
  ggplot(data, aes(x = {{xvar}}, y = {{yvar}}, color = {{factor}})) +
    geom_jitter(alpha = 0.2, width = 0.1) +
    geom_smooth(method = "lm", se = FALSE)
}

log_plot_by_family <- function(data = aus_data, xvar, yvar) {
  p1 <- ggplot(data, aes(x = log({{xvar}}), y = log({{yvar}}))) +
    geom_point(alpha = 0.2, size = 0.6) +
    geom_smooth(
      method = lm,
      se = FALSE,
      linetype = "dashed"
    ) +
    geom_abline(intercept = 0, slope = 1)
  p2 <- ggplot(mapping = aes(x = log({{xvar}}), y = log({{yvar}}))) +
    geom_point(
      data = filter(data, !family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      alpha = 0.2,
      size = 0.6
    ) +
    geom_smooth(
      data = data,
      method = lm,
      linetype = "dashed",
      se = FALSE
    ) +
    geom_point(
      data = filter(data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      alpha = 0.2,
      size = 0.6
    ) +
    geom_smooth(
      data = filter(data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      method = lm,
      se = FALSE
    ) +
    geom_abline(intercept = 0, slope = 1)
  p1 + p2
}

density_plot_by_family <- function(data = aus_data, xvar, xlim = NULL, ylim = NULL) { # nolint: line_length_linter.
  ggplot(
    data = filter(data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
    mapping = aes(x = {{xvar}}, color = family, fill = family)
  ) +
  geom_density(alpha = 0.4, linewidth = 0.7) +
  coord_cartesian(xlim = xlim, ylim = ylim)
}

density_plot_by_factor <- function(xvar, factor, xlim = NULL, ylim = NULL, data = aus_data) { # nolint: line_length_linter.
  ggplot(data, aes(x = {{xvar}}, color = {{factor}}, fill = {{factor}})) +
    geom_density(alpha = 0.4, linewidth = 0.7)
}


# Variability ----
variability_data <- aus_data |> nest_by(species_binom) |>
  mutate(n = nrow(data)) |> 
  filter(n > 50) |> 
  unnest(everything())
variability_data

p1 <- ggplot(variability_data, aes(x = species_binom, y = leaf_N_per_dry_mass)) + 
  geom_jitter(alpha = 0.6, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Species", y = "Foliar Nitrogen (mg/g)")

p2 <- ggplot(variability_data, aes(x = species_binom, y = leaf_P_per_dry_mass)) + 
  geom_jitter(alpha = 0.6, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Species", y = "Foliar Phosphorus (mg/g)")

p3 <- ggplot(variability_data, aes(x = species_binom, y = leaf_C_per_dry_mass)) + 
  geom_jitter(alpha = 0.6, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(400, 600)) +
  labs(x = "Species", y = "Foliar Carbon (mg/g)")

p1 + p2 + p3

p1 <- ggplot(variability_data, aes(x = species_binom, y = leaf_N_per_dry_mass / leaf_P_per_dry_mass)) + 
  geom_jitter(alpha = 0.6, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Species")

p2 <- ggplot(variability_data, aes(x = species_binom, y = leaf_N_per_dry_mass / leaf_C_per_dry_mass)) + 
  geom_jitter(alpha = 0.6, width = 0.1) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Species")

p1 + p2


# Variation ----
# Quick look at continuous distributions via iteration
cont_data <- aus_data |> select(where(is.numeric))
for (i in 4:ncol(cont_data)) {
  print(histogram(cont_data, cont_data[[i]], bins = 50))
}

# Closer look...
# ...at plant traits 
all_data |> histogram(leaf_N_per_dry_mass, 80) # N = 75 is weird, entire Geange data set is suspect 
outlier_candidates <- all_data |> filter(leaf_N_per_dry_mass > 60) # |> bind_rows(outliers)

summarize_cont(all_data, leaf_N_per_dry_mass)

all_data |> histogram(leaf_P_per_dry_mass, 80) # P = 9.99 is weird 
outlier_candidates <- all_data |> filter(leaf_P_per_dry_mass > 9) |> bind_rows(outliers)

summarize_cont(all_data, leaf_P_per_dry_mass)

all_data |> histogram(leaf_C_per_dry_mass, 80) # C = 678, 195 (Wills), both Dong, & 235 weird 
outlier_candidates <- all_data |> filter(leaf_C_per_dry_mass > 650 | leaf_C_per_dry_mass < 250) |> bind_rows(outliers)

summarize_cont(all_data, leaf_C_per_dry_mass)

# ...at environmental data 
all_data |> histogram(SN_total_0_30, 50) 
all_data |> filter(SN_total_0_30 > 0.35) |> 
  select(dataset_id, species_binom, lat_deg, long_deg) # High N is just fertile land 

all_data |> histogram(SP_total_0_30, 50) 
all_data |> filter(SP_total_0_30 > 0.2) |> 
  select(dataset_id, species_binom, lat_deg, long_deg) |>  # High P is independently verified 
  print(n = 25)

all_data |> histogram(CEC_total_0_30, 80) 
all_data |> filter(CEC_total_0_30 > 30) |> 
  select(dataset_id, species_binom, lat_deg, long_deg) # Same site as high N 

v3_merge_error <- aus_data |> filter(precipitation > 1000)

# Fiona recommends NPP / AET investigation 
all_data |> histogram(MAT, 80) 
all_data |> histogram(NPP, 20) 
all_data |> histogram(AET, 50) 


# Co-variation ----
# Pearson Correlation Matrix (could also do Kendall or Spearman coeffs.)
corr_matrix <- aus_data |> 
  select(where(is.numeric)) |> 
  select(!c(Unique_ID, lat_deg, long_deg, NP_ratio, CN_ratio, CP_ratio)) |> 
  cor(use = 'pairwise.complete.obs')  # complete.obs / na.or.complete / pairwise.complete.obs

corr_matrix
corr_matrix |> corrplot(method = 'ellipse', tl.col = 'black')

# Leaf N by predictors
# Start with soil N (and visualize by major family)
aus_data |> count(family) |> arrange(desc(n))
aus_data |> ggplot(aes(x = SN_total_0_30, y = leaf_N_per_dry_mass)) + 
  geom_point(alpha = 0.2, size = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  theme_bw()

# Major families only
ggplot(
    data = filter(aus_data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
    mapping = aes(x = SN_total_0_30, y = leaf_N_per_dry_mass, color = family)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = F) +
  theme_bw()

# Investigate relationships across major families
# Foliar to environmental elements
dot_plot_by_family(xvar = SN_total_0_30, yvar = leaf_N_per_dry_mass)
density_plot_by_family(xvar = SN_total_0_30)

dot_plot_by_family(xvar = SP_total_0_30, yvar = leaf_P_per_dry_mass)
density_plot_by_family(xvar = SP_total_0_30, xlim = c(0, 0.1))

dot_plot_by_family(xvar = AP_total_0_30, yvar = leaf_P_per_dry_mass)
density_plot_by_family(xvar = AP_total_0_30)

# Foliar elements to each other
dot_plot_by_family(xvar = leaf_P_per_dry_mass, yvar = leaf_N_per_dry_mass)
log_plot_by_family(xvar = leaf_P_per_dry_mass, yvar = leaf_N_per_dry_mass)
dot_plot_by_family(xvar = leaf_C_per_dry_mass, yvar = leaf_N_per_dry_mass)
dot_plot_by_family(xvar = leaf_C_per_dry_mass, yvar = leaf_P_per_dry_mass)

density_plot_by_family(xvar = leaf_N_per_dry_mass)
density_plot_by_family(xvar = leaf_P_per_dry_mass)
density_plot_by_family(xvar = leaf_C_per_dry_mass)

ggplot(aus_data, aes(x = leaf_P_per_dry_mass, y = leaf_N_per_dry_mass)) +
    geom_point(alpha = 0.2, size = 0.6) +
    geom_smooth(
      method = glm,
      se = FALSE,
      linetype = "dashed"
    ) +
    geom_abline(intercept = 0, slope = 1) +
    coord_trans(x = "log", y = "log")

ggplot(mapping = aes(x = log(leaf_P_per_dry_mass), y = log(leaf_N_per_dry_mass))) +
    geom_point(
      data = filter(aus_data, !family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      alpha = 0.2,
      size = 0.6
    ) +
    geom_smooth(
      data = aus_data,
      method = lm,
      linetype = "dashed",
      se = FALSE
    ) +
    geom_point(
      data = filter(aus_data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      alpha = 0.2,
      size = 0.6
    ) +
    geom_smooth(
      data = filter(aus_data, family %in% c("Myrtaceae", "Fabaceae", "Proteaceae")),
      mapping = aes(color = family),
      method = lm,
      se = FALSE
    ) +
    geom_abline(intercept = 0, slope = 1)

# Other factors of interest
dot_plot_by_factor(SN_total_0_30, leaf_N_per_dry_mass, putative_BNF)
density_plot_by_factor(leaf_N_per_dry_mass, putative_BNF)
density_plot_by_factor(SN_total_0_30, putative_BNF)

ggplot(
  aus_data,
  aes(
    x = SN_total_0_30,
    y = leaf_N_per_dry_mass,
    color = putative_BNF,
    fill = putative_BNF
  )) +
  geom_violin(alpha = 0.4)

dot_plot_by_factor(SN_total_0_30, leaf_N_per_dry_mass, myc_type)
density_plot_by_factor(leaf_N_per_dry_mass, woodiness)
density_plot_by_factor(SN_total_0_30, woodiness)
