# AusStoich Exploratory Data Analysis 
# Libraries & functions 
library(here)
library(corrplot)
library(tidyverse)

histogram <- function(data, variable, bins = NULL, ylim = NULL) {
  ggplot(data, aes(x = {{variable}})) + 
    geom_histogram(bins = bins) + 
    coord_cartesian(ylim = ylim) +
    labs(x = label)
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

# Categorical variables 
all_data |> count(across(woodiness:putative_BNF))
all_data |> filter(woodiness == 1 & reclass_life_history == 'short') |> 
  distinct(species_binom) # Ambiguous, look up species / set as NA accordingly


# Variation ---------------------------------------------------------------
# Quick look at continuous distributions via iteration 
cont_data <- all_corrected_data |> select(where(is.numeric))
for (i in 4:ncol(cont_data)) {
  print(histogram(cont_data, cont_data[[i]], bins = 50))
}

# Closer look...
# ...at plant traits 
all_data |> histogram(leaf_N_per_dry_mass, 80) # N = 75 is weird, entire Geange data set is suspect 
outliers <- all_data |> filter(leaf_N_per_dry_mass > 60) # |> bind_rows(outliers)

summarize_cont(all_data, leaf_N_per_dry_mass)

all_data |> histogram(leaf_P_per_dry_mass, 80) # P = 9.99 is weird 
outliers <- all_data |> filter(leaf_P_per_dry_mass > 9) |> bind_rows(outliers)

summarize_cont(all_data, leaf_P_per_dry_mass)

all_data |> histogram(leaf_C_per_dry_mass, 80) # C = 678, 195 (Wills), both Dong, & 235 weird 
outliers <- all_data |> filter(leaf_C_per_dry_mass > 650) |> bind_rows(outliers)
outliers <- all_data |> filter(leaf_C_per_dry_mass < 250) |> bind_rows(outliers)

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

# Fiona recommends NPP / AET investigation 
all_data |> histogram(MAT, 80) 
all_data |> histogram(NPP, 20) 
all_data |> histogram(AET, 50) 

# Co-variation 
# Pearson Correlation Matrix (could also do Kendall or Spearman coeffs.)
corr_matrix <- cont_data |> 
  select(!c(Unique_ID, NP_ratio, CN_ratio, CP_ratio)) |> 
  mutate(lat_deg = abs(lat_deg)) |> # Note abs(lat) - should this pass to all data? 
  cor(use = 'pairwise.complete.obs')  # complete.obs / na.or.complete / pairwise.complete.obs

corr_matrix |> corrplot(method = 'ellipse', tl.col = 'black')

# Iterate through matrix to pick out non-correlated combinations ? 
# TBD 

