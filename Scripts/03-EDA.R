# AusStoich Exploratory Data Analysis 
# Libraries & functions ---------------------------------------------------
library(here)
library(corrplot)
library(tidyverse)

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

# WIP -- do not call 
# closer_inequality <- function(x, y) {
#   x_arranged <- arrange(x) 
#   y_arranged <- arrange(y) 
#   ifelse()
# }


# Data join check ---------------------------------------------------------
# Create data objects 
trait_data <- read_csv(
  file = here('Inputs', 'Old', 'austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv'),
  na = c('', 'NA', '#N/A','uncertain')
  )
trait_data_clean <- trait_data |> 
  select(Unique_ID:CP_ratio) |> 
  filter(!is.na(Unique_ID)) |> 
  relocate(species_binom, .after = genus) |> 
  relocate(lat_deg:long_deg, .before = Unique_ID) |> 
  arrange(lat_deg)

env_data <- read_csv(
  file = here('Inputs', 'Old', 'envdata.csv'),
  na = c('', 'NA', '#N/A','uncertain')
  )
env_data_clean <- env_data |> rename(
  lat_deg = lat, 
  long_deg = lon) |> 
  select(lat_deg:AET) |> 
  arrange(lat_deg)

clim_data <- read_csv(
  file = here('Inputs', 'Old', 'AusStoich_Seasonality_WorldClim30s.csv'),
  na = c('', 'NA', '#N/A','uncertain')
  )
clim_data_clean <- clim_data |> 
  rename(lat_deg = `latitude (deg)`, long_deg = `longitude (deg)`) |> 
  arrange(lat_deg)

rm(trait_data, env_data, clim_data)

# Verify keys 
trait_data_clean |> count(lat_deg, long_deg) # 358 combinations 
trait_data_clean |> filter(is.na(lat_deg) | is.na(long_deg)) 

env_data_clean |> count(lat_deg) |> filter(n > 1) 
env_data_clean |> count(lat_deg, long_deg) |> filter(n > 1) 

# Join data... 
# By closest lower value... what about if higher value is closer? 
# merged_data_rolling <- trait_data_clean |> left_join(
#   env_data_clean, 
#   join_by(closest(lat_deg >= lat_deg), closest(long_deg >= long_deg))
#   ) |> 
#   relocate(lat_deg.y:long_deg.y, .after = long_deg.x)

# Round then join 
# Round latitude & longitude to match environment / climate data 
trait_data_clean <- trait_data_clean |> 
  mutate(
    lat_deg = round(lat_deg, digits = 4), 
    long_deg = round(long_deg, digits = 3)
    )

double_lat <- trait_data_clean |> filter(lat_deg %in% c(-33.3718, -30.2766, -30.2406, -16.0072))

clim_data_clean <- clim_data_clean |> mutate(long_deg = round(long_deg, digits = 3))
clim_data_clean |> filter(if_any(PrecipSeasonality_WorldClim30s1:TempSeasonality_WorldClim30s1, is.na))

env_clim_join <- env_data_clean |> left_join(clim_data_clean)
rounding_mismatch <- env_clim_join |> filter(if_any(PrecipSeasonality_WorldClim30s1:TempSeasonality_WorldClim30s1, is.na)) 
# long_deg rounded up by .001 from climate equivalents 

# round up these values and join again 
clim_data_rounded <- clim_data_clean |> semi_join(rounding_mismatch, join_by(lat_deg)) |>
  filter(!is.na(PrecipSeasonality_WorldClim30s1)) |>
  mutate(long_deg = long_deg + 0.001)

env_clim_join <- env_clim_join |> left_join(clim_data_rounded, join_by(lat_deg, long_deg))

env_clim_join <- env_clim_join |> 
  mutate(
    PrecipSeasonality_WorldClim30s1.x = if_else(
      is.na(PrecipSeasonality_WorldClim30s1.x),
      PrecipSeasonality_WorldClim30s1.y, 
      PrecipSeasonality_WorldClim30s1.x
    )
  )|> 
  mutate(
    TempSeasonality_WorldClim30s1.x = if_else(
      is.na(TempSeasonality_WorldClim30s1.x),
      TempSeasonality_WorldClim30s1.y, 
      TempSeasonality_WorldClim30s1.x
    )
  ) |> 
  select(!c(PrecipSeasonality_WorldClim30s1.y, TempSeasonality_WorldClim30s1.y)) |> 
  rename(precipitation = PrecipSeasonality_WorldClim30s1.x, temp_seasonality = TempSeasonality_WorldClim30s1.x)

rm(env_data_clean, clim_data_clean, rounding_mismatch, clim_data_rounded)

# First merge unique latitude keys 
trait_data_single_lat <- trait_data_clean |> anti_join(double_lat, join_by(lat_deg))
joined_data_equality <- trait_data_single_lat |> 
  left_join(env_clim_join, join_by(lat_deg)) |> 
  select(!long_deg.y) |> 
  rename(long_deg = long_deg.x)

# Mismatched latlongs 
# mismatch <- merged_data_equality |> filter(long_deg.x != long_deg.y) |> 
#   relocate(long_deg.y, .after = long_deg.x) # Rounded long_deg.y is 0.001 above long_deg.x in all cases 

# Then add double latitude keys by longitude 
double_lat <- double_lat |> left_join(env_clim_join)
joined_data_equality <- joined_data_equality |> 
  bind_rows(double_lat) |> 
  arrange(lat_deg)

rm(trait_data_clean, trait_data_single_lat, double_lat, env_clim_join)

# Check for missing values due to merge error (TD: turn into a function)
merge_miss <- joined_data_equality |> filter(if_any(SN_total_0_30:precipitation, is.na))
merge_miss |> distinct(lat_deg, long_deg) # Returns expected Esperon Rodriguez / Hayes env data and missing seasonality data (Schulze)

# Compare to manual merge 
comparison_data <- joined_data_equality |> relocate(lat_deg:long_deg, .after = dataset_id) # |> 
#   rename(
#     precipitation = PrecipSeasonality_WorldClim30s1,
#     temp_seasonality = TempSeasonality_WorldClim30s1
#   )

all_data <- read_csv(
  file = here('Inputs', 'ausdata_merged_v3_SQ.csv'),
  na = c('', 'NA', '#N/A','uncertain')
  )

all_data <- all_data |> 
  relocate(lat_deg:long_deg, .after = dataset_id) |> 
  relocate(species_binom, .after = genus) 

all_data
comparison_data # Some NAs in climate data merge 

mismatch <- all_data |> # Returns rows in manual data join without a match here 
  anti_join(
    comparison_data, # Should figure out how to iterate through all desired columns 
    join_by(leaf_N_per_dry_mass, SN_total_0_30, SP_total_0_30, NPP, MAT, precipitation, temp_seasonality)
  ) 


# Missing data ------------------------------------------------------------
# All entries with unexpected NAs (not C and P or ratios)
missing <- aus_data |> filter(if_any(!leaf_P_per_dry_mass:CP_ratio, is.na))
missing |> # Print unexpected column names with NAs 
  summarize(across(everything(), \(x) sum(is.na(x)))) |> 
  pivot_longer(everything()) |> 
  filter(value > 0) |> 
  print(n = Inf)

# Soil 
missing_soil <- missing |> filter(is.na(SN_total_0_30)) # Hayes_2014 and EsperonRodriguez_2020 
missing_soil |> select(c(observation_id, species_binom, lat_deg:long_deg, SN_total_0_30:AP_total_0_30)) |> 
  print(n = Inf)
aus_data |> filter(dataset_id == 'EsperonRodriguez_2020') # Both datasets have more entries than these NA ones 

# NPP and climate 
missing_climate_npp <- missing |> filter(is.na(NPP)) # All from Hayes_2014 
missing_seasonality |> select(c(observation_id, species_binom, lat_deg:long_deg, NPP:temp_seasonality))
aus_data |> filter(dataset_id == 'Hayes_2014') # But many more Hayes_2014 entries w/ seasonality 


# Categorical variables 
aus_data |> count(across(woodiness:putative_BNF))
aus_data |> filter(woodiness == 1 & reclass_life_history == 'short') |> 
  distinct(species_binom) # Ambiguous, look up species / set as NA accordingly


# Variation ---------------------------------------------------------------
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


# Co-variation ------------------------------------------------------------
# Pearson Correlation Matrix (could also do Kendall or Spearman coeffs.)
corr_matrix <- aus_data |> 
  select(where(is.numeric)) |> 
  select(!c(Unique_ID, NP_ratio, CN_ratio, CP_ratio)) |> 
  mutate(lat_deg = abs(lat_deg)) |> # Note abs(lat) - should this pass to all data? 
  cor(use = 'pairwise.complete.obs')  # complete.obs / na.or.complete / pairwise.complete.obs

corr_matrix
corr_matrix |> corrplot(method = 'ellipse', tl.col = 'black')

# Leaf N by predictors 
# Start with soil N (and visualize by major family)
aus_data |> count(family) |> arrange(desc(n))

aus_data |> filter(SN_total_0_30 < 50) |> 
  ggplot(
    aes(x = SN_total_0_30, 
        y = leaf_N_per_dry_mass,
        color = family %in% c('Myrtaceae', 'Fabaceae', 'Proteaceae')
        )) + 
  geom_point(alpha = 0.4) + 
  labs(color = 'family') +
  theme_bw() 

temp_data <- aus_data |> filter(SN_total_0_30 < 50)

ggplot(mapping = aes(x = SN_total_0_30, y = leaf_N_per_dry_mass)) + 
  geom_point(data = filter(temp_data, !family %in% c('Myrtaceae', 'Fabaceae', 'Proteaceae'))) +  
  geom_point(data = filter(temp_data, family == 'Myrtaceae'), color = 'red') + 
  geom_point(data = filter(temp_data, family == 'Fabaceae'), color = 'cyan') + 
  geom_point(data = filter(temp_data, family == 'Proteaceae'), color = 'orange') +
  theme_bw() 



ggplot(mapping = aes(x = SN_total_0_30, y = leaf_N_per_dry_mass)) + geom_point(data = aus_data |> filter(family == 'Myrtaceae'), color = 'red')
