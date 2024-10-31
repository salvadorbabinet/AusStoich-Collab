# AusStoich Data Join Validation
# Libraries & Functions ----
library(here)
library(tidyverse)


# Validate join ----
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

# Join data by rounding
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
comparison_data <- joined_data_equality |> relocate(lat_deg:long_deg, .after = dataset_id) 

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

mismatch_selection <- mismatch |> 
  group_by(lat_deg) |> 
  slice_head() |> 
  select(Unique_ID, dataset_id, lat_deg, long_deg, leaf_N_per_dry_mass, SN_total_0_30, SP_total_0_30, NPP, MAT, precipitation, temp_seasonality)
comparison_selection <- comparison_data |> 
  filter(Unique_ID %in% mismatch_selection$Unique_ID) |> 
  select(Unique_ID, dataset_id, lat_deg, long_deg, leaf_N_per_dry_mass, SN_total_0_30, SP_total_0_30, NPP, MAT, precipitation, temp_seasonality)
