# Missing data investigation
# Libraries & functions ----
library(here)
library(tidyverse)


# Unexpected missing entries ----
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


# Are missing data systematic? ----
# Sofia visualizations here
