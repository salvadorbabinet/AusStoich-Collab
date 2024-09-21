# AusStoich Exploratory Data Analysis 
# Libraries & functions 
library(here)
library(tidyverse)

# Data import (following structure adjustments from 01)
all_data <- read_csv(
  file = here('Inputs', 'ausdata_merged_v3_SQ.csv'),
  na = c('', 'NA', '#N/A','uncertain'),
  col_types = cols(
    woodiness = readr::col_factor(c('0', '1')),
    reclass_life_history = readr::col_factor(c('short', 'long')),
    putative_BNF = readr::col_factor(c('0', '1')),
    myc_type = readr::col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
  )
)

# LCVP name standardization - derivation in phylogeny script
naming_corrections <- read_csv(here('Inputs', 'all_naming_corrections.csv'))

all_corrected_data <- all_data %>%
  left_join(naming_corrections, by = c("species_binom" = "species_before_correction",
                                           "genus" = "genus_before_correction",
                                           "family" = "family_before_correction")) %>%
  mutate(
    species_binom = ifelse(!is.na(species_after_correction), species_after_correction, species_binom),
    genus = ifelse(!is.na(genus_after_correction), genus_after_correction, genus),
    family = ifelse(!is.na(family_after_correction), family_after_correction, family)
  ) %>%
  select(-species_after_correction, -genus_after_correction, -family_after_correction) 

# Outliers (only Fiona-confirmed, see 03-EDA for all candidates)
outliers <- all_corrected_data |> filter(leaf_N_per_dry_mass > 60) 
outliers <- all_corrected_data |> filter(leaf_P_per_dry_mass > 9) |> bind_rows(outliers)
outliers <- all_corrected_data |> filter(leaf_C_per_dry_mass > 650 | leaf_C_per_dry_mass < 250) |> bind_rows(outliers)
outliers <- all_corrected_data |> filter(is.na(SN_total_0_30)) |> bind_rows(outliers)

outliers_removed_data <- all_corrected_data |> setdiff(outliers)

# Set aus_data to use in subsequent scripts and remove intermediates 
aus_data <- outliers_removed_data |> relocate(species_binom, .before = woodiness)
rm(all_data, outliers, naming_corrections, all_corrected_data, outliers_removed_data) 