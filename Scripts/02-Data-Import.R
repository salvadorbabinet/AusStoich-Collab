# AusStoich Exploratory Data Analysis 
# Libraries & functions 
library(here)
library(tidyverse)

# Data import (following structure adjustments from 01)
all_data <- read_csv(
  file = here('Inputs', 'AusStoich_merged_final.csv'),
  na = c('', 'NA', '#N/A','uncertain'),
  col_types = cols(
    woodiness = col_factor(c('0', '1')),
    reclass_life_history = col_factor(c('short', 'long')),
    putative_BNF = col_factor(c('0', '1')),
    myc_type = col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
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

rm(naming_corrections)
