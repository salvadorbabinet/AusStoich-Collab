# AusStoich Modeling 
# Libraries & functions 
library(here)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# Data import
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

cont_data <- all_data |> select(where(is.numeric)) # |> select(leaf_N_per_dry_mass:temp_seasonality)

