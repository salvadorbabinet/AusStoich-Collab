# AusStoich Exploratory Data Analysis 
# Libraries & functions 
library(here)
library(rlang)
library(tidyverse)

histogram <- function(data, variable, bins = NULL, ylim = NULL) {
  label <- englue('{{variable}}')
  
  ggplot(data, aes(x = {{variable}})) + 
    geom_histogram(bins = bins) + 
    coord_cartesian(ylim = ylim)
}

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

# Finding outliers 
all_data |> count(across(woodiness:putative_BNF))
all_data |> filter(woodiness == 1 & reclass_life_history == 'short') |> 
  distinct(species_binom) 

all_data |> histogram(leaf_N_per_dry_mass, 80)
outliers <- all_data |> filter(leaf_N_per_dry_mass > 60) |> bind_rows(outliers)

all_data |> histogram(leaf_P_per_dry_mass, 80)
outliers <- all_data |> filter(leaf_P_per_dry_mass > 9) |> bind_rows(outliers)

all_data |> histogram(leaf_C_per_dry_mass, 80) 
outliers <- all_data |> filter(leaf_C_per_dry_mass > 650) |> bind_rows(outliers)
