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


# Categorical variables 
all_data |> count(across(woodiness:putative_BNF))
all_data |> filter(woodiness == 1 & reclass_life_history == 'short') |> 
  distinct(species_binom) 


# Variation ---------------------------------------------------------------
all_data |> histogram(leaf_N_per_dry_mass, 80)
outliers <- all_data |> filter(leaf_N_per_dry_mass > 60) |> bind_rows(outliers)

all_data |> histogram(leaf_P_per_dry_mass, 80)
outliers <- all_data |> filter(leaf_P_per_dry_mass > 9) |> bind_rows(outliers)

all_data |> histogram(leaf_C_per_dry_mass, 80) 
outliers <- all_data |> filter(leaf_C_per_dry_mass > 650) |> bind_rows(outliers)


# Co-variation 