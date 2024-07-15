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

# Standardize species names with LCVP 

