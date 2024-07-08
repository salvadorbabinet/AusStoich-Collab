# AusStoich Modeling 
# Libraries & functions 
library(here)
library(tidyverse)
library(tidymodels)

# Data import
data <- read_csv(
  file = here('Inputs', 'AusStoich_merged_final.csv'),
  na = c('', 'NA', 'uncertain'),
  col_types = cols(
    woodiness = col_factor(c('0', '1')),
    reclass_life_history = col_factor(c('short', 'long')),
    putative_BNF = col_factor(c('0', '1')),
    myc_type = col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
    )
  )
