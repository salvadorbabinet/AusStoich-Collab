# AusStoich Phylogeny 
# Libraries & functions 
library(here)
library(tidyverse)
library(ape)

# Import initial phylogeny 
tree <- read.tree(here('Inputs','ITS_tree.tre'))
tree_species <- 
  tibble(tree[["treeTREE1="]][["tip.label"]]) |> 
  rename(species_in_tree = 'tree[["treeTREE1="]][["tip.label"]]')