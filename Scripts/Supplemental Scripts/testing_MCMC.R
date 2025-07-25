# This script is intended for running exclusively on the lab computer, should be kept clean at all times. 
# Repositories downloaded from Git as zip files, labelled with dates.
# Git Connection Non-Existent on lab computer.
# Eventually this script will be automated for all analyses.
# DO NOT MODIFY THIS SCRIPT LOCALLY ON LAB COMPUTER!!!!

# Script includes:
# Data Import - aus_data + tree
# Data cleaning
# Model run through


#Script should include:
# Distributions of dependent
# Feature selection
# Diagnostic plots
# Implementation of categorical
# and more..

library(ape)
library(MCMCglmm)
library(ggtree)
library(tictoc)
library(dplyr)

aus_data <- aus_data #from 02-Data-Import script

ausdata_all_pos_sp_tree <- read.tree("Inputs/Trees/ausdata_all_pos_sp.tre")

is.ultrametric(ausdata_all_pos_sp_tree)

ausdata_all_pos_sp_tree$node.label <- NULL 

phylo_inv <- inverseA(ausdata_all_pos_sp_tree, nodes ="TIPS",scale=TRUE)

prior_phylo <- list(
  G = list(
    G1 = list(V = 1, nu = 0.02), #for phylo
    G2 = list(V = 1, nu = 0.02)  #for species
  ),
  R = list(V = 1, nu = 0.02) #residual 
)

aus_data$phylo <- aus_data$species_binom

ausdata_all_pos_sp <- aus_data %>% 
  filter(species_binom %in% ausdata_all_pos_sp_tree$tip.label)

ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo)
ausdata_all_pos_sp$species_binom <- factor(ausdata_all_pos_sp$species_binom)

ausdata_all_pos_sp$phylo <- factor(ausdata_all_pos_sp$phylo,
                                   levels = rownames(phylo_inv$Ainv))

ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)

ausdata_all_pos_sp <- ausdata_all_pos_sp %>%
  rename(fam = family)
ausdata_all_pos_sp$family <- NULL

ausdata_all_pos_sp <- as.data.frame(ausdata_all_pos_sp)


tic("model run")
model <- MCMCglmm(ln_NP_ratio ~  SN_total_0_30 + SP_total_0_30 + SOC_total_0_30 +
                    + CEC_total_0_30 + AP_total_0_30 +
                    + NPP + MAT + PPT + AET +
                    + precipitation_seasonality + temp_seasonality,
                  random = ~ phylo +species_binom, #columns in data
                  family = "gaussian",
                  ginverse = list(phylo = phylo_inv$Ainv), prior = prior_phylo,
                  data = ausdata_all_pos_sp, nitt = 500000, burnin = 10000, thin = 1)
toc()
summary(model)
summary(model)$solutions
plot(model$Sol)
autocorr.plot(model$Sol)