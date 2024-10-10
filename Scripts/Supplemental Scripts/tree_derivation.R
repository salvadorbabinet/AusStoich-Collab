library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(V.PhyloMaker2)


#---------austraits_all_pos_sp.tre derivation---------
austraits_all_pos_sp_df <- read_csv(here('Inputs',
                                         'all_pos_austraits_LCVP_sp.csv'))

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")
#with this object can write .tre file, however it is already in Inputs
#write.tree(austraits_all_pos_sp$scenario.3, "Inputs/Trees/austraits_all_pos_sp.tre")

austraits_all_pos_sp_tree<- read.tree(here("Inputs/Trees/austraits_all_pos_sp.tre"))


#---------pruned_aus_data tree derivation---------

prune_prep_tree <- function (df) {
  #This function will take a pruned aus_data object
  #and create a new df in format necessary to create tree
  #using phylo.maker
  #Note: shouldn't use this for aus_data only, as names have not been standardized
  
  pruned_prep <- df %>%
    distinct(species_binom, genus, family) %>%
    mutate(
      species.relative = NA,
      genus.relative = NA
    ) %>%
    rename(species = species_binom)
  
  return(pruned_prep)
} #works as intended

#test writing function   
test_prune_prep <- pruned_ausdata_three%>%
  distinct(species_binom, genus, family) %>%
  mutate(
    species_relative = NA,  # Add empty column for species.relative
    genus_relative = NA     # Add empty column for genus.relative
  ) %>%
  rename(species = species_binom) 
  
#species with less than or equal to three entries
pruned_three_prepped <- prune_prep_tree(pruned_ausdata_three)
  
  
pruned_three_tree <-phylo.maker(sp.list = pruned_three_prepped,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")

write.tree(pruned_three_tree$scenario.3, "Inputs/Trees/austraits_pruned_three.tre")

auspruned_three_tree <-read.tree(here("Inputs/Trees/austraits_pruned_three.tre"))


#--------austraits_one_rep_per_gen.tre & genera lost---------

austraits_one_rep_per_gen_tree<- read.tree(here("Inputs/Trees/austraits_one_rep_per_gen.tre"))
#derivation of this .tre in supplemental scripts - LCVP & early phylogeny
#ensure ichnocarpus is in here

plot(austraits_one_rep_per_gen_tree, cex= 0.1)
ggtree(austraits_one_rep_per_gen_tree, branch.length = "none",
       layout = "circular") + geom_tiplab(size = 2) + ggtitle("One Rep Per Genera in tips.info.LCVP")
