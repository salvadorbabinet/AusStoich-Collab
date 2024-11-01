library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(V.PhyloMaker2)


#---------austraits_all_pos_sp.tre derivation---------
austraits_all_pos_sp_df <- read_csv(here('Inputs',
                                         'all_pos_austraits_LCVP_sp.csv'))
#derivation of csv in lcvp_naming_standardization

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")
#with this object can write .tre file, however it is already in Inputs
#write.tree(austraits_all_pos_sp$scenario.3, "Inputs/Trees/austraits_all_pos_sp.tre")

austraits_all_pos_sp_tree<- read.tree(here("Inputs/Trees/austraits_all_pos_sp.tre"))


#---------pruned_aus_data tree derivation---------

#species with less than or equal to three entries
#
pruned_three_prepped <- prune_prep_tree(pruned_ausdata_three)
  
  
pruned_three_tree <-phylo.maker(sp.list = pruned_three_prepped,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")

write.tree(pruned_three_tree$scenario.3, "Inputs/Trees/austraits_pruned_three.tre")

auspruned_three_tree <-read.tree(here("Inputs/Trees/austraits_pruned_three.tre"))


#---------no gymnosperm tree derivation---------
ausdata_no_gymn #from 01

nogymn_prep <- prune_prep_tree((ausdata_no_gymn))
nogymn_tree <- phylo.maker(sp.list = nogymn_prep,
                                               tree = GBOTB.extended.LCVP,
                                               nodes = nodes.info.1.LCVP,
                                                scenarios="S3")

write.tree(nogymn_tree$scenario.3, "Inputs/Trees/no_gymnosperm_tree.tre")

nogymn_tree <-read.tree(here("Inputs/Trees/no_gymnosperm_tree.tre"))

#--------aus_data only---------
aus_data
ausdata_tree <- prune_prep_tree(aus_data)
ausdata_tree <- phylo.maker(sp.list = ausdata_tree,
                         tree = GBOTB.extended.LCVP,
                         nodes = nodes.info.1.LCVP,
                         scenarios="S3")
write.tree(ausdata_tree$scenario.3, "Inputs/Trees/ausdata.tre")
ausdata_tree <- read.tree(here("Inputs/Trees/ausdata.tre"))
