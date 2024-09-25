library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)

##########---------Data Import---------##########
#from 02 Data Import
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

#LCVP name standardization - derivation in phylogeny script
naming_corrections <- read_csv(here('Inputs', 'all_naming_corrections.csv'))

aus_data <- all_data %>%
  left_join(naming_corrections, by = c("species_binom" = "species_before_correction",
                                       "genus" = "genus_before_correction",
                                       "family" = "family_before_correction")) %>%
  mutate(
    species_binom = ifelse(!is.na(species_after_correction), species_after_correction, species_binom),
    genus = ifelse(!is.na(genus_after_correction), genus_after_correction, genus),
    family = ifelse(!is.na(family_after_correction), family_after_correction, family)
  ) %>%
  select(-species_after_correction, -genus_after_correction, -family_after_correction) 

#remove outliers from continuous traits as well as structure analysis
rm(all_data)
rm(naming_corrections)

##########---------austraits_all_pos_sp.tre derivation---------##########
austraits_all_pos_sp_df <- read_csv(here('Inputs', 'Supplemental Inputs - Sofia',
                                         'all_pos_austraits_LCVP_sp.csv'))

austraits_all_pos_sp <- phylo.maker(sp.list = austraits_all_pos_sp_df,
                                    tree = GBOTB.extended.LCVP,
                                    nodes = nodes.info.1.LCVP,
                                    scenarios="S3")
#with this object can write .tre file, however it is already in Inputs
#write.tree(austraits_all_pos_sp$scenario.3,
          # "Inputs/Trees/austraits_all_pos_sp.tre")

austraits_all_pos_sp_tree<- read.tree(here("Inputs/Trees/austraits_all_pos_sp.tre"))
#plot(austraits_all_pos_sp_tree, cex= 0.1)

##########---------austraits_all_pos_sp.tre plots---------##########
library(ggtree)
library(tidytree)
library(treeio)

austraits_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)


#start of all_pos_sp_all tib derivation
all_pos_sp_all_data <- aus_data[all_corrected_data$species_binom %in%
                                            austraits_all_pos_sp_df$species, ]
length(unique(all_pos_sp_all_data$species_binom)) #829 so ok

all_pos_sp_all_data <- all_pos_sp_all_data[,c("species_binom", "family", "genus",
                                              "woodiness", "reclass_life_history",
                                              "putative_BNF", "myc_type",
                                              "leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                                              "leaf_C_per_dry_mass", "NP_ratio",
                                              "CN_ratio", "CP_ratio")]

all_pos_sp_all_data <- all_sp_nut_data %>%
  group_by(species_binom) %>%
  mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                             na.rm = TRUE),
         CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                             na.rm = TRUE),
         CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                             na.rm = TRUE))

avg_all_pos_sp_all_data <- aggregate(. ~ species_binom, data = all_pos_sp_all_data, FUN = mean)
#end of all_pos_sp_all tib derivation




p <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab() 
#can xlim(0.1) if needed
plot(p)

ggtree(austraits_all_pos_sp_tree,layout='circular')
ggtree(austraits_all_pos_sp_tree, branch.length = "none",
       layout = "circular") + geom_tiplab(size = 0.7) + ggtitle("All Possible Species in tips.info.LCVP")
ggtree(austraits_all_pos_sp_tree, branch.length = "none",
       layout = "circular") + geom_nodelab()




##########---------austraits_one_rep_per_gen.tre & genera lost---------##########

austraits_one_rep_per_gen_tree<- read.tree(here("Inputs/Trees/austraits_one_rep_per_gen.tre"))
#derivation of this .tre in supplemental scripts - LCVP & early phylogeny
#ensure ichnocarpus is in here

plot(austraits_one_rep_per_gen_tree, cex= 0.1)
ggtree(austraits_one_rep_per_gen_tree, branch.length = "none",
       layout = "circular") + geom_tiplab(size = 2) + ggtitle("One Rep Per Genera in tips.info.LCVP")
