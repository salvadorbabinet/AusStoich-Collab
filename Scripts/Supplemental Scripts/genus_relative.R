library(V.PhyloMaker2)
library(ggtree)
library(ape)
library(dplyr)

#using complete aus_data object after naming corrections
#get genera that aren't in tips.info
#find relatives to add and build a new tree 

aus_data

tips.info.LCVP

gen_notin_LCVP <- aus_data %>%
  anti_join(tips.info.LCVP, by = "genus") %>%
  distinct(genus, species_binom, family) #to ensure no duplicates
#34 genera not in megatree

sp_notin_LCVP <- aus_data %>%
  anti_join(tips.info.LCVP, by = c("species_binom" = "species"))%>% 
  distinct(species_binom, genus, family)
#587 

common <- intersect(
  select(gen_notin_LCVP, species_binom),
  select(sp_notin_LCVP, species_binom)
)
#56 species whose genus and species name aren't in megatree



fam_notin_LCVP <- aus_data %>%
  anti_join(tips.info.LCVP, by = "family") 
#%>% distinct(family)
#inspected manually and corrected families, object is currently at 0 (good)