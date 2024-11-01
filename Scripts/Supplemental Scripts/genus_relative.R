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


#want to see how many genera are in families who include missing genera
#to see if its worth adding a relative
check <- gen_notin_LCVP %>%
  # Get unique families from missing genera
  distinct(family) %>%
  # Join with the count of genera for each family in tips.info.LCVP
  left_join(
    tips.info.LCVP %>%
      group_by(family) %>%
      summarise(lcvp_genera_count = n_distinct(genus)),
    by = "family"
  ) %>%
  # Sort by the number of genera in LCVP in descending order
  arrange(desc(lcvp_genera_count))
#output of this in excel to possibly get genus relatives