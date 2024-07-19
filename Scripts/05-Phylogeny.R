library(here)
library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(ggtreeExtra)

########------------------Data Import------------------########
# Ran 02 - Data import file
aus_data <- aus_data

########------------------Function Definitions------------------###
#for parsing through aus_data
#input: dataframe in aus_data format, with only species of interest selected

#function for selecting relevant categorical and nutrient columns from aus_data
select_relevant_columns <- function(df) {
  selected_columns_df <- df[,c("species_binom", "family", "genus",
                               "woodiness", "reclass_life_history",
                               "putative_BNF", "myc_type",
                               "leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                               "leaf_C_per_dry_mass")]
  return(selected_columns_df)
}

#function for adding coefficient of variation column
add_CV_columns <- function(df) {
  CV_added_df <- df %>%
    group_by(species_binom) %>%
    mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                               na.rm = TRUE),
           CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                               na.rm = TRUE),
           CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                               na.rm = TRUE))
  return(CV_added_df) 
}

#function for averaging nutrient data after adding covariance
average_nutrient_data <- function(df) {
  nutrient_averaged_df <- df %>%
    group_by(species_binom, family, genus, woodiness, reclass_life_history,
             putative_BNF, myc_type, CV_N, CV_P, CV_C) %>%
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass),
      avg_leaf_C = mean(leaf_C_per_dry_mass),
      avg_leaf_P = mean(leaf_P_per_dry_mass),
    )
  return(nutrient_averaged_df)
}

avg_all_sp_data <- average_nutrient_data(add_CV_columns(all_pos_sp_data))

#########-------------austraits_all_pos_sp.tre derivation-------------#########
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

########---------austraits_all_pos_sp.tre plots---------########

#start of all_pos_sp_all_data derivation
all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                                            austraits_all_pos_sp_df$species, ]

all_pos_sp_data <- select_relevant_columns(all_pos_sp_data)
all_pos_sp_data <- add_CV_columns(all_pos_sp_data) #CV = NA can mean only one entry per that species
#end of all_pos_sp_all  derivation

#avg_all_pos_sp_all  derivation
avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data) #831 entries for some reason
#look into this later
length(unique(avg_all_pos_sp_data$species_binom)) #830 so ok

all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) +
  geom_tippoint(data = avg_all_pos_sp_data, mapping = aes(colour = family))


#add family and genus columns function 
add_family_genus <- function(tree_tib, avg_sp_data) {
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  return(merged_tib)
} #has to be used with AVERAGE traits - one entry per species only

#adding relevant columns onto tibble
aus_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)

aus_all_pos_sp_tree_tib <- add_family_genus(aus_all_pos_sp_tree_tib, avg_all_pos_sp_data)
#turn back into phylo class for plotting with tippoint

all_pos_sp_data_tree <- as.phylo(aus_all_pos_sp_tree_tib)
#bug
#https://github.com/YuLab-SMU/treeio/issues/36 

#bug solved manually here, using issues there
class(aus_all_pos_sp_tree_tib) <- c("tbl_tree", class(aus_all_pos_sp_tree_tib))
all_pos_sp_data_tree <- aus_all_pos_sp_tree_tib %>% as.phylo

#-----------Plotting------------#
#horizontal base
all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = avg_leaf_N),
  orientation = "y")  + ggtitle("Average Leaf N")

ggtree(all_pos_sp_data_tree)

#circular base
all_pos_sp_circular_plot <- ggtree(austraits_all_pos_sp_tree, layout = "circular",
     branch.length = "none") + ggtitle("All Pos. Sp.")

#most basic, no coloring circular bar plot                                
all_pos_sp_circular_plot + geom_fruit(
  data = avg_all_pos_sp_data,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N") 


all_pos_sp_circular_plot + geom_fruit(
  data = avg_all_pos_sp_data,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N") +
  geom_tip


#scrap 
p <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab() + xlim_tree(0.1)
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
