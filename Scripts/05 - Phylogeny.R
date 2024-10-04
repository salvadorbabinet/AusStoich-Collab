library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(phytools)
library(V.PhyloMaker2)

aus_data <- aus_data

##----------------------------Function Definitions-----------------------------
#---------for parsing through aus_data
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
#for any dataframe
add_CV_columns <- function(df) {
  CV_added_df <- df %>%
    group_by(species_binom) %>% #need to rewrite, not getting same value by hand
    #grouping by species should solve the issue idk where the number is coming from....
    mutate(CV_N = sd(leaf_N_per_dry_mass, na.rm = TRUE) / mean(leaf_N_per_dry_mass,
                                                               na.rm = TRUE),
           CV_P = sd(leaf_P_per_dry_mass, na.rm = TRUE) / mean(leaf_P_per_dry_mass,
                                                               na.rm = TRUE),
           CV_C = sd(leaf_C_per_dry_mass, na.rm = TRUE) / mean(leaf_C_per_dry_mass,
                                                               na.rm = TRUE)) %>%
    ungroup()
  return(CV_added_df) 
}
#CV = NA can mean only one entry per that species
#CV = 0 means no variation for that species


#function for averaging nutrient data only
average_nutrient_data_v1 <- function(df) {
  nutrient_averaged_df <- df %>%
    group_by(species_binom) %>%
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass, na.rm = TRUE),
      avg_leaf_C = mean(leaf_C_per_dry_mass, na.rm = TRUE),
      avg_leaf_P = mean(leaf_P_per_dry_mass, na.rm = TRUE),
    ) %>%
    ungroup()
  return(nutrient_averaged_df)
  #returns df of just species identity and associated leaf concentration
} 


#function for averaging nutrient data, once covariance is added
average_nutrient_data <- function(df) {
  nutrient_averaged_df <- df %>%
    group_by(species_binom) %>%
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass, na.rm = TRUE),
      avg_leaf_C = mean(leaf_C_per_dry_mass, na.rm = TRUE),
      avg_leaf_P = mean(leaf_P_per_dry_mass, na.rm = TRUE),
      #Keep all columns:
      across(-c(leaf_N_per_dry_mass, leaf_C_per_dry_mass, leaf_P_per_dry_mass),
             ~ first(.), .names = "{.col}") 
    ) %>%
    ungroup() %>%
    select(-avg_leaf_N, -avg_leaf_C, -avg_leaf_P,
           everything(), avg_leaf_N, avg_leaf_C, avg_leaf_P) #move columns to end
  
  return(nutrient_averaged_df)
}
#avg = NaN means all entries for that species NA 


#--------- merging tree tib with trait data
add_relevant_columns <- function(tree_tib, avg_sp_data) {
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))

  return(merged_tib)
}


extract_trait_values <- function(tree_tib, label_col, trait_col, cut) {
  # tree_tib: tree tibble object with associated trait data
  # label_col: name of the column that contains name of tip.labels from tree
  # trait_col: name of the column that has trait value of interest
  # cut: number of rows to keep from tree_tib
  
  # Cut the tibble to the specified number of rows
  cut_tree_tib <- tree_tib %>%
    slice(1:cut) #to ensure vector only includes nutrient values, not extra node info
  
  labels <- cut_tree_tib[[label_col]]
  traits <- cut_tree_tib[[trait_col]]
  
  trait_values <- setNames(as.numeric(traits), labels)
  #return named numeric vector, of column of interest in the order 
  #of input of tree_tib
  return(trait_values)
}
#-------------------------------------------------------------------------------



#------------------------------Data Entry---------------------------------------

austraits_all_pos_sp_tree<- read.tree("Inputs/Trees/austraits_all_pos_sp.tre")


austraits_all_pos_sp_df <- read_csv('Inputs/all_pos_austraits_LCVP_sp.csv') #829

#-----all_pos_sp_all_data derivation

all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                              austraits_all_pos_sp_df$species, ]
length(unique(all_pos_sp_data$species_binom)) #829


all_pos_sp_data <- select_relevant_columns(all_pos_sp_data)


all_pos_sp_data <- add_CV_columns(all_pos_sp_data)


#------end of all_pos_sp_all derivation

#-----avg_all_pos_sp_all_data derivation

avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data)
length(unique(avg_all_pos_sp_data$species_binom)) #829


#-----end of avg_all_pos_sp_all_data derivation


#-----ITS tree data entry


ITS_tree <- read.nexus("Inputs/Trees/ITS_tree.tre") 

ITS_tree_tib <- as_tibble(ITS_tree)
#need to remove irrelevant columns and add trait values 

ITS_sp_data <- aus_data[aus_data$species_binom %in%
                              ITS_tree_tib$label, ]

ITS_sp_data <- select_relevant_columns((ITS_sp_data))

ITS_sp_data <- add_CV_columns(ITS_sp_data)

#average 
avg_ITS_sp_data <- average_nutrient_data(ITS_sp_data)

#-----ITS tree data entry end


#-------------------------------------------------------------------------------


#---------------------------------Plots-----------------------------------------

#horizontal base
all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = CV_C),
  orientation = "y")+  
  ggtitle("CV C") +
  theme(plot.title = element_text(size = 20))


#circular base
all_pos_sp_circular_plot <- ggtree(austraits_all_pos_sp_tree, layout = "circular",
                                   branch.length = "none")+ ggtitle("All Pos. Sp.")

#most basic, no coloring circular bar plot                                
all_pos_sp_circular_plot + geom_fruit(
  data = avg_all_pos_sp_data,
  geom = geom_bar,
  mapping = aes(x = avg_leaf_N, y = species_binom),
  orientation = "y",
  stat = "identity") + ggtitle("Average Leaf N")


ggtree(ITS_tree) + geom_tiplab(size = 1.1) +
  geom_facet(
    panel = 'Trait',
    data = avg_all_pos_sp_data,
    geom = geom_col,
    mapping = aes(x = avg_leaf_C),
    orientation = "y")  + ggtitle("Average Leaf C")


#-------------------------------------------------------------------------------



#------------------------Phylogenetic Signal------------------------------------
#trait data must be in same order as label in tree

aus_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)
aus_all_pos_sp_tree_tib_sig <- add_relevant_columns(aus_all_pos_sp_tree_tib,
                                                     avg_all_pos_sp_data)

ITS_tree_tib <- add_relevant_columns(ITS_tree_tib, avg_ITS_sp_data)
#cut at row 106, keep 105


# 1. Pick tree, input as string. Options:
# "ITS_tree", cut = 105
# "austraits", cut = 830

tree_tib <- "austraits" 

if (tree_tib == "ITS_tree") {
  cut = 105
  tree_tib = ITS_tree_tib
  tree = ITS_tree
}


if (tree_tib == "austraits") {
  cut = 830
  tree_tib = aus_all_pos_sp_tree_tib_sig
  tree = austraits_all_pos_sp_tree
}

# 2. Write in trait of interest as string. Options:
# avg_leaf_N
# avg_leaf_C
# avg_leaf_P
# CV_N
# CV_P
# CV_C

trait <- "CV_C"

# 3. extract_trait_values() on tree tib to get values of interest
trait_data <- extract_trait_values(tree_tib, "label", 
                                   trait, cut)

# 4. Get signals
K_signal <- phylosig(tree, trait_data,
                     method = "K", nsim = 10) 
print(K_signal)
#note that number doesn't change depending on nsim

lambda <- phylosig(tree, trait_data,
                   method = "lambda") 
print(lambda)
#---------------------------------Testing---------------------------------------

m <- left_join(aus_all_pos_sp_tree_tib, avg_all_pos_sp_data, by = c("label" = "species_binom"))

add_relevant_columns <- function(tree_tib, avg_sp_data) {
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  
  return(merged_tib)
}

n <- add_relevant_columns(aus_all_pos_sp_tree_tib, avg_all_pos_sp_data)