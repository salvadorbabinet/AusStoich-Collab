library(tidyverse)
library(dplyr)
library(ape)
library(ggtree)
library(tidytree)
library(treeio)
library(phytools)
library(V.PhyloMaker2)
library(httpgd)


aus_data <- aus_data

httpgd::hgd() #plot viewer
hgd_browse()

##----------------------------Function Definitions-----------------------------
#---------for parsing through aus_data
#input: dataframe in aus_data format, with only species of interest selected

#function for selecting relevant categorical and nutrient columns from aus_data
select_relevant_columns <- function(df) {
  selected_columns_df <- df[,c("species_binom", "family", "genus",
                               "woodiness", "reclass_life_history",
                               "putative_BNF", "myc_type",
                               "leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                               "leaf_C_per_dry_mass", "NP_ratio", "CN_ratio", "CP_ratio")]
  return(selected_columns_df)
}


#function for adding coefficient of variation column
#for any dataframe
add_CV_columns <- function(df) {
  CV_added_df <- df %>%
    group_by(species_binom) %>%
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

#function for calculating geometric mean
#automatically excludes all NAs
#important note: use this function only for untransformed data
#to get geometric mean of ln(ratio) data, use normal mean() function
#this will give same result
geometric_mean <- function(x) {
  exp(mean(log(x), na.rm = TRUE))
}

#function for averaging nutrient data, once covariance is added
#also average ratios, arithmetically and geometrically
#note that only transformed ratios should be averaged geometrically
average_nutrient_data <- function(df) {
  nutrient_averaged_df <- df %>%
    group_by(species_binom) %>%
    summarize(
      avg_leaf_N = mean(leaf_N_per_dry_mass, na.rm = TRUE),
      avg_leaf_C = mean(leaf_C_per_dry_mass, na.rm = TRUE),
      avg_leaf_P = mean(leaf_P_per_dry_mass, na.rm = TRUE),
      avg_ar_NP_ratio = mean(NP_ratio, na.rm = TRUE),
      avg_ar_CN_ratio = mean(CN_ratio, na.rm = TRUE),
      avg_ar_CP_ratio = mean(CP_ratio, na.rm = TRUE),
      avg_geo_NP_ratio = geometric_mean(NP_ratio),
      avg_geo_CN_ratio = geometric_mean(CN_ratio),
      avg_geo_CP_ratio = geometric_mean(CP_ratio),
      # Keep all columns, without this will get rid of the rest
      across(-c(leaf_N_per_dry_mass, leaf_C_per_dry_mass, leaf_P_per_dry_mass,
                NP_ratio, CN_ratio, CP_ratio),
             ~ first(.), .names = "{.col}")
    ) %>%
    ungroup() %>%
    # Relocate all "avg_"columns to the right
    relocate(starts_with("avg_"), starts_with("geo_"), .after = last_col())
  
  return(nutrient_averaged_df)
}
#avg = NaN means all entries for that species NA
#species with one observation will have same value 




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


#all pos sp data entry
austraits_all_pos_sp_tree<- read.tree("Inputs/Trees/austraits_all_pos_sp.tre")

austraits_all_pos_sp_df <- read_csv('Inputs/all_pos_austraits_LCVP_sp.csv') #831

#-----all_pos_sp_all_data derivation

all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                              austraits_all_pos_sp_df$species, ]
length(unique(all_pos_sp_data$species_binom)) #831


all_pos_sp_data <- select_relevant_columns(all_pos_sp_data)


all_pos_sp_data <- add_CV_columns(all_pos_sp_data)


#------end of all_pos_sp_all derivation

#-----avg_all_pos_sp_all_data derivation

avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data)
length(unique(avg_all_pos_sp_data$species_binom)) #831


#-----end of avg_all_pos_sp_all_data derivation


# pruned tree data entry ----

auspruned_three_tree <-read.tree(here("Inputs/Trees/austraits_pruned_three.tre"))

#function in tree derivation + 01
pruned_three_prepped <- prune_prep_tree(pruned_ausdata_three)

#need pruned ausdata, derived in 001
pruned_ausdata_three
length(unique(pruned_ausdata_three$species_binom)) #473 

pruned_three_data <- select_relevant_columns(pruned_ausdata_three)
pruned_three_data <- add_CV_columns(pruned_ausdata_three)

#avg
avg_pruned_three_data <- average_nutrient_data(pruned_three_data)

#----end of pruned tree data entry


# no gymnosperm tree data entry
nogymn_tree <-read.tree(here("Inputs/Trees/no_gymnosperm_tree.tre"))
ausdata_no_gymn #from 01
ausdata_no_gymn <- select_relevant_columns(ausdata_no_gymn)
ausdata_no_gymn <- add_CV_columns(ausdata_no_gymn)

avg_no_gymn <- average_nutrient_data(ausdata_no_gymn)
# end


# ausdata tree data entry 
ausdata_tree
aus_data
ausdata_nut <- add_CV_columns(select_relevant_columns(aus_data))

avg_ausdata <- average_nutrient_data(ausdata_nut)
# end


# ITS tree data entry ----

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


# Plots ----

#horizontal base
all_pos_sp_plot <- ggtree(austraits_all_pos_sp_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = avg_leaf_C),
  orientation = "y") +
  ggtitle("avg leaf C") +
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

p <- ggtree(ausdata_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
p + geom_facet(
  panel = "Trait",
  data = avg_ausdata,
  geom = geom_col,
  mapping = aes(x = avg_leaf_C),
  orientation = "y") +
  ggtitle("avg leaf C") +
  theme(plot.title = element_text(size = 20))

#-------------------------------------------------------------------------------



#------------------------Phylogenetic Signal------------------------------------
#trait data must be in same order as label in tree

aus_all_pos_sp_tree_tib <- as_tibble(austraits_all_pos_sp_tree)
aus_all_pos_sp_tree_tib_sig <- add_relevant_columns(aus_all_pos_sp_tree_tib,
                                                     avg_all_pos_sp_data)

pruned_three_tree_tib <- as_tibble(auspruned_three_tree)
pruned_three_tree_tib <- add_relevant_columns(pruned_three_tree_tib,
                                              avg_pruned_three_data)

ITS_tree_tib <- add_relevant_columns(ITS_tree_tib, avg_ITS_sp_data)
#cut at row 106, keep 105

nogymn_tree_tib <- as_tibble(nogymn_tree)
nogymn_tree_tib <- add_relevant_columns(nogymn_tree_tib, avg_no_gymn)

#ausdata
ausdata_tree_tib <- as_tibble(ausdata_tree)
#derived avg nutrient data df
ausdata_tree_tib <- add_relevant_columns(ausdata_tree_tib, avg_ausdata)

# 1. Pick tree, input as string. Options:
# "ITS_tree", cut = 105
# "austraits", cut = 831
# "pruned_three", cut = 473
# "ausdata", cut = 1414 
# Note that cut is inclusive i.e. up to and including

tree_tib <- "nogymn"

if (tree_tib == "ITS_tree") {
  cut = 105
  tree_tib = ITS_tree_tib
  tree = ITS_tree
}

if (tree_tib == "nogymn") {
  cut = 1403
  tree_tib = nogymn_tree_tib
  tree = nogymn_tree
}

if (tree_tib== "ausdata") {
  cut = 1414
  tree_tib = ausdata_tree_tib
  tree = ausdata_tree
}


#what was prev. though to be all pos. sp.
if (tree_tib == "austraits") {
  cut = 831
  tree_tib = aus_all_pos_sp_tree_tib_sig
  tree = austraits_all_pos_sp_tree
}

if (tree_tib == "pruned_three") {
  cut = 473
  tree_tib = pruned_three_tree_tib
  tree = auspruned_three_tree
}


# 2. Write in trait of interest as string. Options:
# avg_leaf_N
# avg_leaf_C
# avg_leaf_P
# CV_N
# CV_P
# CV_C
# avg_ar_NP_ratio
# avg_ar_CN_ratio
# avg_ar_CP_ratio
# avg_geo_NP_ratio
# avg_geo_CN_ratio
# avg_geo_CP_ratio

trait <- "avg_leaf_N"

# 3. extract_trait_values() on tree tib to get values of interest
trait_data <- extract_trait_values(tree_tib, "label",
                                   trait, cut)

# 4. Get signals
K_signal <- phylosig(tree, trait_data,
                     method = "K", nsim = 10000)
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