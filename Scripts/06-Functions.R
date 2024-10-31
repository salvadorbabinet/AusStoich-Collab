library(dplyr)
library(tidyverse)
library(here)


# For parsing through aus_data

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
}