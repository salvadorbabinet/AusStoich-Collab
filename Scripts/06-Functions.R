library(dplyr)
library(tidyverse)
library(here)



#all of the following require aus_data-formatted input

select_relevant_columns <- function(df) {
  #function for selecting relevant categorical and nutrient columns from aus_data
  #ln ratios not selected since geometric mean calculated from raw values
  selected_columns_df <- df[,c("species_binom", "family", "genus",
                               "woodiness", "reclass_life_history",
                               "putative_BNF", "myc_type",
                               "leaf_N_per_dry_mass", "leaf_P_per_dry_mass", 
                               "leaf_C_per_dry_mass", "NP_ratio", "CN_ratio", "CP_ratio")]
  return(selected_columns_df)
}


add_CV_columns <- function(df) {
  #function for adding coefficient of variation column
  #CV = NA can mean only one entry per that species
  #CV = 0 means no variation for that species
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


geometric_mean <- function(x) {
  #function for calculating geometric mean
  #automatically excludes all NAs
  #important note: use this function only for untransformed data
  #to get geometric mean of ln(ratio) data, use normal mean() function
  #this will give same result
  exp(mean(log(x), na.rm = TRUE))
}


average_nutrient_data <- function(df) {
  #function for averaging nutrient data, once covariance is added
  #also average ratios, arithmetically and geometrically
  #note that only transformed ratios should be averaged geometrically
  
  #avg = NaN means all entries for that species NA
  #species with one observation will have same value 
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



add_tree_traits <- function(tree_tib, avg_sp_data) {
  #merging tree tib with trait data
  #avg_sp_data input = output of average_nutrient_data function
  merged_tib <- left_join(tree_tib, avg_sp_data, by = c("label" = "species_binom"))
  
  return(merged_tib)
}


extract_trait_values <- function(tree_tib, label_col, trait_col, cut) {
  # trait data must be in same order as label in tree
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


prune_ausdata <- function(df, m) {
  #This function creates a species observation object
  #Uses the list it generates to get names of species that occur at
  #certain frequencies in our data, and uses said list to remove
  #all entries in aus_data that whose names are in list
  #Unidirectional condition! Will only remove species LESS THAN or equal to m
  
  #Create species observation object:
  #Freq, species_count, and list of species associated with Freq
  sp_obs <- df %>%
    count(species_binom) %>%
    rename(Freq = n) %>%
    arrange(desc(Freq)) %>%
    group_by(Freq) %>%
    summarize(
      species_count = n(),
      species_list = list(toString(species_binom))
    ) %>%
    ungroup()
  
  #access list of species associated with freq less than, not equal to n:
  sp_list <- subset(sp_obs, Freq <= m)$species_list %>%
    unlist() %>% #to properly format dif. rows as one vector
    strsplit(", ") %>%
    unlist()
  #this list will be used to prune aus_data
  
  #Remove species with Freq < n from aus_data
  pruned_df <- df %>%
    filter(!species_binom %in% sp_list)
  
  #remove intermediates
  rm(sp_obs, sp_list)
  
  return(pruned_df)
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