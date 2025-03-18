library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

#--------------------transparent australia map---------------------------------- 
australia_map <- map_data("world", region = "Australia")
ggplot() +
  geom_polygon(data = australia_map, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", alpha = 0.5) +  # Transparent inside
  geom_point(data = aus_data, aes(x = long_deg, y = lat_deg), size = 4) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave("Outputs/Visualizations/Poster/p1.png", bg = "transparent",
        dpi = 800)



#--------------------------Leaf Nutrient Dotplot--------------------------------
pruned <- prune_ausdata(aus_data, 62)
complete <- aus_data %>% mutate(species_binom = "All Species")
combined <- bind_rows(pruned, complete)

#capitalize
capitalize_species <- function(name) {
  name <- gsub("_", " ", name)  # Replace underscores with spaces
  name <- paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  return(name)
}


# Apply the capitalization function to species names in pruned dataframe
pruned <- pruned %>%
  mutate(species_binom = capitalize_species(species_binom))

# Correctly set the factor levels with "All Species" first
combined <- bind_rows(complete, pruned)
combined$species_binom <- factor(combined$species_binom,
                                 levels = c("All Species", unique(pruned$species_binom)))
#Explicitly define factor levels with "All Species" first


# Plot with adjustments
ggplot(combined, aes(x = species_binom, y = leaf_N_per_dry_mass)) +
  geom_jitter(width = 0.2, height = 0, size = 1.5, alpha = 0.6, color = "black") +
  theme_classic() +
  labs(title = "Leaf N for Top 7 Species",
       y = "Leaf N Concentration",
       x = NULL) +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1, size = 25, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 25, color = "black", margin = margin(r = 20)),
    plot.title = element_text(size = 30, color = "black")
    )
ggsave("Outputs/Visualizations/Poster/p2.png", dpi = 800)

ggplot(combined, aes(x = species_binom, y = leaf_N_per_dry_mass, color = species_binom)) +
  geom_jitter(width = 0.2, height = 0, size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("All Species" = "black", setNames(palette(), unique(pruned$species_binom)))) +
  theme_classic() +
  labs(title = "Leaf N for Top 7 Species",
       y = "Leaf N Concentration",
       x = NULL) +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1, size = 25, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 25, color = "black", margin = margin(r = 20)),
    plot.title = element_text(size = 30, color = "black")
  )



#--------------------------Sp. Level lin.reg------------------------------------

pruned <- prune_ausdata(aus_data, 62)

capitalize_species <- function(name) {
  name <- gsub("_", " ", name)
  name <- paste0(toupper(substring(name, 1, 1)), substring(name, 2))
  return(name)
}

# Apply capitalization to species_binom in pruned dataframe
pruned <- pruned %>%
  mutate(species_binom = capitalize_species(species_binom))

# Plot with updated legend
ggplot(pruned, aes(x = SN_total_0_30, y = leaf_N_per_dry_mass, color = species_binom)) +
  geom_point(alpha = 0.6, size = 4) +
  # geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(
    title = "Leaf N vs. Soil N: Top 7 Species",
    y = "Leaf N Concentration",
    x = "Total Soil Nitrogen (%)",
    color = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 50, hjust = 1, size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 25, color = "black", margin = margin(r = 20)),
    axis.title.x = element_text(size = 20, color = "black"), 
    legend.text = element_text(size = 16),       
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 30, color = "black")
  )

ggsave("Outputs/Visualizations/Poster/p3.png", dpi = 800)


#--- colors
# Generate a consistent color palette for species (excluding "All Species")
# Ensure species_binom is a factor with "All Species" first, then alphabetical order
species_order <- c("All Species", sort(unique(pruned$species_binom)))
combined$species_binom <- factor(combined$species_binom, levels = species_order)
pruned$species_binom <- factor(pruned$species_binom, levels = species_order)

# First Plot: Leaf N for Top 7 Species
ggplot(combined, aes(x = species_binom, y = leaf_N_per_dry_mass, color = species_binom)) +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.6) +
  scale_color_manual(values = c("black", scales::hue_pal()(length(species_order) - 1))) +  
  theme_classic() +
  labs(title = "Leaf N for 7 Most Abundant Species",
       y = "Leaf N Concentration (mg/kg)",
       x = NULL) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 50, hjust = 1, size = 25, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 25, color = "black", margin = margin(r = 20)),
    axis.title.x = element_text(size = 20, color = "black"), 
    plot.title = element_text(size = 30, color = "black")
  )
ggsave("Outputs/Visualizations/Poster/p2.png", dpi = 800)

# Second Plot: Leaf N vs Soil N
ggplot(pruned, aes(x = SN_total_0_30, y = leaf_N_per_dry_mass, color = species_binom)) +
  geom_point(alpha = 0.6, size = 4) +
  scale_color_discrete() +  
  theme_classic() +
  labs(
    title = "Leaf N vs. Soil N: 7 Most Abundant Species",
   # y = "Leaf N Concentration (mg/kg)",
    x = "Total Soil Nitrogen (%)",
    #color = "Species"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 50, hjust = 1, color = "black", size = 20),
    axis.text.y = element_text(size = 20, color = "black"),
    #axis.title.y = element_text(size = 20, color = "black", margin = margin(r = 20)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 25, color = "black"), 
    #legend.text = element_text(size = 16),       
    #legend.title = element_text(size = 18),
    plot.title = element_text(size = 30, color = "black")
  )
ggsave("Outputs/Visualizations/Poster/p3.png", dpi = 800)    

ggplot(pruned, aes(x = SN_total_0_30, y = leaf_N_per_dry_mass, color = species_binom)) +
  geom_point(data = combined, aes(x = SN_total_0_30, y = leaf_N_per_dry_mass, color = species_binom), 
             alpha = 0.4, size = 3) +  # Black dots for "All Species"
  geom_point(alpha = 0.2, size = 4) +
  scale_color_manual(values = c("black", scales::hue_pal()(length(species_order) - 1))) +  
  theme_classic() +
  labs(
    title = "Leaf N vs. Soil N: 7 Most Abundant Species",
    x = "Total Soil Nitrogen (%)"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 50, hjust = 1, color = "black", size = 20),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 25, color = "black"), 
    plot.title = element_text(size = 30, color = "black")
  )
ggsave("Outputs/Visualizations/Poster/p4.png", dpi = 800)

#---


#--------------------------Missing Data densities--------------------------------
na_data <- aus_data %>%
  mutate(across(c(leaf_P_per_dry_mass,leaf_C_per_dry_mass,
                  NP_ratio, CN_ratio, CP_ratio), as.character)) %>% 
  replace_na(list(leaf_N_per_dry_mass= "NA", leaf_P_per_dry_mass = "NA",
                  leaf_C_per_dry_mass = "NA",NP_ratio = "NA",
                  CN_ratio = "NA", CP_ratio = "NA"))

na_data <- na_data %>%
  filter(leaf_P_per_dry_mass == "NA" | leaf_C_per_dry_mass == "NA" |
           NP_ratio == "NA" | CN_ratio == "NA" | CP_ratio == "NA")

#specify colors
color_palette <- c("NA rows" = "lightgray", "All data" = "lightgreen")

#density plot of missing leaf concentration data
#using (complete) leaf N to compare rows with NAs for P and C
ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = leaf_N_per_dry_mass,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Leaf N observations with Missing Leaf P and/or Leaf C",
       x = "Leaf Nitrogen Concentration", y = "Density") +
  theme_classic() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))

#missing env. coverage
ggplot() +
  geom_density(data = aus_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "All data", alpha = 0.5)) +
  geom_density(data = na_data,
               mapping = aes(x = SN_total_0_30,
                             fill = "NA rows", alpha = 0.5)) +
  labs(title = "Observations with Missing Leaf P and/or Leaf C",
       x = "Total Soil Nitrogen",y = "Density") +
  theme_classic() +
  scale_fill_manual(values = color_palette, name = " ") +
  theme(legend.position = c(0.83, 0.86))


#--------------------------tree--------------------------------
library(ggtree)
library(tidytree)
library(treeio)
library(cowplot)
library(ggpubr)

ausdata_all_pos_sp_tree <- read.tree("Inputs/Trees/ausdata_all_pos_sp.tre")
austraits_all_pos_sp_df <- read_csv('Inputs/all_pos_austraits_LCVP_sp.csv')

all_pos_sp_data <- aus_data[aus_data$species_binom %in%
                              austraits_all_pos_sp_df$species, ]

all_pos_sp_data <- add_CV_columns(select_relevant_columns(all_pos_sp_data))

avg_all_pos_sp_data <- average_nutrient_data(all_pos_sp_data)

#horizontal base
all_pos_sp_plot <- ggtree(ausdata_all_pos_sp_tree) + geom_tiplab(size = 0.5)

#most basic, no coloring, horizontal bar plot
all_pos_sp_plot + geom_facet(
  panel = 'Trait',
  data = avg_all_pos_sp_data,
  geom = geom_col,
  mapping = aes(x = avg_leaf_N),
  orientation = "y") +
  ggtitle("") +
  theme(plot.title = element_text(size = 20))

names(avg_all_pos_sp_data)[1] <- "label"
attemptree <- full_join(as.treedata(ausdata_all_pos_sp_tree), avg_all_pos_sp_data, by = "label")

#mess with line thickness, and very small tip labels to manually write over later
#get label in the middle
ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular") +
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00")

ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular", size = 1) + # Increase line thickness with size
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  theme(
    legend.position = c(0.4, 0.5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )

ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular", size = 1) + 
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  labs(color = "Average Leaf N") +  
  theme(
    legend.position = c(0.4, 0.5),
    legend.title = element_text(size = 10, margin = margin(b = 20)),  # Adds space below title
    legend.text = element_text(size = 10)
  )

ggtree(attemptree, aes(color = avg_leaf_N), layout = "circular", size = 1) + 
  scale_color_continuous(low = "#6ad1f3", high = "#ee6b00") +
  labs(color = "Average Leaf N") +  
  theme(
    legend.position = "none"  # This removes the legend
  ) + geom_tiplab(size = 0.5)

ggpubr::as_ggplot(get_legend(p))

ggsave("Outputs/Visualizations/Poster/tree.png", dpi = 800)
80

