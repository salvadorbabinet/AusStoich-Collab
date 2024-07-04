# AusStoich EDA 
# Libraries & functions
library(here)
library(tidyverse)
library(ape)

# Takes tibble tb, categorical variable x; returns factorized count table for levels of x
count_table <- function(tb, x) { 
  tb |> 
    count({{x}}, sort = T) |>
    filter({{x}} != 'NA') |> 
    mutate(name = fct({{x}})) |> 
    select(n:name) |> relocate(name)
}

# Takes tibble tb, continuous variable x, (optional) grouping; returns summary stats 
summarize_cont <- function(tb, x, grouping = NULL) {
  tb |> summarize(
    min = min({{x}}, na.rm = T),
    median = median({{x}}, na.rm = T),
    mean = mean({{x}}, na.rm = T),
    var = var({{x}}, na.rm = T), 
    sd = sd({{x}}, na.rm = T), 
    max = max({{x}}, na.rm = T), 
    n = n(), 
    is_NA = sum(is.na({{x}})),
    .by = {{grouping}}
  )
}

# Data import & tidying ---------------------------------------------------
# Initial import 
raw_data <- read_csv(here('Inputs','austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv')) 
raw_data #For reference

tidy_data <- read_csv(
  file = here('Inputs','austraits_leaf_stoichiometry_MASTER_v1.0_10-05-2024.csv'),
  na = c('', 'NA', 'uncertain'),
  col_types = cols(
    woodiness = col_factor(c('herbaceous', 'woody')),
    reclass_life_history = col_factor(c('short', 'long')),
    putative_BNF = col_factor(c('0', '1')),
    myc_type = col_factor(c('AM', 'EcM', 'EcM-AM', 'ErM', 'NM', 'NM-AM'))
    )
  )

tidy_data <- tidy_data |> 
  select(Unique_ID:LATLONG) |> 
  filter(!is.na(Unique_ID)) |> 
  relocate(species_binom, .after = genus) |> 
  relocate(c(lat_deg, long_deg), .after = dataset_id)

tidy_data 

# Merging environmental data 
env_data <- read_csv(here('Inputs', 'Aus-Stoich_gridded_env_data.csv')) |>
  rename(lat_deg = lat, long_deg = lon) 

seasonality_data <- read_csv(here('Inputs', 'AusStoich_Seasonality_WorldClim30s.csv')) |> 
  rename(lat_deg = `latitude (deg)`, long_deg = `longitude (deg)`)

joined_env <- full_join(env_data, seasonality_data)

joined_data <- left_join(tidy_data, joined_env)

write_csv(joined_data, 'AusStoich_Combined_Dataset_1.0.csv')


# Phylogeny (this is not yet implemented properly)
tree <- read.tree(here('Inputs','ITS_tree.tre'))
tree_species <- 
  tibble(tree[["treeTREE1="]][["tip.label"]]) |> 
  rename(species_in_tree = 'tree[["treeTREE1="]][["tip.label"]]')
  
  
# Structure  ---------------------------------------------------------
# Observation frequencies across taxa 
species <- count_table(tidy_data, species_binom) 
species

genus <- count_table(tidy_data, genus)
genus

family <- count_table(tidy_data, family)
family 

species |> #All species
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_blank()) + 
  labs(
    title = 'Species observation frequency in AusTraits',
    x = 'Species', y = 'Frequency'
  )

species |> #Only species above a given frequency threshold 
  filter(n >= 30) |> 
  ggplot(aes(x = name, y = n)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90)) + 
  labs(
    title = 'Species observation frequency in AusTraits (n > 30)',
    x = 'Species', y = 'Frequency'
    )

species |> #Observation density for all species 
  ggplot(aes(x = n)) +
  geom_density() +
  labs(
    title = 'Density of species observations in Austraits',
    x = 'Number of observations', y = 'Density'
  )

species |> #Observation density within a given frequency range 
  filter(n > 10 & n < 100) |> 
  ggplot(aes(x = n)) + geom_density() +
  labs(
    title = 'Density of species observations in Austraits (100 > n > 10)',
    x = 'Number of observations', y = 'Density'
  )

# Spatial distribution 
# All species
australia <- map_data('world', region = 'australia') |> filter(long < 155)
ggplot() +
  geom_polygon(
    data = australia, 
    aes(x = long, y = lat, group = group), 
    fill = 'white', color = 'black'
    ) + 
  coord_quickmap() +
  geom_jitter( #Bin by species frequency? 
    data = tidy_data, 
    aes(x = long_deg, y = lat_deg),
    alpha = 0.1
    ) +
  labs(
    title = 'Spatial distribution of observations in AusTraits',
    x = 'Longitude', y = 'Latitude'
  )

# Other factors 
tidy_data |> count(woodiness)
tidy_data |> count(across(woodiness:putative_BNF)) 


# Variation ---------------------------------------------------------------
# Foliar carbon 
tidy_data |> ggplot(aes(x = leaf_C_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar C concentration across all samples',
    x = 'Foliar C per dry mass', y = 'Frequency'
  )

summarize_cont(tidy_data, leaf_C_per_dry_mass)

# Foliar nitrogen 
tidy_data |> ggplot(aes(x = leaf_N_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar N concentration across all samples',
    x = 'Foliar N per dry mass', y = 'Frequency'
  )

summarize_cont(tidy_data, leaf_N_per_dry_mass)

# outliers_N <- tidy_data |> 
#   filter(leaf_N_per_dry_mass > 50) |> 
#   arrange(desc(leaf_N_per_dry_mass)) 

# Foliar phosphorus 
tidy_data |> ggplot(aes(x = leaf_P_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar P concentration across all samples',
    x = 'Foliar P per dry mass', y = 'Frequency'
  )

summarize_cont(tidy_data, leaf_P_per_dry_mass)

# Stoichiometry distributions 
tidy_data |> ggplot(aes(x = CN_ratio)) +
  geom_histogram(bins = 60)

tidy_data |> ggplot(aes(x = NP_ratio)) +
  geom_histogram(bins = 60) +
  labs(
    title = 'Foliar N:P across all samples',
    x = 'N:P', y = 'Frequency'
  )

tidy_data |> ggplot(aes(x = CP_ratio)) +
  geom_histogram(bins = 60)


# Co-variation ------------------------------------------------------------
# Example grouped stats 
tidy_data |> summarize_cont(leaf_C_per_dry_mass, grouping = woodiness) 

# Example density curve 
tidy_data |> ggplot(aes(x = CN_ratio, y = after_stat(density))) +
  geom_freqpoly(aes(linetype = woodiness)) +
  labs(
    title = 'C:N density curve across woodiness levels',
    x = 'C:N Ratio', y = 'Density'
  )

#Example box plot 
tidy_data |> filter(!is.na(woodiness)) |> 
  ggplot(aes(x = woodiness, y = CN_ratio)) +
  geom_boxplot() +
  labs(
    title = 'C:N ratio across woodiness levels',
    x = 'Woodiness', y = 'C:N Ratio'
  )

# Example scatter plot 
tidy_data |> ggplot(aes(x = CN_ratio, y = CP_ratio)) +
  geom_point(alpha = 0.4) +
  labs(
    title = 'Relationship between C:N & C:P across all samples',
    x = 'C:N Ratio', y = 'C:P Ratio'
  )