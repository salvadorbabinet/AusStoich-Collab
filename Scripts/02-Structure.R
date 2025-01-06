# AusStoich Data Structure

# Investiating structural properties of the data, but not yet
# relationships between variables. Observation counts, etc.

# Libraries & functions ----
library(here)
library(tidyverse)
library(httpgd)

hgd()
hgd_browse()

# Takes tibble tb, categorical variable x
# Returns factorized count table for levels of x
count_table <- function(tb, x) { 
  tb |> 
    count({{x}}, sort = T) |>
    filter({{x}} != 'NA') |> 
    mutate(name = fct({{x}})) |> 
    select(n:name) |> relocate(name)
}

# Takes tibble tb, continuous variable x, (optional) grouping
# Returns summary stats
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


# Observation frequencies across taxa ----
species <- count_table(aus_data, species_binom)
species

genus <- count_table(aus_data, genus)
genus

family <- count_table(aus_data, family)
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


# Spatial distribution ----
# All species
aus_NPC <- aus_NPC |> rename(Family = family)
aus_subset <- filter(aus_NPC, Family %in% c("Myrtaceae", "Fabaceae", "Proteaceae"))
australia <- map_data('world', region = 'australia') |> filter(long < 155)

ggplot() +
  geom_polygon(
    data = australia, 
    aes(x = long, y = lat, group = group), 
    fill = 'white', color = 'black'
  ) + 
  coord_quickmap() +
  geom_jitter(
    data = anti_join(aus_data, aus_subset),
    aes(x = long_deg, y = lat_deg),
    alpha = 0.1, size = 1, color = "#bebebec4"
  ) +
  geom_jitter(
    data = filter(aus_NPC, !(Family %in% c("Myrtaceae", "Fabaceae", "Proteaceae"))),
    aes(x = long_deg, y = lat_deg),
    alpha = 0.2, size = 2.5,
    width = 0.4,
    height = 0.4
  ) +
  geom_jitter( 
    data = aus_subset,
    aes(x = long_deg, y = lat_deg, color = Family, shape = Family),
    alpha = 0.7, size = 2.5
  ) +
  labs(
    title = 'Spatial distribution of observations in AusTraits',
    x = 'Longitude', y = 'Latitude'
  )

# Other factors 
tidy_data |> count(woodiness)
tidy_data |> count(across(woodiness:putative_BNF)) 


# Some distributions of variable measures ----
# Foliar carbon
aus_data |> ggplot(aes(x = leaf_C_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar C concentration across all samples',
    x = 'Foliar C per dry mass', y = 'Frequency'
  )

summarize_cont(aus_data, leaf_C_per_dry_mass)

# Foliar nitrogen
aus_data |> ggplot(aes(x = leaf_N_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar N concentration across all samples',
    x = 'Foliar N per dry mass', y = 'Frequency'
  )

summarize_cont(aus_data, leaf_N_per_dry_mass)

# Foliar phosphorus
aus_data |> ggplot(aes(x = leaf_P_per_dry_mass)) +
  geom_histogram(bins = 80) +
  labs(
    title = 'Foliar P concentration across all samples',
    x = 'Foliar P per dry mass', y = 'Frequency'
  )

summarize_cont(aus_data, leaf_P_per_dry_mass)

# Can also group a continuous variable by some categorical variable
# Density curves can complement histograms nicely, especially if grouping
aus_data |> summarize_cont(leaf_C_per_dry_mass, grouping = woodiness)

aus_data |> ggplot(aes(x = CN_ratio, y = after_stat(density))) +
  geom_freqpoly(aes(linetype = woodiness)) +
  labs(
    title = 'C:N density curve across woodiness levels',
    x = 'C:N Ratio', y = 'Density'
  )
