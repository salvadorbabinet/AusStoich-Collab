# AusStoich Modeling 
# Libraries & functions 
library(here)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

library(corrplot)
library(patchwork)
library(ggforce)
library(bestNormalize)


# Starting with leaf N outcome 
# Correlation plot 
aus_data |> select(!Unique_ID:myc_type) |> 
  select(!leaf_P_per_dry_mass:CP_ratio) |> 
  cor(use = 'pairwise.complete.obs') |> 
  corrplot(method = 'ellipse', tl.col = 'black')


# Data split 
aus_split <- aus_data |> 
  select(!c(Unique_ID, leaf_P_per_dry_mass:CP_ratio)) |> 
  initial_split(prop = 0.8, strata = leaf_N_per_dry_mass)
aus_train <- training(aus_split)
aus_test <- testing(aus_split)

n1 <- ggplot(aus_data, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('All data')
n2 <- ggplot(aus_train, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('Training data')
n3 <- ggplot(aus_test, aes(x = leaf_N_per_dry_mass)) + 
  geom_histogram() + 
  ggtitle('Testing data')

# Check leaf N stratification 
n1 + n2 + n3 
rm(n1, n2, n3)


# Initial recipe with PCA pre-requirements 
aus_rec <- recipe(leaf_N_per_dry_mass ~ ., data = aus_train) |> 
  update_role(dataset_id:myc_type, new_role = 'Factor') |> 
  step_zv(all_numeric_predictors()) |> 
  step_orderNorm(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()
aus_rec

# Check scaling and normalizing 
aus_rec_processed <- bake(aus_rec, new_data = aus_validate)
p1 <- aus_validate |> 
  ggplot(aes(x = temp_seasonality)) + 
  geom_histogram(bins = 50, color = 'white') + 
  ggtitle('Original validation set') + 
  theme_bw()

p2 <- aus_rec_processed |> 
  ggplot(aes(x = temp_seasonality)) +
  geom_histogram(bins = 50, color = 'white') +
  ggtitle('Processed validation set') +
  theme_bw()

p1 + p2

# Add PCA step 
aus_pca <- aus_rec |> 
  step_pca(all_numeric_predictors(), num_comp = 4) |> 
  prep() 
aus_pca

# Variance coverage and loadings 
pca_extract <- aus_pca$steps[[4]]
glimpse(pca_extract)

pca_variance <- pca_extract |> tidy(type = 'variance') 
pca_variance |> 
  filter(terms %in% c('percent variance', 'cumulative percent variance')) |> 
  pivot_wider(names_from = terms, values_from = value) |> 
  select(!id)

pca_loadings <- pca_extract |> # Subset PCA step 
  tidy(type = 'coef') |> 
  mutate(component = parse_number(component))
pca_loadings |> 
  group_by(component) |> 
  slice_max(abs(value), n = 3) |> 
  print(n = 15)

# Loadings plot 
pca_loadings |> filter(component == 1) |> 
  # slice_max(abs(value), n = 10) |> 
  ggplot(aes(y = terms, x = value)) +
  geom_col() + 
  labs(
    title = 'PC1 Loadings Plot', 
    x = 'Contribution', 
    y = 'Term')

pca_loadings |> filter(component < 5) |> 
ggplot(aes(x = value, y = terms)) + 
  geom_col() + 
  facet_wrap(~component)

# Then bake and plot components 
aus_pca_processed <- aus_pca |> bake(new_data = aus_validate)

# Plot w/o additional visualization 
aus_pca_processed |> ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2)

# Bin and visualize outcome ? 
aus_pca_processed |> bake(new_data = aus_validate) |> 
  ggplot(aes(x = .panel_x, y = .panel_y, 
      color = cut_number(leaf_N_per_dry_mass, 4), # Cut by interval or number ? 
      fill = cut_number(leaf_N_per_dry_mass, 4)
      )) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2) + 
  labs(color = 'Foliar N Range', fill = 'Foliar N Range')

# Major families ? 
aus_pca_processed |> bake(new_data = aus_validate) |> 
  ggplot(aes(x = .panel_x, y = .panel_y, 
             color = ###, 
             fill = ### 
  )) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2) + 
  labs(color = 'Foliar N Range', fill = 'Foliar N Range')

