# AusStoich Modeling 
# Libraries & functions 
library(here)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

library(corrplot)
library(bestNormalize)
library(patchwork)
library(ggforce)

# Focusing on leaf N outcome to start 
# Correlation plot 
aus_data |> select(!Unique_ID:myc_type) |> 
  select(!leaf_P_per_dry_mass:CP_ratio) |> 
  cor(use = 'pairwise.complete.obs') |> 
  corrplot(method = 'ellipse', tl.col = 'black')

# Data split 
aus_split <- aus_data |> 
  select(!Unique_ID:myc_type) |> 
  select(!leaf_P_per_dry_mass:CP_ratio) |> 
  initial_validation_split(prop = c(0.8, 0.1), strata = leaf_N_per_dry_mass)
aus_train <- training(aus_split)
aus_validate <- validation(aus_split)

# Initial recipe with PCA pre-requirements 
aus_rec <- recipe(leaf_N_per_dry_mass ~ ., data = aus_train) |> 
  step_zv(all_numeric_predictors()) |> 
  step_orderNorm(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()

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

# Variance coverage and loadings 
pca_step <- aus_pca$steps[[4]]

pca_variance <- pca_step |> tidy(type = 'variance') 
pca_variance |> filter(terms == 'percent variance') 
pca_variance |> filter(terms == 'cumulative percent variance') 

pca_loadings <- pca_step |> # Subset PCA step 
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

