# AusStoich Modeling 
# Libraries & functions 
library(here)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

library(bestNormalize)
library(patchwork)

# Initial PCA recipe 
# Data split 
aus_split <- aus_data |> 
  select(!Unique_ID:myc_type) |> 
  select(!leaf_P_per_dry_mass:CP_ratio) |> 
  initial_validation_split(prop = c(0.8, 0.1), strata = leaf_N_per_dry_mass)
aus_train <- training(aus_split)
aus_validate <- validation(aus_split)

aus_rec <- recipe(leaf_N_per_dry_mass ~ ., data = aus_train) |> 
  step_zv(all_numeric_predictors()) |> 
  step_orderNorm(all_numeric_predictors()) |> 
  step_normalize(all_numeric_predictors()) |> 
  prep()

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

aus_pca <- aus_rec |> 
  step_pca(all_numeric_predictors(), num_comp = 4) |> 
  prep() 

# Access loadings here ? 
# 
# 

# Then bake and plot components 
aus_pca |> bake(new_data = aus_validate) |> ggplot(aes(x = .panel_x, y = .panel_y)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_autodensity(alpha = 0.3) +
  facet_matrix(vars(everything()), layer.diag = 2)
